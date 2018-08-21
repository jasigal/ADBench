#include <iostream>
#include <string>
#include <chrono>
#include <set>
#include <limits>
#include <stdexcept>

#include "../cpp-common/utils.h"
#include "../cpp-common/defs.h"

#if defined DO_GMM
#include "../cpp-common/gmm.h"
#elif defined DO_BA
#include "../cpp-common/ba.h"
#elif defined DO_HAND
#if defined DO_EIGEN
#include "../cpp-common/hand_eigen.h"
#else
#include "../cpp-common/hand_light_matrix.h"
#endif
#endif

using std::cout;
using std::endl;
using std::string;
using namespace std::chrono;

const double DELTA = 1e-5;

// VECTOR UTILS

// Subtract B from A
template<typename T>
T* sub_vec(T* a, T* b, int sz) {
	for (int i = 0; i < sz; i++) a[i] -= b[i];
	return a;
}

// Divide vector A by scalar B
template<typename T>
T* div_vec(T* a, int a_sz, T b) {
	for (int i = 0; i < a_sz; i++) a[i] /= b;
	return a;
}

// Insert B starting at a point in A
template<typename T>
T* vec_ins(T* a, T* b, int b_sz) {
	for (int i = 0; i < b_sz; i++) a[i] = b[i];
	return a;
}

// FINITE DIFFERENTIATION FUNCTION

/// <summary>Approximately differentiate a function using finite differences</summary>
/// <param name="func">Function to be differentiated.
///		Should accept 2 pointers as arguments (one input, one output).
///		Each output will be differentiated with respect to each input.</param>
/// <param name="input">Pointer to input data (scalar or vector)</param>
/// <param name="input_size">Input data size (1 for scalar)</param>
/// <param name="output">Pointer to where 'func' should output data (scalar or vector)</param>
/// <param name="output_size">Size of 'func' output data</param>
/// <param name="result">Pointer to where resultant Jacobian should go.
///		Will be stored as a vector(input_size * output_size).
///		Will store in format foreach (input) { foreach (output) {} }</param>
/// <param name="delta">The difference to use in finite differentiation</param>
template<typename T>
void finite_differences(std::function<void(T*, T*)> func,
	T *input, int input_size, T* output, int output_size,
	T *result, T delta = DELTA)
{
	func(input, output);
	vector<T> tmp_output(output_size);
	for (int i = 0; i < input_size; i++)
	{
		input[i] += delta;
		func(input, tmp_output.data());
		div_vec(sub_vec(tmp_output.data(), output, output_size), output_size, delta);
		vec_ins(&result[output_size * i], tmp_output.data(), output_size);
		input[i] -= delta;
	}
}

// OBJECTIVE APPLICATION

#if defined DO_GMM

template<typename T>
T gmm_objective_wrapper(int d, int k, int n,
	const T* const alphas,
	const T* const means,
	const T* const icf,
	const double* const x,
	Wishart wishart) {
	T err;
	gmm_objective(d, k, n, alphas, means, icf, x, wishart, &err);
	return err;
}

void compute_gmm_J(int d, int k, int n,
	vector<double> alphas,
	vector<double> means,
	vector<double> icf,
	vector<double> x,
	Wishart wishart,
	double* J)
{
	double *alphas_d = &J[0];
	double *means_d = &J[k];
	double *icf_d = &J[k + d * k];

	double err;

	finite_differences<double>([&](double* alphas_in, double* err) {
		gmm_objective(d, k, n, alphas_in, means.data(), icf.data(), x.data(), wishart, err);
	}, alphas.data(), alphas.size(), &err, 1, alphas_d);

	finite_differences<double>([&](double* means_in, double* err) {
		gmm_objective(d, k, n, alphas.data(), means_in, icf.data(), x.data(), wishart, err);
	}, means.data(), means.size(), &err, 1, means_d);

	finite_differences<double>([&](double* icf_in, double* err) {
		gmm_objective(d, k, n, alphas.data(), means.data(), icf_in, x.data(), wishart, err);
	}, icf.data(), icf.size(), &err, 1, icf_d);
}

void test_gmm(const string& fn_in, const string& fn_out,
	int nruns_f, int nruns_J, double time_limit, bool replicate_point)
{
	int d, k, n;
	vector<double> alphas, means, icf, x;
	double err;
	Wishart wishart;

	// Read instance
	read_gmm_instance(fn_in + ".txt", &d, &k, &n,
		alphas, means, icf, x, wishart, replicate_point);

	int icf_sz = d * (d + 1) / 2;
	int Jrows = 1;
	int Jcols = (k*(d + 1)*(d + 2)) / 2;

	vector<double> J(Jcols);

	// Test
	double tf = timer([&]() {
		gmm_objective(d, k, n, alphas.data(), means.data(),
			icf.data(), x.data(), wishart, &err);
	}, nruns_f, time_limit);
	cout << "err: " << err << endl;

	double tJ = timer([&]() {
		compute_gmm_J(d, k, n, alphas, means, icf, x, wishart, J.data());
	}, nruns_J, time_limit);
	cout << "err: " << err << endl;

	string name("Finite");
	write_J(fn_out + "_J_" + name + ".txt", Jrows, Jcols, J.data());
	//write_times(tf, tJ);
	write_times(fn_out + "_times_" + name + ".txt", tf, tJ);
}

#elif defined DO_BA

void compute_ba_J(int n, int m, int p, double *cams, double *X,
	double *w, int *obs, double *feats, double *reproj_err,
	double *w_err, BASparseMat& J)
{
	J = BASparseMat(n, m, p);

	int n_new_cols = BA_NCAMPARAMS + 3 + 1;
	vector<double> reproj_err_d(2 * n_new_cols);

	for (int i = 0; i < p; i++) {
		// TODO use std::fill
		memset(reproj_err_d.data(), 0, 2 * n_new_cols * sizeof(double));

		int camIdx = obs[2 * i + 0];
		int ptIdx = obs[2 * i + 1];

		finite_differences<double>([&](double* cam_in, double* reproj_err) {
			computeReprojError(cam_in, &X[ptIdx * 3], &w[i], &feats[2 * i], reproj_err);
		}, &cams[camIdx * BA_NCAMPARAMS], BA_NCAMPARAMS, &reproj_err[2 * i], 2, reproj_err_d.data());

		finite_differences<double>([&](double* X_in, double* reproj_err) {
			computeReprojError(&cams[camIdx * BA_NCAMPARAMS], X_in, &w[i], &feats[2 * i], reproj_err);
		}, &X[ptIdx * 3], 3, &reproj_err[2 * i], 2, reproj_err_d.data());

		finite_differences<double>([&](double* w_in, double* reproj_err) {
			computeReprojError(&cams[camIdx * BA_NCAMPARAMS], &X[ptIdx * 3], w_in, &feats[2 * i], reproj_err);
		}, &w[i], 1, &reproj_err[2 * i], 2, reproj_err_d.data());

		J.insert_reproj_err_block(i, camIdx, ptIdx, reproj_err_d.data());
	}

	double w_d;

	for (int i = 0; i < p; i++)
	{
		finite_differences<double>([&](double* w_in, double* w_er) {
			computeZachWeightError(w_in, w_er);
		}, &w[i], 1, w_err, 1, &w_d);

		J.insert_w_err_block(i, w_d);
	}
}

void test_ba(const string& fn_in, const string& fn_out,
	int nruns_f, int nruns_J, double time_limit)
{
	int n, m, p;
	vector<double> cams, X, w, feats;
	vector<int> obs;

	read_ba_instance(fn_in + ".txt", n, m, p,
		cams, X, w, obs, feats);

	vector<double> reproj_err(2 * p);
	vector<double> w_err(p);
	BASparseMat J(n, m, p);

	double tf = timer([&]() {
		ba_objective(n, m, p, cams.data(), X.data(), w.data(),
			obs.data(), feats.data(), reproj_err.data(), w_err.data());
	}, nruns_f, time_limit);

	double tJ = timer([&]() {
		compute_ba_J(n, m, p, cams.data(), X.data(), w.data(), obs.data(),
			feats.data(), reproj_err.data(), w_err.data(), J);
	}, nruns_J, time_limit);

	string name("Finite");

	write_J_sparse(fn_out + "_J_" + name + ".txt", J);
	write_times(fn_out + "_times_" + name + ".txt", tf, tJ);
}

#elif defined DO_HAND

void test_hand(const string& model_dir, const string& fn_in, const string& fn_out,
	int nruns_f, int nruns_J, double time_limit)
{
	vector<double> theta;
#ifdef DO_EIGEN
	HandDataEigen data;
#else
	HandDataLightMatrix data;
#endif

	read_hand_instance(model_dir, fn_in + ".txt", &theta, &data);

	vector<double> err(3 * data.correspondences.size());
	vector<double> J(err.size() * theta.size());

	double tf = timer([&]() {
		hand_objective(&theta[0], data, &err[0]);
	}, nruns_f, time_limit);

	double tJ = timer([&]() {
		finite_differences<double>([&](double *theta_in, double *err) {
			hand_objective(&theta_in[0], data, &err[0]);
		}, &theta[0], theta.size(), &err[0], err.size(), &J[0]);
	}, nruns_J, time_limit);

#ifdef DO_EIGEN
	string name = "Finite_eigen";
#else
	string name = "Finite_light";
#endif

	write_J(fn_out + "_J_" + name + ".txt", (int)err.size(), (int)theta.size(), &J[0]);
	//write_times(tf, tJ);
	write_times(fn_out + "_times_" + name + ".txt", tf, tJ);
}

#endif

int main(int argc, char *argv[])
{
	string dir_in(argv[1]);
	string dir_out(argv[2]);
	string fn(argv[3]);
	int nruns_f = std::stoi(string(argv[4]));
	int nruns_J = std::stoi(string(argv[5]));
	double time_limit;
	if (argc >= 7) time_limit = std::stod(string(argv[6]));
	else time_limit = std::numeric_limits<double>::infinity();

	// read only 1 point and replicate it?
	bool replicate_point = (argc >= 8 && string(argv[7]).compare("-rep") == 0);

#if defined DO_GMM
	test_gmm(dir_in + fn, dir_out + fn, nruns_f, nruns_J, time_limit, replicate_point);
#elif defined DO_BA
	test_ba(dir_in + fn, dir_out + fn, nruns_f, nruns_J, time_limit);
#elif defined DO_HAND
	test_hand(dir_in + "model/", dir_in + fn, dir_out + fn, nruns_f, nruns_J, time_limit);
#endif
}
