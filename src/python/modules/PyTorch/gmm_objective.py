# Copyright (c) Microsoft Corporation.
# Licensed under the MIT license.
# cSpell:disable
# python3 runner/main.py GMM modules/PyTorch/PyTorchGMM.py ../../data/gmm/test.txt . 0.5 1 1 1000

import math
from scipy import special as scipy_special
import torch


def logsumexp(x):
    mx = torch.max(x)
    emx = torch.exp(x - mx)
    return torch.log(sum(emx)) + mx


def logsumexpvec(x):
    '''The same as "logsumexp" but calculates result for each row separately.'''

    mx = torch.max(x, 1).values
    lset = torch.logsumexp(torch.t(x) - mx, 0)
    return torch.t(lset + mx)


def log_gamma_distrib(a, p):
    return scipy_special.multigammaln(a, p)


def sqsum(x):
    return sum(x ** 2)


def log_wishart_prior(p, wishart_gamma, wishart_m, sum_qs, Qdiags, icf):
    n = p + wishart_m + 1
    k = icf.shape[0]

    out = torch.sum(
        0.5 * wishart_gamma * wishart_gamma *
        (torch.sum(Qdiags ** 2, dim = 1) + torch.sum(icf[:,p:] ** 2, dim = 1)) -
        wishart_m * sum_qs
    )

    C = n * p * (math.log(wishart_gamma / math.sqrt(2)))
    return out - k * (C - log_gamma_distrib(0.5 * n, p))


def constructL(d, icf):
    constructL.Lparamidx = d

    print("icf")
    print(icf)
    def make_L_col(i):
        print("make_L_col", i)
        nelems = d - i - 1
        slice_sz = (icf[constructL.Lparamidx:(constructL.Lparamidx + nelems)]).shape
        print("slize_sz")
        print(slice_sz)
        col = torch.cat([
            torch.zeros(i + 1, dtype = torch.float64),
            icf[constructL.Lparamidx:(constructL.Lparamidx + nelems)]
        ])
        print("col")
        print(col)
        constructL.Lparamidx += nelems
        return col

    columns = [make_L_col(i) for i in range(d)]
    return torch.stack(columns, -1)


def Qtimesx(Qdiag, L, x):
    print("FINDME\n")
    print("L.shape\n")
    print(L.shape)
    print("\nx.shape")
    print(x.shape)
    f = torch.einsum('ijk,mik->mij', L, x)
    return Qdiag * x + f


def gmm_objective(alphas, means, icf, x, wishart_gamma, wishart_m):
    n = x.shape[0]
    d = x.shape[1]

    print("QDiags")
    Qdiags = torch.exp(icf[:, :d])
    print(Qdiags)
    print("sum_qs")
    sum_qs = torch.sum(icf[:, :d], 1)
    print(sum_qs)
    print("icf")
    print(icf)
    print("Ls")
    Ls = torch.stack([constructL(d, curr_icf) for curr_icf in icf])
    print(Ls)

    print("x")
    print(x)
    print("means")
    print(means)
    print("xcentered")
    xcentered = torch.stack(tuple( x[i] - means for i in range(n) ))
    print(xcentered)
    print("Lxcentered")
    Lxcentered = Qtimesx(Qdiags, Ls, xcentered)
    print(Lxcentered)
    print("sqsum_Lxcentered")
    sqsum_Lxcentered = torch.sum(Lxcentered ** 2, 2)
    print(sqsum_Lxcentered)
    print("inner_term")
    inner_term = alphas + sum_qs - 0.5 * sqsum_Lxcentered
    print(inner_term)
    print("lse")
    lse = logsumexpvec(inner_term)
    print(lse)
    print("slse")
    slse = torch.sum(lse)
    print(slse)

    print("wish")
    print(log_wishart_prior(d, wishart_gamma, wishart_m, sum_qs, Qdiags, icf))
    CONSTANT = -n * d * 0.5 * math.log(2 * math.pi)
    res = CONSTANT + slse - n * logsumexp(alphas) \
        + log_wishart_prior(d, wishart_gamma, wishart_m, sum_qs, Qdiags, icf)
    print("RESULT")
    print(res)
    assert(False)
