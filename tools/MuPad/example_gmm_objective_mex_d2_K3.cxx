// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#line 2 "au_autodiff_generate_template.cpp"
#include <math.h>

#include "au_mex.h"

// Autogenerated by au_ccode
// FROM: f
// au_autodiff_template.cpp - outline code for au_autodiff*
// awf@microsoft.com, Dec 13

void mlx_function(mlx_inputs& ins, mlx_outputs& outs)
{
    mlx_array<mlx_double> in(ins[0]);
    mlx_array<mlx_double> data(ins[1]);
    mlx_array<mlx_logical> jacobian(ins[2]);
    bool do_jacobian = jacobian[0];

    mlx_assert(in.cols == data.cols);
    mlx_assert(in.rows == 18);
    mlx_assert(data.rows == 5);

    mwSize out_rows = 1 + (do_jacobian ? 18 : 0);
    mwSize out_cols = 1 * in.cols;
    mlx_make_array<mlx_double> out(out_rows, out_cols);

    double const* in_ptr = in.data;
    double const* data_ptr = data.data;
    double* out_ptr = out.data;

    if (do_jacobian) {
        // const mwSize out_rows = 18 + 1;
        const mwSize out_step = (18 + 1) * 1;
        for(mwSize c_in = 0; c_in < in.cols; ++c_in,
                in_ptr += in.rows,
                data_ptr += data.rows,
                out_ptr += out_step) {
              /* inner loop do_jac=1 */
    double x1 = in_ptr[0];
    double x2 = in_ptr[1];
    double x3 = in_ptr[2];
    double x4 = in_ptr[3];
    double x5 = in_ptr[4];
    double x6 = in_ptr[5];
    double x7 = in_ptr[6];
    double x8 = in_ptr[7];
    double x9 = in_ptr[8];
    double x10 = in_ptr[9];
    double x11 = in_ptr[10];
    double x12 = in_ptr[11];
    double x13 = in_ptr[12];
    double x14 = in_ptr[13];
    double x15 = in_ptr[14];
    double x16 = in_ptr[15];
    double x17 = in_ptr[16];
    double x18 = in_ptr[17];
    double data1 = data_ptr[0];
    double data2 = data_ptr[1];
    double data3 = data_ptr[2];
    double data4 = data_ptr[3];
    double data5 = data_ptr[4];
    double t3 = data1-x4;
    double t14 = exp(x11);
    double t21 = t3*x12;
    double t22 = data2-x5;
    double t23 = t14*t22;
    double t2 = t21+t23;
    double t5 = data1-x6;
    double t15 = exp(x14);
    double t30 = t5*x15;
    double t31 = data2-x7;
    double t32 = t15*t31;
    double t4 = t30+t32;
    double t7 = data1-x8;
    double t16 = exp(x17);
    double t37 = t7*x18;
    double t38 = data2-x9;
    double t39 = t16*t38;
    double t6 = t37+t39;
    double t8 = x10*2.0;
    double t9 = exp(t8);
    double t10 = x13*2.0;
    double t11 = exp(t10);
    double t12 = x16*2.0;
    double t13 = exp(t12);
    double t17 = exp(x1);
    double t18 = exp(x2);
    double t19 = exp(x3);
    double t20 = t17+t18+t19;
    double t24 = t2*t2;
    double t25 = t3*t3;
    double t28 = t24*(1.0/2.0);
    double t29 = t9*t25*(1.0/2.0);
    double t26 = -data3-t28-t29+x1+x10+x11;
    double t27 = exp(t26);
    double t33 = t4*t4;
    double t34 = t5*t5;
    double t46 = t33*(1.0/2.0);
    double t47 = t11*t34*(1.0/2.0);
    double t35 = -data3-t46-t47+x2+x13+x14;
    double t36 = exp(t35);
    double t40 = t6*t6;
    double t41 = t7*t7;
    double t48 = t40*(1.0/2.0);
    double t49 = t13*t41*(1.0/2.0);
    double t42 = -data3-t48-t49+x3+x16+x17;
    double t43 = exp(t42);
    double t44 = t27+t36+t43;
    double t45 = 1.0/t20;
    double t50 = 1.0/t44;
    double t51 = data1*2.0;
    double t52 = exp(x10);
    double t53 = x11*2.0;
    double t54 = exp(t53);
    double t55 = exp(x13);
    double t56 = x14*2.0;
    double t57 = exp(t56);
    double t58 = exp(x16);
    double t59 = x17*2.0;
    double t60 = exp(t59);
  out_ptr[0] = data3-log(t20)+log(t44)-data5*(t14+t52)-data5*(t15+t55)-data5*(t16+t58)+data4*(t9+t54+x12*x12)+data4*(t11+t57+x15*x15)+data4*(t13+t60+x18*x18);
  out_ptr[1] = -t17*t45+t27*t50;
  out_ptr[2] = -t18*t45+t36*t50;
  out_ptr[3] = -t19*t45+t43*t50;
  out_ptr[4] = t27*t50*(t2*x12+t9*(t51-x4*2.0)*(1.0/2.0));
  out_ptr[5] = t2*t14*t27*t50;
  out_ptr[6] = t36*t50*(t4*x15+t11*(t51-x6*2.0)*(1.0/2.0));
  out_ptr[7] = t4*t15*t36*t50;
  out_ptr[8] = t43*t50*(t6*x18+t13*(t51-x8*2.0)*(1.0/2.0));
  out_ptr[9] = t6*t16*t43*t50;
  out_ptr[10] = data4*t9*2.0-data5*t52-t27*t50*(t9*t25-1.0);
  out_ptr[11] = -data5*t14+data4*t54*2.0-t27*t50*(t2*t14*t22-1.0);
  out_ptr[12] = data4*x12*2.0-t2*t3*t27*t50;
  out_ptr[13] = data4*t11*2.0-data5*t55-t36*t50*(t11*t34-1.0);
  out_ptr[14] = -data5*t15+data4*t57*2.0-t36*t50*(t4*t15*t31-1.0);
  out_ptr[15] = data4*x15*2.0-t4*t5*t36*t50;
  out_ptr[16] = data4*t13*2.0-data5*t58-t43*t50*(t13*t41-1.0);
  out_ptr[17] = -data5*t16+data4*t60*2.0-t43*t50*(t6*t16*t38-1.0);
  out_ptr[18] = data4*x18*2.0-t6*t7*t43*t50;

#line 39 "au_autodiff_generate_template.cpp"
        }
    } else {
        const mwSize out_step = 1;
        for(mwSize c_in = 0; c_in < in.cols; ++c_in,
                in_ptr += in.rows,
                data_ptr += data.rows,
                out_ptr += out_step) {
              /* inner loop do_jac=0 */
    double x1 = in_ptr[0];
    double x2 = in_ptr[1];
    double x3 = in_ptr[2];
    double x4 = in_ptr[3];
    double x5 = in_ptr[4];
    double x6 = in_ptr[5];
    double x7 = in_ptr[6];
    double x8 = in_ptr[7];
    double x9 = in_ptr[8];
    double x10 = in_ptr[9];
    double x11 = in_ptr[10];
    double x12 = in_ptr[11];
    double x13 = in_ptr[12];
    double x14 = in_ptr[13];
    double x15 = in_ptr[14];
    double x16 = in_ptr[15];
    double x17 = in_ptr[16];
    double x18 = in_ptr[17];
    double data1 = data_ptr[0];
    double data2 = data_ptr[1];
    double data3 = data_ptr[2];
    double data4 = data_ptr[3];
    double data5 = data_ptr[4];
    double t3 = data1-x4;
    double t14 = exp(x11);
    double t2 = t3*x12+t14*(data2-x5);
    double t5 = data1-x6;
    double t15 = exp(x14);
    double t4 = t5*x15+t15*(data2-x7);
    double t7 = data1-x8;
    double t16 = exp(x17);
    double t6 = t7*x18+t16*(data2-x9);
    double t8 = x10*2.0;
    double t9 = exp(t8);
    double t10 = x13*2.0;
    double t11 = exp(t10);
    double t12 = x16*2.0;
    double t13 = exp(t12);
    out_ptr[0] = data3+log(exp(-data3+x1+x10+x11-(t3*t3)*t9*(1.0/2.0)-(t2*t2)*(1.0/2.0))+exp(-data3+x2+x13+x14-(t5*t5)*t11*(1.0/2.0)-(t4*t4)*(1.0/2.0))+exp(-data3+x3+x16+x17-(t7*t7)*t13*(1.0/2.0)-(t6*t6)*(1.0/2.0)))-log(exp(x1)+exp(x2)+exp(x3))-data5*(t14+exp(x10))-data5*(t15+exp(x13))-data5*(t16+exp(x16))+data4*(t9+exp(x11*2.0)+x12*x12)+data4*(t11+exp(x14*2.0)+x15*x15)+data4*(t13+exp(x17*2.0)+x18*x18);

#line 48 "au_autodiff_generate_template.cpp"
        }
    }
    
    outs[0] = out;
}
