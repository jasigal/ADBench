% function [dpartial y] = dpartial_atan(x)
%
% Compute partial derivative diagonal of y = atan(x).
%
% see also dpartial_exp.
%
% This file is part of the ADiMat runtime environment
%
% Copyright 2012 Johannes Willkomm, Fachgebiet Scientific Computing
%                     TU Darmstadt
function [dpartial y] = dpartial_atan(x)
  dpartial = 1 ./ (1 + x.^2);
  if nargout > 1
    y = atan(x);
  end
  
% $Id: dpartial_atan.m 3246 2012-03-23 14:38:47Z willkomm $
