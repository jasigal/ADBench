﻿// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

using System;
using System.Collections.Generic;

namespace DotnetModulesTests
{
    class TolerantDoubleComparer : IEqualityComparer<double>
    {
        readonly double tolerance;

        public TolerantDoubleComparer(double tolerance)
        {
            this.tolerance = tolerance;
        }

        public static TolerantDoubleComparer FromTolerance(double tolerance) => new TolerantDoubleComparer(tolerance);

        public bool Equals(double x, double y) => Math.Abs(x - y) < tolerance;

        public int GetHashCode(double obj)
        {
            throw new NotImplementedException();
        }
    }
}
