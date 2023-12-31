﻿// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

using DotnetRunner.Benchmarks;
using DotnetRunner.Data;
using System;

namespace DotnetRunner
{
    class Program
    {
        static int Main(string[] args)
        {
            try
            {
                if (args.Length < 8)
                {
                    Console.Error.WriteLine("usage: DotnetRunner testType modulePath inputFilepath outputDir minimumMeasurableTime nrunsF nrunsJ timeLimit [-rep]");
                    return 1;
                }

                var testType = args[0].ToUpperInvariant();
                var modulePath = args[1];
                var inputFilePath = args[2];
                var outputPrefix = args[3];
                var minimumMeasurableTime = TimeSpan.FromMilliseconds(double.Parse(args[4]));
                var nrunsF = int.Parse(args[5]);
                var nrunsJ = int.Parse(args[6]);
                var timeLimit = TimeSpan.FromMilliseconds(double.Parse(args[7]));

                // read only 1 point and replicate it?
                var replicate_point = (args.Length > 8 && args[8] == "-rep");

                if (testType == "GMM")
                {
                    var benchmark = new GMMBenchmark();
                    benchmark.Run(modulePath, inputFilePath, outputPrefix, minimumMeasurableTime, nrunsF, nrunsJ, timeLimit, new GMMParameters() { ReplicatePoint = replicate_point });
                }
                else if (testType == "BA")
                {
                    var benchmark = new BABenchmark();
                    benchmark.Run(modulePath, inputFilePath, outputPrefix, minimumMeasurableTime, nrunsF, nrunsJ, timeLimit);
                }
                else if (testType == "HAND")
                {
                    var benchmark = new HandBenchmark();
                    benchmark.Run(modulePath, inputFilePath, outputPrefix, minimumMeasurableTime, nrunsF, nrunsJ, timeLimit, new HandParameters() { IsComplicated = false });
                }
                else if (testType == "HAND-COMPLICATED")
                {
                    var benchmark = new HandBenchmark();
                    benchmark.Run(modulePath, inputFilePath, outputPrefix, minimumMeasurableTime, nrunsF, nrunsJ, timeLimit, new HandParameters() { IsComplicated = true });
                }
                else if (testType == "LSTM")
                {
                    var benchmark = new LSTMBenchmark();
                    benchmark.Run(modulePath, inputFilePath, outputPrefix, minimumMeasurableTime, nrunsF, nrunsJ, timeLimit);
                }
                else
                {
                    throw new Exception("Dotnet runner doesn't support tests of " + testType + " type");
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine("An exception caught: " + ex.ToString());
                return 1;
            }
            return 0;
        }
    }
}
