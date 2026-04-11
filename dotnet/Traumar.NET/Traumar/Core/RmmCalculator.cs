using System;
using System.Collections.Generic;
using System.Linq;
using Traumar.Models;

namespace Traumar.Core
{
    public static class RmmCalculator
    {
        public static RmmResult CalculateRmm(IEnumerable<PatientRecord> records, int minSampleSize = 10, int nSamples = 100, int? seed = null)
        {
            var recordList = records.ToList();
            int totalRecords = recordList.Count;

            if (totalRecords < minSampleSize)
                throw new ArgumentException($"資料量不足：需要至少 {minSampleSize} 筆，目前僅有 {totalRecords} 筆。", nameof(records));

            // Population Calculation
            var popBins = NonLinearBinsCalculator.CalculateBins(recordList);
            CalculateRmmAndCiForBins(popBins, totalRecords, out double popRmm, out double popCi);

            // Bootstrap Calculation
            var random = seed.HasValue ? new Random(seed.Value) : new Random();
            var bootRmms = new List<double>();

            for (int i = 0; i < nSamples; i++)
            {
                var sample = new List<PatientRecord>(totalRecords);
                for (int j = 0; j < totalRecords; j++)
                {
                    sample.Add(recordList[random.Next(totalRecords)]);
                }

                if (NonLinearBinsCalculator.TryCalculateBins(sample, out var bootBins))
                {
                    CalculateRmmAndCiForBins(bootBins, sample.Count, out double bootRmm, out _);
                    bootRmms.Add(bootRmm);
                }
            }

            double bootMeanRmm = 0;
            double bootCi = 0;

            if (bootRmms.Count > 0)
            {
                bootMeanRmm = bootRmms.Average();
                
                if (bootRmms.Count > 1)
                {
                    double sumSq = bootRmms.Sum(r => Math.Pow(r - bootMeanRmm, 2));
                    double sd = Math.Sqrt(sumSq / (bootRmms.Count - 1));
                    double se = sd / Math.Sqrt(nSamples);
                    bootCi = 1.96 * se;
                }
            }

            double Clip(double v) => Math.Max(-1.0, Math.Min(1.0, v));

            return new RmmResult
            {
                PopulationRMM = popRmm,
                PopulationRMM_LL = Clip(popRmm - popCi),
                PopulationRMM_UL = Clip(popRmm + popCi),
                PopulationCI = popCi,
                
                BootstrapRMM = bootMeanRmm,
                BootstrapRMM_LL = Clip(bootMeanRmm - bootCi),
                BootstrapRMM_UL = Clip(bootMeanRmm + bootCi),
                BootstrapCI = bootCi
            };
        }

        private static void CalculateRmmAndCiForBins(List<BinStatistics> bins, int totalRecords, out double rmm, out double ci)
        {
            double numerator = 0;
            double denominator = 0;
            double sumAntiM = 0;
            double sumAntiS = 0;
            int sumNb = 0;

            foreach (var bin in bins)
            {
                double rb = bin.BinMax - bin.BinMin;
                double expectedDeaths = bin.PatientCount - bin.ExpectedSurvivors;
                
                double antiM = expectedDeaths / bin.PatientCount;
                double antiS = bin.ExpectedSurvivors / bin.PatientCount;
                
                double observedDeaths = bin.PatientCount - bin.ObservedSurvivors;
                double em = observedDeaths / bin.PatientCount;

                numerator += rb * (antiM - em);
                denominator += rb * antiM;

                sumAntiM += antiM;
                sumAntiS += antiS;
                sumNb += bin.PatientCount;
            }

            if (Math.Abs(denominator) < 1e-12)
            {
                if (numerator > 1e-12) rmm = 1.0;
                else if (numerator < -1e-12) rmm = -1.0;
                else rmm = 0.0;
            }
            else
            {
                double val = numerator / denominator;
                if (double.IsNaN(val))
                    rmm = 0.0;
                else
                    rmm = Math.Max(-1.0, Math.Min(1.0, val));
            }

            if (sumNb > 0)
            {
                ci = 1.96 * Math.Sqrt((sumAntiM * sumAntiS) / sumNb);
            }
            else
            {
                ci = 0;
            }
        }
    }
}
