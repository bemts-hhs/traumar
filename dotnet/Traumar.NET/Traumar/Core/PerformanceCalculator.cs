using System;
using System.Collections.Generic;
using System.Linq;
using Traumar.Models;

namespace Traumar.Core
{
    public static class PerformanceCalculator
    {
        private static readonly double[] MtosDistribution = new double[] 
        { 
            0.842, // 0.96 - 1.00
            0.053, // 0.91 - 0.95
            0.052, // 0.76 - 0.90
            0.0,   // 0.51 - 0.75
            0.043, // 0.26 - 0.50
            0.01   // 0.00 - 0.25
        };

        public static TraumaPerformanceResult CalculateTraumaPerformance(IEnumerable<PatientRecord> records, int minSampleSize = 10)
        {
            var recordList = records.ToList();
            int totalPatients = recordList.Count;

            if (totalPatients < minSampleSize)
                throw new ArgumentException($"資料量不足：需要至少 {minSampleSize} 筆，目前僅有 {totalPatients} 筆。", nameof(records));

            int totalSurvivors = recordList.Count(r => r.Outcome == 1);
            double expectedSurvivors = recordList.Sum(r => r.Ps);

            // Calculate W Score
            double wScore = (totalSurvivors - expectedSurvivors) / (totalPatients / 100.0);

            // Calculate Z Score
            double scaleFactorSum = recordList.Sum(r => r.Ps * (1.0 - r.Ps));
            if (scaleFactorSum == 0)
                throw new InvalidOperationException("Z-Score 分母為零 (所有病患的存活機率皆為極值 0 或 1)。");

            double zScore = (totalSurvivors - expectedSurvivors) / Math.Sqrt(scaleFactorSum);

            // Calculate M Score array
            int[] rangeCounts = new int[6];
            foreach (var r in recordList)
            {
                if (r.Ps >= 0.96) rangeCounts[0]++;
                else if (r.Ps >= 0.91) rangeCounts[1]++;
                else if (r.Ps >= 0.76) rangeCounts[2]++;
                else if (r.Ps >= 0.51) rangeCounts[3]++;
                else if (r.Ps >= 0.26) rangeCounts[4]++;
                else rangeCounts[5]++;
            }

            double[] mScoreArray = new double[6];
            for (int i = 0; i < 6; i++)
            {
                mScoreArray[i] = (double)rangeCounts[i] / totalPatients;
            }

            return new TraumaPerformanceResult
            {
                WScore = wScore,
                MScore = mScoreArray,
                ZScore = zScore
            };
        }
    }
}
