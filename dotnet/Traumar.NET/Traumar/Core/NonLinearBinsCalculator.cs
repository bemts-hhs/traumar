using System;
using System.Collections.Generic;
using System.Linq;
using Traumar.Models;

namespace Traumar.Core
{
    internal class BinStatistics
    {
        public double BinMin { get; set; }
        public double BinMax { get; set; }
        public int ObservedSurvivors { get; set; }
        public double ExpectedSurvivors { get; set; }
        public int PatientCount { get; set; }
    }

    internal static class NonLinearBinsCalculator
    {
        public static bool TryCalculateBins(IEnumerable<PatientRecord> records, out List<BinStatistics> bins, double divisor1 = 5.0, double divisor2 = 5.0, double threshold1 = 0.9, double threshold2 = 0.99)
        {
            bins = null;
            var sortedPsList = records.Select(r => r.Ps).OrderBy(p => p).ToList();
            if (sortedPsList.Count < 2) return false;

            int min9A = sortedPsList.FindIndex(p => p > threshold1);
            int min9B = sortedPsList.FindIndex(p => p > threshold2);

            if (min9A == -1 || min9B == -1)
                return false;

            int max9B = sortedPsList.FindLastIndex(p => p > threshold2);
            if (max9B == -1) 
                return false;

            int minLoc9A = min9A + 1;
            int minLoc9B = min9B + 1;
            int maxLoc9B = max9B + 1;

            int len9C = minLoc9B - minLoc9A; 

            int step1 = (int)Math.Round(minLoc9A / divisor1, MidpointRounding.ToEven);
            int step2 = (int)Math.Round(len9C / divisor2, MidpointRounding.ToEven);

            if (step1 <= 0 || step2 <= 0)
                return false;

            var indices = new List<int>();

            for (int i = 1; i <= minLoc9A; i += step1)
                indices.Add(i);

            for (int i = minLoc9A; i <= minLoc9B; i += step2)
                indices.Add(i);

            indices.Add(maxLoc9B);

            indices = indices.Distinct().OrderBy(i => i).ToList();
            var intervals = indices.Select(idx => sortedPsList[idx - 1]).Distinct().OrderBy(v => v).ToList();

            if (intervals.Count < 2) return false;

            bins = new List<BinStatistics>();

            for (int i = 0; i < intervals.Count - 1; i++)
            {
                double binMin = intervals[i];
                double binMax = intervals[i + 1];

                var binRecords = records.Where(r => 
                {
                    if (i == 0)
                        return r.Ps >= binMin && r.Ps <= binMax;
                    else
                        return r.Ps > binMin && r.Ps <= binMax;
                }).ToList();

                if (binRecords.Count > 0)
                {
                    bins.Add(new BinStatistics
                    {
                        BinMin = binMin,
                        BinMax = binMax,
                        ObservedSurvivors = binRecords.Count(r => r.Outcome == 1),
                        ExpectedSurvivors = binRecords.Sum(r => r.Ps),
                        PatientCount = binRecords.Count
                    });
                }
            }

            return true;
        }

        public static List<BinStatistics> CalculateBins(IEnumerable<PatientRecord> records, double divisor1 = 5.0, double divisor2 = 5.0, double threshold1 = 0.9, double threshold2 = 0.99)
        {
            if (TryCalculateBins(records, out var bins, divisor1, divisor2, threshold1, threshold2))
                return bins;
            
            throw new ArgumentException("Unable to calculate valid step sizes for defining survival probability intervals due to non-finite values or insufficient data variation.");
        }
    }
}
