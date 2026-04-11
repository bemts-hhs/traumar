using System;
using System.Collections.Generic;
using Xunit;
using Traumar.Models;
using Traumar.Core;

namespace Traumar.Tests.Core
{
    public class PerformanceCalculatorTests
    {
        [Fact]
        public void CalculateTraumaPerformance_ValidData_CalculatesScores()
        {
            var records = new List<PatientRecord>();
            for(int i = 0; i < 100; i++)
            {
                records.Add(new PatientRecord { Ps = 0.8, Outcome = 1 });
            }

            var result = PerformanceCalculator.CalculateTraumaPerformance(records);

            // wScore: 100 survivors. expected: 80. diff: 20 per 100.
            Assert.Equal(20.0, result.WScore, 4);
            
            // ZScore: denom = sqrt(100 * (0.8 * 0.2)) = sqrt(16) = 4. num = 20. z = 5.
            Assert.Equal(5.0, result.ZScore, 4);

            // MScore Array: all 0.8 goes to range 0.76-0.90 (index 2)
            Assert.Equal(0, result.MScore[0]);
            Assert.Equal(0, result.MScore[1]);
            Assert.Equal(1.0, result.MScore[2]);
            Assert.Equal(0, result.MScore[3]);
        }

        [Fact]
        public void CalculateTraumaPerformance_ZeroVariance_ThrowsException()
        {
            var records = new List<PatientRecord>();
            for(int i = 0; i < 20; i++)
            {
                records.Add(new PatientRecord { Ps = 1.0, Outcome = 1 }); // Extreme variance
            }

            var ex = Assert.Throws<InvalidOperationException>(() => PerformanceCalculator.CalculateTraumaPerformance(records));
            Assert.Contains("Z-Score 分母為零", ex.Message);
        }

        [Fact]
        public void CalculateTraumaPerformance_TooFewData_ThrowsArgumentException()
        {
            var records = new List<PatientRecord> { new PatientRecord { Ps = 0.5, Outcome = 1 } };
            var ex = Assert.Throws<ArgumentException>(() => PerformanceCalculator.CalculateTraumaPerformance(records, minSampleSize: 10));
            Assert.Contains("資料量不足", ex.Message);
        }
    }
}
