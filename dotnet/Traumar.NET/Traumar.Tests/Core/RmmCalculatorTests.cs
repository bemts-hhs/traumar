using System;
using System.Collections.Generic;
using Xunit;
using Traumar.Models;
using Traumar.Core;

namespace Traumar.Tests.Core
{
    public class RmmCalculatorTests
    {
        [Fact]
        public void CalculateRmm_AllSurvivors_ReturnsPositiveRmm()
        {
            var records = new List<PatientRecord>();
            for(int i = 1; i <= 100; i++)
            {
                records.Add(new PatientRecord { Ps = i / 100.0, Outcome = 1 });
            }

            var result = RmmCalculator.CalculateRmm(records);

            // RMM should be > 0 because observed mortality (0) is less than expected mortality
            Assert.True(result.PopulationRMM > 0);
        }

        [Fact]
        public void CalculateRmm_AllDead_ReturnsNegativeRmm()
        {
            var records = new List<PatientRecord>();
            for(int i = 1; i <= 100; i++)
            {
                records.Add(new PatientRecord { Ps = i / 100.0, Outcome = 0 });
            }

            var result = RmmCalculator.CalculateRmm(records);

            // RMM should be < 0 because observed mortality (1) is greater than expected mortality
            Assert.True(result.PopulationRMM < 0);
        }

        [Fact]
        public void CalculateRmm_TooFewData_ThrowsArgumentException()
        {
            var records = new List<PatientRecord> { new PatientRecord { Ps = 0.5, Outcome = 1 } };
            var ex = Assert.Throws<ArgumentException>(() => RmmCalculator.CalculateRmm(records, minSampleSize: 10));
            Assert.Contains("資料量不足", ex.Message);
        }
    }
}
