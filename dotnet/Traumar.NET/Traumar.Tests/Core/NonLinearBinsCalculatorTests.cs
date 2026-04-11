using System;
using System.Collections.Generic;
using Xunit;
using Traumar.Models;
using Traumar.Core;

namespace Traumar.Tests.Core
{
    public class NonLinearBinsCalculatorTests
    {
        [Fact]
        public void CalculateBins_ValidData_ReturnsCorrectBins()
        {
            var records = new List<PatientRecord>();
            for(int i = 1; i <= 100; i++)
            {
                records.Add(new PatientRecord { Ps = i / 100.0, Outcome = i % 2 == 0 ? 1 : 0 });
            }

            var bins = NonLinearBinsCalculator.CalculateBins(records, divisor1: 5.0, divisor2: 5.0, threshold1: 0.90, threshold2: 0.99);

            Assert.NotEmpty(bins);
            Assert.True(bins.Count > 0);
        }

        [Fact]
        public void CalculateBins_TooFewData_ThrowsArgumentException()
        {
            var records = new List<PatientRecord> { new PatientRecord { Ps = 0.5 } };
            Assert.Throws<ArgumentException>(() => NonLinearBinsCalculator.CalculateBins(records));
        }
    }
}
