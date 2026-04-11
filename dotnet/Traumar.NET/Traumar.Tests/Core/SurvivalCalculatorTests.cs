using System;
using Xunit;
using Traumar.Models;
using Traumar.Core;

namespace Traumar.Tests.Core
{
    public class SurvivalCalculatorTests
    {
        [Theory]
        [InlineData(InjuryType.Blunt, 30, 7.84, 10, 0.993655)]
        [InlineData(InjuryType.Penetrating, 60, 6.90, 25, 0.825700)]
        public void ProbabilityOfSurvival_ValidInputs_ReturnsExpectedPs(InjuryType type, int age, double rts, int iss, double expectedPs)
        {
            var input = new PatientInput
            {
                InjuryType = type,
                Age = age,
                Rts = rts,
                Iss = iss
            };

            var record = SurvivalCalculator.ProbabilityOfSurvival(input);

            Assert.Equal(expectedPs, record.Ps, 4);
        }

        [Fact]
        public void ProbabilityOfSurvival_NegativeAge_ThrowsException()
        {
            var input = new PatientInput { InjuryType = InjuryType.Blunt, Age = -1, Rts = 7.84, Iss = 10 };
            var ex = Assert.Throws<ArgumentOutOfRangeException>(() => SurvivalCalculator.ProbabilityOfSurvival(input));
            Assert.Contains("年齡不能為負數", ex.Message);
        }

        [Fact]
        public void ProbabilityOfSurvival_InvalidRts_ThrowsException()
        {
            var input = new PatientInput { InjuryType = InjuryType.Blunt, Age = 30, Rts = 8.0, Iss = 10 };
            var ex = Assert.Throws<ArgumentOutOfRangeException>(() => SurvivalCalculator.ProbabilityOfSurvival(input));
            Assert.Contains("RTS 必須在 0 到 7.8408 之間", ex.Message);
        }

        [Fact]
        public void ProbabilityOfSurvival_InvalidIss_ThrowsException()
        {
            var input = new PatientInput { InjuryType = InjuryType.Blunt, Age = 30, Rts = 7.84, Iss = 76 };
            var ex = Assert.Throws<ArgumentOutOfRangeException>(() => SurvivalCalculator.ProbabilityOfSurvival(input));
            Assert.Contains("ISS 必須在 0 到 75 之間", ex.Message);
        }
    }
}
