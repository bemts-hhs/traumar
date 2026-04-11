using System;
using System.Collections.Generic;
using Xunit;
using Traumar.Models;
using Traumar.Seqic;

namespace Traumar.Tests.Seqic
{
    public class Indicators1to4Tests
    {
        [Fact]
        public void CalculateIndicator1_EvaluatesCorrectly()
        {
            var data = new List<Indicator1Input>
            {
                // 1A: <= 15m. Matches!
                new Indicator1Input { UniqueIncidentId = "1", ActivationLevel = TraumaTeamActivationLevel.Level1, ServiceType = PhysicianServiceType.SurgeryTrauma, Level = TraumaLevel.I, ResponseTime = 10, ActivationProvider = "Dr. A" },
                // 1A: > 15m. Denom +1, Num +0. (Also matches 1B <= 30m)
                new Indicator1Input { UniqueIncidentId = "2", ActivationLevel = TraumaTeamActivationLevel.Level1, ServiceType = PhysicianServiceType.SurgeryTrauma, Level = TraumaLevel.II, ResponseTime = 20, ActivationProvider = "Dr. B" },
                // 1C: Missing response time.
                new Indicator1Input { UniqueIncidentId = "3", ActivationLevel = TraumaTeamActivationLevel.Level1, ServiceType = PhysicianServiceType.SurgeryTrauma, Level = TraumaLevel.III, ResponseTime = null, ActivationProvider = "Dr. C" }
            };

            var result = Indicators1to4.CalculateIndicator1(data);

            // 1A (Level 1, Surg, Level I/II)
            Assert.Equal(2, result.Indicator1A.Denominator);
            Assert.Equal(1, result.Indicator1A.Numerator);
            Assert.Equal(0.5, result.Indicator1A.Rate);

            // 1B (Level 1, Surg, Level I/II/III, ResponseTime HasValue) -> Note ID=3 is excluded coz ResponseTime is null.
            Assert.Equal(2, result.Indicator1B.Denominator); // ID = 1 and 2
            Assert.Equal(2, result.Indicator1B.Numerator);   // Both <= 30

            // 1C (Missing response time constraint)
            Assert.Equal(3, result.Indicator1C.Denominator);
            Assert.Equal(1, result.Indicator1C.Numerator); // ID = 3
        }

        [Fact]
        public void CalculateIndicator2_EvaluatesCorrectly()
        {
            var data = new List<Indicator2Input>
            {
                new Indicator2Input { UniqueIncidentId = "1", Level = TraumaLevel.I, IncidentTime = DateTime.Now },
                new Indicator2Input { UniqueIncidentId = "2", Level = TraumaLevel.II, IncidentTime = null },
                new Indicator2Input { UniqueIncidentId = "2", Level = TraumaLevel.II, IncidentTime = null } // Duplicate! Should be distinct.
            };

            var result = Indicators1to4.CalculateIndicator2(data);

            Assert.Equal(2, result.Indicator2.Denominator);
            Assert.Equal(1, result.Indicator2.Numerator);
            Assert.Equal(0.5, result.Indicator2.Rate);
        }

        [Fact]
        public void CalculateIndicator3_EvaluatesCorrectly()
        {
            var data = new List<Indicator3Input>
            {
                new Indicator3Input { UniqueIncidentId = "1", Level = TraumaLevel.I, TraumaType = TraumaType.Blunt, ProbabilityOfSurvival = 0.9 },
                new Indicator3Input { UniqueIncidentId = "2", Level = TraumaLevel.I, TraumaType = TraumaType.Penetrating, ProbabilityOfSurvival = null },
                new Indicator3Input { UniqueIncidentId = "3", Level = TraumaLevel.I, TraumaType = TraumaType.Burn, ProbabilityOfSurvival = 0.8 } // Ignored because of Burn
            };

            var result = Indicators1to4.CalculateIndicator3(data);

            Assert.Equal(2, result.Indicator3.Denominator);
            Assert.Equal(1, result.Indicator3.Numerator);
            Assert.Equal(0.5, result.Indicator3.Rate);
        }

        [Fact]
        public void CalculateIndicator4_EvaluatesCorrectly()
        {
            var data = new List<Indicator4Input>
            {
                // Deceased, Has autopsy, Normal LOS
                new Indicator4Input { UniqueIncidentId = "1", Level = TraumaLevel.I, EdDisposition = Disposition.DeceasedExpired, Autopsy = YesNo.Yes, EdLos = 100 },
                // Deceased, No autopsy, Long LOS (>4320)
                new Indicator4Input { UniqueIncidentId = "2", Level = TraumaLevel.I, HospitalDisposition = Disposition.DeceasedExpired, Autopsy = YesNo.No, HospitalLos = 5000 },
                // Alive -> Ignored
                new Indicator4Input { UniqueIncidentId = "3", Level = TraumaLevel.I, EdDisposition = Disposition.Admitted, HospitalDisposition = Disposition.Discharged }
            };

            var result = Indicators1to4.CalculateIndicator4(data);

            // 4A: Deceased patients with autopsy
            Assert.Equal(2, result.Indicator4A.Denominator);
            Assert.Equal(1, result.Indicator4A.Numerator); // ID = 1

            // 4B: Deceased + Long LOS without autopsy
            Assert.Equal(1, result.Indicator4B.Denominator); // ID = 2
            Assert.Equal(1, result.Indicator4B.Numerator);   // ID = 2
        }
    }
}
