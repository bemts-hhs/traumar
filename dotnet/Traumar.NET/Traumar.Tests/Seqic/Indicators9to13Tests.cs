using System;
using System.Collections.Generic;
using Xunit;
using Traumar.Models;
using Traumar.Seqic;

namespace Traumar.Tests.Seqic
{
    public class Indicators9to13Tests
    {
        [Fact]
        public void CalculateIndicator9_EvaluatesCorrectly()
        {
            var data = new List<Indicator9Input>
            {
                new Indicator9Input { UniqueIncidentId = "1", Level = TraumaLevel.I, TransferOutIndicator = YesNo.Yes, TransportMethod = "Ambulance", TraumaTeamActivated = YesNo.Yes, RiskGroup = RiskGroup.High, EdLos = 200, EdDecisionLos = 130, EdDecisionDischargeLos = 130 },
                new Indicator9Input { UniqueIncidentId = "2", Level = TraumaLevel.I, TransferOutIndicator = YesNo.Yes, TransportMethod = "Private Vehicle", TraumaTeamActivated = YesNo.Yes, RiskGroup = RiskGroup.High, EdLos = 200 }, // Excluded by transport
            };

            var res = Indicators9to13.CalculateIndicator9(data);

            Assert.Equal(1, res.Overall.Indicator9A.Denominator);
            Assert.Equal(1, res.Overall.Indicator9A.Numerator); // ED Los > 120 (200 > 120)
            Assert.Equal(1, res.Overall.Indicator9B.Numerator); // ED Los > 180 (200 > 180)

            Assert.Equal(1, res.Overall.Indicator9C.Numerator); // Decision > 60
            Assert.Equal(1, res.Overall.Indicator9E.Numerator); // Decision to Discharge > 60
        }

        [Fact]
        public void CalculateIndicator10_EvaluatesCorrectly()
        {
            var data = new List<Indicator10Input>
            {
                // Undertriage: Limited activation, but major trauma
                new Indicator10Input { UniqueIncidentId = "1", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, ActivationLevel = TraumaTeamActivationLevel.Level2, Iss = 25 },
                // Overtriage: Full activation, but minor trauma
                new Indicator10Input { UniqueIncidentId = "2", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, ActivationLevel = TraumaTeamActivationLevel.Level1, Iss = 5 },
                // Appropriate: Full activation, major trauma
                new Indicator10Input { UniqueIncidentId = "3", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, ActivationLevel = TraumaTeamActivationLevel.Level1, Iss = 25 }
            };

            var res = Indicators9to13.CalculateIndicator10(data);

            // 10a (Undertriage rate among limited activations) -> ID 1 is limited, so den=1. It is major, so num=1.
            Assert.Equal(1, res.Indicator10A.Denominator);
            Assert.Equal(1, res.Indicator10A.Numerator);

            // 10b (Overtriage rate among full activations) -> ID 2, 3 are full. den=2. ID 2 is minor, so num=1.
            Assert.Equal(2, res.Indicator10B.Denominator);
            Assert.Equal(1, res.Indicator10B.Numerator);

            // 10c (Undertriage rate among major traumas) -> ID 1, 3 are major. den=2. ID 1 is limited, so num=1.
            Assert.Equal(2, res.Indicator10C.Denominator);
            Assert.Equal(1, res.Indicator10C.Numerator);
        }



        [Fact]
        public void CalculateIndicator11_EvaluatesCorrectly()
        {
            var data = new List<Indicator11Input>
            {
                new Indicator11Input { UniqueIncidentId = "1", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, ReceivingIndicator = YesNo.Yes, Iss = 5, EdLos = 500 }, // Over
                new Indicator11Input { UniqueIncidentId = "2", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, ReceivingIndicator = YesNo.Yes, Iss = 15, EdLos = 500 } // Normal
            };

            var res = Indicators9to13.CalculateIndicator11(data);
            Assert.Equal(2, res.Indicator11.Denominator);
            Assert.Equal(1, res.Indicator11.Numerator); // ID = 1
        }

        [Fact]
        public void CalculateIndicator12_EvaluatesCorrectly()
        {
            var data = new List<Indicator12Input>
            {
                new Indicator12Input { UniqueIncidentId = "1", Level = TraumaLevel.I, FacilityId = "A", DataEntryTime = 30 },
                new Indicator12Input { UniqueIncidentId = "2", Level = TraumaLevel.I, FacilityId = "A", DataEntryTime = 90 },
                new Indicator12Input { UniqueIncidentId = "3", Level = TraumaLevel.I, FacilityId = "B", DataEntryTime = 30 } // Excluded
            };

            var res = Indicators9to13.CalculateIndicator12(data, new[] {"B"}, 60);

            Assert.Equal(2, res.Indicator12.Denominator); // ID 1, 2
            Assert.Equal(1, res.Indicator12.Numerator); // ID 1
        }

        [Fact]
        public void CalculateIndicator13_EvaluatesCorrectly()
        {
            var data = new List<Indicator13Input>
            {
                new Indicator13Input { UniqueIncidentId = "1", Level = TraumaLevel.I, ValidityScore = 90 }, // Pass
                new Indicator13Input { UniqueIncidentId = "2", Level = TraumaLevel.I, ValidityScore = 80 }, // Fail
            };

            var res = Indicators9to13.CalculateIndicator13(data, 85);

            Assert.Equal(2, res.Indicator13.Denominator);
            Assert.Equal(1, res.Indicator13.Numerator);
        }
    }
}
