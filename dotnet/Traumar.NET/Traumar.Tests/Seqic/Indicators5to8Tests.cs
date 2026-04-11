using System;
using System.Collections.Generic;
using Xunit;
using Traumar.Models;
using Traumar.Seqic;

namespace Traumar.Tests.Seqic
{
    public class Indicators5to8Tests
    {
        [Fact]
        public void CalculateIndicator5_EvaluatesCorrectly()
        {
            var data = new List<Indicator5Input>
            {
                new Indicator5Input { UniqueIncidentId = "1", Level = TraumaLevel.I, BloodAlcoholContent = 0.08, DrugScreen = "OPIOID" },
                new Indicator5Input { UniqueIncidentId = "2", Level = TraumaLevel.I, BloodAlcoholContent = 0.0, DrugScreen = "None" },
                new Indicator5Input { UniqueIncidentId = "3", Level = TraumaLevel.I, BloodAlcoholContent = null, DrugScreen = "SomethingElse" }
            };

            var result = Indicators5to8.CalculateIndicator5(data);

            // 5a: tested
            Assert.Equal(3, result.Indicator5A.Denominator);
            Assert.Equal(2, result.Indicator5A.Numerator); // 1, 2

            // 5b: tested > 0
            Assert.Equal(2, result.Indicator5B.Denominator);
            Assert.Equal(1, result.Indicator5B.Numerator); // 1

            // 5c: matched drug
            Assert.Equal(3, result.Indicator5C.Denominator);
            Assert.Equal(2, result.Indicator5C.Numerator); // 1, 2

            // 5d: among matched, positive drug
            Assert.Equal(2, result.Indicator5D.Denominator);
            Assert.Equal(1, result.Indicator5D.Numerator); // 1
        }

        [Fact]
        public void CalculateIndicator6_EvaluatesCorrectly()
        {
            var data = new List<Indicator6Input>
            {
                // Numerator match
                new Indicator6Input { UniqueIncidentId = "1", Level = TraumaLevel.I, LowGcsIndicator = true, ReceivingIndicator = YesNo.Yes, TransferOutIndicator = YesNo.No, TimeFromInjuryToArrival = 200 },
                // Denominator match only
                new Indicator6Input { UniqueIncidentId = "2", Level = TraumaLevel.I, LowGcsIndicator = true, ReceivingIndicator = YesNo.Yes, TransferOutIndicator = YesNo.No, TimeFromInjuryToArrival = 150 },
                // Excluded (No low GCS)
                new Indicator6Input { UniqueIncidentId = "3", Level = TraumaLevel.I, LowGcsIndicator = false, ReceivingIndicator = YesNo.Yes, TransferOutIndicator = YesNo.No, TimeFromInjuryToArrival = 200 }
            };

            var result = Indicators5to8.CalculateIndicator6(data);
            Assert.Equal(2, result.Indicator6.Denominator);
            Assert.Equal(1, result.Indicator6.Numerator);
        }

        [Fact]
        public void CalculateIndicator7_EvaluatesCorrectly()
        {
            var data = new List<Indicator7Input>
            {
                new Indicator7Input { UniqueIncidentId = "1", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, TimeFromInjuryToArrival = 200 },
                new Indicator7Input { UniqueIncidentId = "2", Level = TraumaLevel.I, TransferOutIndicator = YesNo.Yes, TimeFromInjuryToArrival = 200 },
                new Indicator7Input { UniqueIncidentId = "3", Level = TraumaLevel.I, TransferOutIndicator = YesNo.No, TimeFromInjuryToArrival = 150 }
            };

            var result = Indicators5to8.CalculateIndicator7(data);
            Assert.Equal(3, result.Indicator7.Denominator);
            Assert.Equal(1, result.Indicator7.Numerator); // ID = 1
        }

        [Fact]
        public void CalculateIndicator8_EvaluatesCorrectly()
        {
            var data = new List<Indicator8Input>
            {
                new Indicator8Input { UniqueIncidentId = "1", Level = TraumaLevel.I, MortalityIndicator = YesNo.No, RiskGroup = RiskGroup.High },
                new Indicator8Input { UniqueIncidentId = "2", Level = TraumaLevel.I, MortalityIndicator = YesNo.No, RiskGroup = RiskGroup.Low },
                new Indicator8Input { UniqueIncidentId = "3", Level = TraumaLevel.I, MortalityIndicator = YesNo.Yes, RiskGroup = RiskGroup.High }
            };

            var result = Indicators5to8.CalculateIndicator8(data);

            // Overall
            Assert.Equal(3, result.Overall.Denominator);
            Assert.Equal(2, result.Overall.Numerator); // 1, 2 = survived

            // High risk
            Assert.True(result.RiskGroupScores.ContainsKey(RiskGroup.High));
            Assert.Equal(2, result.RiskGroupScores[RiskGroup.High].Denominator);
            Assert.Equal(1, result.RiskGroupScores[RiskGroup.High].Numerator); // ID = 1

            // Low risk
            Assert.True(result.RiskGroupScores.ContainsKey(RiskGroup.Low));
            Assert.Equal(1, result.RiskGroupScores[RiskGroup.Low].Denominator);
            Assert.Equal(1, result.RiskGroupScores[RiskGroup.Low].Numerator); // ID = 2
        }
    }
}
