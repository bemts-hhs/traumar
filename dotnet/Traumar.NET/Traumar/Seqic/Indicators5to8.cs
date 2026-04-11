using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Traumar.Models;

namespace Traumar.Seqic
{
    public static class Indicators5to8
    {
        private static readonly string[] DrugKeywords = new string[] 
        {
            "alcohol", "bzo", "benzodiazepine", "amp", "amphetamine", "coc", "cocaine", "thc", "cannabinoid", 
            "opi", "opioid", "pcp", "phencyclidine", "bar", "barbiturate", "mamp", "methamphetamine", "mdma", 
            "ectasy", "mtd", "methadone", "tca", "tricyclic antidepressant", "oxy", "oxycodone", "none", "other"
        };
        private static readonly string[] PositiveDrugKeywords = DrugKeywords.Where(k => k != "none").ToArray();

        private static readonly Regex DrugPattern = new Regex($"(?:{string.Join("|", DrugKeywords)})", RegexOptions.IgnoreCase | RegexOptions.Compiled);
        private static readonly Regex PositiveDrugPattern = new Regex($"(?:{string.Join("|", PositiveDrugKeywords)})", RegexOptions.IgnoreCase | RegexOptions.Compiled);

        public static Indicator5Result CalculateIndicator5(IEnumerable<Indicator5Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var distinctIncidents = data
                .Where(x => validLevelsHashSet.Contains(x.Level))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            var aDenom = distinctIncidents.Count;
            var aNum = distinctIncidents.Count(x => x.BloodAlcoholContent.HasValue);

            var bDenom = distinctIncidents.Count(x => x.BloodAlcoholContent.HasValue);
            var bNum = distinctIncidents.Count(x => x.BloodAlcoholContent.HasValue && x.BloodAlcoholContent.Value > 0);

            var cDenom = distinctIncidents.Count;
            var cNum = distinctIncidents.Count(x => !string.IsNullOrEmpty(x.DrugScreen) && DrugPattern.IsMatch(x.DrugScreen));

            var dDenom = distinctIncidents.Count(x => !string.IsNullOrEmpty(x.DrugScreen) && DrugPattern.IsMatch(x.DrugScreen));
            var dNum = distinctIncidents.Count(x => !string.IsNullOrEmpty(x.DrugScreen) && PositiveDrugPattern.IsMatch(x.DrugScreen));

            return new Indicator5Result
            {
                Indicator5A = new SeqicRate { Numerator = aNum, Denominator = aDenom },
                Indicator5B = new SeqicRate { Numerator = bNum, Denominator = bDenom },
                Indicator5C = new SeqicRate { Numerator = cNum, Denominator = cDenom },
                Indicator5D = new SeqicRate { Numerator = dNum, Denominator = dDenom }
            };
        }

        public static Indicator6Result CalculateIndicator6(IEnumerable<Indicator6Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var distinctIncidents = data
                .Where(x => validLevelsHashSet.Contains(x.Level))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            var denomSet = distinctIncidents.Where(x => x.LowGcsIndicator && x.ReceivingIndicator == YesNo.Yes && x.TransferOutIndicator == YesNo.No).ToList();
            var dDenom = denomSet.Count;
            var dNum = denomSet.Count(x => x.TimeFromInjuryToArrival > 180);

            return new Indicator6Result
            {
                Indicator6 = new SeqicRate { Numerator = dNum, Denominator = dDenom }
            };
        }

        public static Indicator7Result CalculateIndicator7(IEnumerable<Indicator7Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var distinctIncidents = data
                .Where(x => validLevelsHashSet.Contains(x.Level))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            var dDenom = distinctIncidents.Count;
            var dNum = distinctIncidents.Count(x => x.TransferOutIndicator == YesNo.No && x.TimeFromInjuryToArrival > 180);

            return new Indicator7Result
            {
                Indicator7 = new SeqicRate { Numerator = dNum, Denominator = dDenom }
            };
        }

        public static Indicator8Result CalculateIndicator8(IEnumerable<Indicator8Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var distinctIncidents = data
                .Where(x => validLevelsHashSet.Contains(x.Level))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            var allNum = distinctIncidents.Count(x => x.MortalityIndicator != YesNo.Yes);
            var allDenom = distinctIncidents.Count;

            var riskDict = new Dictionary<RiskGroup, SeqicRate>();
            foreach (var r in distinctIncidents.GroupBy(x => x.RiskGroup))
            {
                riskDict[r.Key] = new SeqicRate
                {
                    Denominator = r.Count(),
                    Numerator = r.Count(x => x.MortalityIndicator != YesNo.Yes)
                };
            }

            return new Indicator8Result
            {
                Overall = new SeqicRate { Numerator = allNum, Denominator = allDenom },
                RiskGroupScores = riskDict
            };
        }
    }
}
