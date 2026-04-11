using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Traumar.Models;

namespace Traumar.Seqic
{
    public static class Indicators9to13
    {
        private static readonly Regex ExcludedTransportPattern = new Regex(@"(private vehicle|public vehicle|walk[\s-]in|not\s(known|recorded|applicable)|other)", RegexOptions.IgnoreCase | RegexOptions.Compiled);

        public static Indicator9Result CalculateIndicator9(IEnumerable<Indicator9Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var prep = data
                .Where(x => validLevelsHashSet.Contains(x.Level) && 
                            x.TransferOutIndicator == YesNo.Yes && 
                            (string.IsNullOrEmpty(x.TransportMethod) || !ExcludedTransportPattern.IsMatch(x.TransportMethod)))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            Indicator9MultiResult CalcMulti(IEnumerable<Indicator9Input> subset)
            {
                var sList = subset.ToList();
                int total = sList.Count;
                return new Indicator9MultiResult
                {
                    Indicator9A = new SeqicRate { Numerator = sList.Count(x => x.EdLos > 120), Denominator = total },
                    Indicator9B = new SeqicRate { Numerator = sList.Count(x => x.EdLos > 180), Denominator = total },
                    Indicator9C = new SeqicRate { Numerator = sList.Count(x => x.EdDecisionLos > 60), Denominator = total },
                    Indicator9D = new SeqicRate { Numerator = sList.Count(x => x.EdDecisionLos > 120), Denominator = total },
                    Indicator9E = new SeqicRate { Numerator = sList.Count(x => x.EdDecisionDischargeLos > 60), Denominator = total },
                    Indicator9F = new SeqicRate { Numerator = sList.Count(x => x.EdDecisionDischargeLos > 120), Denominator = total },
                };
            }

            var result = new Indicator9Result
            {
                Overall = CalcMulti(prep),
                Activations = prep.GroupBy(x => x.TraumaTeamActivated).ToDictionary(g => g.Key, g => CalcMulti(g)),
                RiskGroups = prep.GroupBy(x => x.RiskGroup).ToDictionary(g => g.Key, g => CalcMulti(g))
            };

            return result;
        }

        public static Indicator10Result CalculateIndicator10(IEnumerable<Indicator10Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var prep = data
                .Where(x => validLevelsHashSet.Contains(x.Level) && x.TransferOutIndicator == YesNo.No)
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            int num10a = 0, den10a = 0;
            int num10b = 0, den10b = 0;
            int num10c = 0, den10c = 0;

            foreach (var r in prep)
            {
                bool isFullActivation = r.ActivationLevel == TraumaTeamActivationLevel.Level1;
                bool isLimitedActivation = !isFullActivation;

                bool majorTrauma = false;
                bool minorTrauma = false;

                if (r.Nfti.HasValue)
                {
                    majorTrauma = r.Nfti.Value == YesNo.Yes;
                    minorTrauma = r.Nfti.Value == YesNo.No;
                }
                else if (r.Iss.HasValue)
                {
                    majorTrauma = r.Iss.Value > 15;
                    minorTrauma = r.Iss.Value < 9;
                }
                else
                {
                    // No valid triage criteria, skip
                    continue;
                }

                bool undertriage = isLimitedActivation && majorTrauma;
                bool overtriage = isFullActivation && minorTrauma;

                // 10a
                if (isLimitedActivation) den10a++;
                if (undertriage) num10a++;

                // 10b
                if (isFullActivation) den10b++;
                if (overtriage) num10b++;

                // 10c
                if (majorTrauma) den10c++;
                if (undertriage) num10c++;
            }

            return new Indicator10Result
            {
                Indicator10A = new SeqicRate { Numerator = num10a, Denominator = den10a },
                Indicator10B = new SeqicRate { Numerator = num10b, Denominator = den10b },
                Indicator10C = new SeqicRate { Numerator = num10c, Denominator = den10c }
            };
        }

        public static Indicator11Result CalculateIndicator11(IEnumerable<Indicator11Input> data, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var prep = data
                .Where(x => validLevelsHashSet.Contains(x.Level) && 
                            x.TransferOutIndicator == YesNo.No && 
                            x.ReceivingIndicator == YesNo.Yes)
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            var den = prep.Count;
            var num = prep.Count(x => x.Iss < 9 && x.EdLos < 1440);

            return new Indicator11Result
            {
                Indicator11 = new SeqicRate { Numerator = num, Denominator = den }
            };
        }

        public static Indicator12Result CalculateIndicator12(IEnumerable<Indicator12Input> data, IEnumerable<string> excludeFacilityList = null, double dataEntryStandard = 60, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);
            var excluded = excludeFacilityList != null ? new HashSet<string>(excludeFacilityList, StringComparer.OrdinalIgnoreCase) : new HashSet<string>();

            var prep = data
                .Where(x => validLevelsHashSet.Contains(x.Level) && 
                            (x.FacilityId == null || !excluded.Contains(x.FacilityId)))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            return new Indicator12Result
            {
                Indicator12 = new SeqicRate { Denominator = prep.Count, Numerator = prep.Count(x => x.DataEntryTime <= dataEntryStandard) }
            };
        }

        public static Indicator13Result CalculateIndicator13(IEnumerable<Indicator13Input> data, double validityThreshold = 85, IEnumerable<TraumaLevel> includedLevels = null)
        {
            includedLevels = includedLevels ?? new[] { TraumaLevel.I, TraumaLevel.II, TraumaLevel.III, TraumaLevel.IV };
            var validLevelsHashSet = new HashSet<TraumaLevel>(includedLevels);

            var prep = data
                .Where(x => validLevelsHashSet.Contains(x.Level))
                .GroupBy(x => x.UniqueIncidentId)
                .Select(g => g.First())
                .ToList();

            return new Indicator13Result
            {
                Indicator13 = new SeqicRate { Denominator = prep.Count, Numerator = prep.Count(x => x.ValidityScore >= validityThreshold) }
            };
        }
    }
}
