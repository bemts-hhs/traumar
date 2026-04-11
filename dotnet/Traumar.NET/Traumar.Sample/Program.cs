using System;
using System.Collections.Generic;
using Traumar.Models;
using Traumar.Core;
using Traumar.Seqic;

namespace Traumar.Sample
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("=== Traumar.NET Sample Application ===");
            
            // 1. Survival Probability Example
            var input = new PatientInput { Age = 45, InjuryType = InjuryType.Blunt, Iss = 16, Rts = 7.84 };
            var outRecord = SurvivalCalculator.ProbabilityOfSurvival(input);
            Console.WriteLine($"\n[Survival Probability]");
            Console.WriteLine($"Patient: Age=45, ISS=16, RTS=7.84, Blunt -> Ps = {outRecord.Ps:P2}");

            // 2. Metrics Example
            var dataset = new[] {
                new PatientRecord { Age = 45, InjuryType = InjuryType.Blunt, Iss = 16, Rts = 7.84, Outcome = 1, Ps = 0.95 },
                new PatientRecord { Age = 55, InjuryType = InjuryType.Penetrating, Iss = 25, Rts = 5.0, Outcome = 0, Ps = 0.40 }
            };

            try 
            {
                var rmmResult = RmmCalculator.CalculateRmm(dataset);
                var perfResult = PerformanceCalculator.CalculateTraumaPerformance(dataset);

                Console.WriteLine($"\n[Trauma Performance]");
                Console.WriteLine($"W-Score: {perfResult.WScore:F4}, M-Score: [{string.Join(", ", perfResult.MScore)}], Z-Score: {perfResult.ZScore:F4}");
                Console.WriteLine($"Population RMM: {rmmResult.PopulationRMM:F4}");
            }
            catch (Exception)
            {
                Console.WriteLine($"\n[Trauma Performance]");
                Console.WriteLine("Performance metrics (W/M/Z-Score, RMM) require a larger and more varied dataset to process correctly.");
            }

            // 3. SEQIC Example
            var seqicInput = new[] {
                new Indicator1Input { UniqueIncidentId = "1", Level = TraumaLevel.I, ActivationLevel = TraumaTeamActivationLevel.Level1, ResponseTime = 5, ActivationProvider = "ED Physician" },
                new Indicator1Input { UniqueIncidentId = "2", Level = TraumaLevel.I, ActivationLevel = TraumaTeamActivationLevel.Level1, ResponseTime = 20, ActivationProvider = "EMS" }
            };

            var seqic1 = Indicators1to4.CalculateIndicator1(seqicInput, new[] { TraumaLevel.I });
            Console.WriteLine($"\n[SEQIC Indicator 1a (Level 1 Activation Response < 15min)]");
            Console.WriteLine($"Numerator: {seqic1.Indicator1A.Numerator}, Denominator: {seqic1.Indicator1A.Denominator}, Rate: {seqic1.Indicator1A.Rate:P2}");

            Console.WriteLine("\n[Finished]");
        }
    }
}
