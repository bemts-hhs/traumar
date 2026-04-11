using System;
using Traumar.Models;

namespace Traumar.Core
{
    public static class SurvivalCalculator
    {
        public static PatientRecord ProbabilityOfSurvival(PatientInput input)
        {
            if (input.Age < 0)
                throw new ArgumentOutOfRangeException(nameof(input.Age), input.Age, "年齡不能為負數。");
            if (input.Rts < 0 || input.Rts > 7.8408)
                throw new ArgumentOutOfRangeException(nameof(input.Rts), input.Rts, "RTS 必須在 0 到 7.8408 之間。");
            if (input.Iss < 0 || input.Iss > 75)
                throw new ArgumentOutOfRangeException(nameof(input.Iss), input.Iss, "ISS 必須在 0 到 75 之間。");

            double b0, b1, b2, b3;

            switch (input.InjuryType)
            {
                case InjuryType.Blunt:
                    b0 = -0.4499;
                    b1 = 0.8085;
                    b2 = -0.0835;
                    b3 = -1.7430;
                    break;
                case InjuryType.Penetrating:
                    b0 = -2.5355;
                    b1 = 0.9934;
                    b2 = -0.0651;
                    b3 = -1.1360;
                    break;
                default:
                    throw new ArgumentException("不支援的傷害類型。", nameof(input.InjuryType));
            }

            int agePoints = input.Age < 55 ? 0 : 1;
            double b = b0 + (b1 * input.Rts) + (b2 * input.Iss) + (b3 * agePoints);
            double ps = 1.0 / (1.0 + Math.Exp(-b));

            return new PatientRecord
            {
                InjuryType = input.InjuryType,
                Age = input.Age,
                Rts = input.Rts,
                Iss = input.Iss,
                Ps = ps,
                Outcome = 0
            };
        }
    }
}
