using System;
using System.Collections.Generic;

namespace Traumar.Models
{
    public enum TraumaTeamActivationLevel
    {
        Other,
        Level1,
        Level2,
        Level3
    }

    public enum PhysicianServiceType
    {
        Other,
        SurgeryTrauma,
        EmergencyMedicine,
        FamilyPractice,
        NursePractitioner,
        PhysicianAssistant,
        SurgerySeniorResident,
        Hospitalist,
        InternalMedicine
    }

    public enum TraumaLevel
    {
        Other,
        I,
        II,
        III,
        IV,
        V
    }

    public enum TraumaType
    {
        Other,
        Blunt,
        Penetrating,
        Burn
    }

    public enum RiskGroup
    {
        Unknown,
        Low,
        Moderate,
        High
    }

    public enum Disposition
    {
        Other,
        DeceasedExpired,
        Admitted,
        Discharged,
        Transferred,
        OperatingRoom
    }

    public enum YesNo
    {
        Unknown,
        Yes,
        No
    }

    public class SeqicRate
    {
        public int Numerator { get; set; }
        public int Denominator { get; set; }
        public double? Rate => Denominator > 0 ? (double)Numerator / Denominator : (double?)null;
    }

    // Indicator 1
    public class Indicator1Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaTeamActivationLevel ActivationLevel { get; set; }
        public PhysicianServiceType ServiceType { get; set; }
        public TraumaLevel Level { get; set; }
        public double? ResponseTime { get; set; }
        public string ActivationProvider { get; set; }
    }

    public class Indicator1Result
    {
        public SeqicRate Indicator1A { get; set; }
        public SeqicRate Indicator1B { get; set; }
        public SeqicRate Indicator1C { get; set; }
        public SeqicRate Indicator1D { get; set; }
        public SeqicRate Indicator1E { get; set; }
        public SeqicRate Indicator1F { get; set; }
    }

    // Indicator 2
    public class Indicator2Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public DateTime? IncidentTime { get; set; }
    }

    public class Indicator2Result
    {
        public SeqicRate Indicator2 { get; set; }
    }

    // Indicator 3
    public class Indicator3Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public TraumaType TraumaType { get; set; }
        public double? ProbabilityOfSurvival { get; set; }
    }

    public class Indicator3Result
    {
        public SeqicRate Indicator3 { get; set; }
    }

    // Indicator 4
    public class Indicator4Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public Disposition EdDisposition { get; set; }
        public double? EdLos { get; set; }
        public Disposition HospitalDisposition { get; set; }
        public double? HospitalLos { get; set; }
        public YesNo? Autopsy { get; set; }
    }

    public class Indicator4Result
    {
        public SeqicRate Indicator4A { get; set; }
        public SeqicRate Indicator4B { get; set; }
    }

    // Indicator 5
    public class Indicator5Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public double? BloodAlcoholContent { get; set; }
        public string DrugScreen { get; set; }
    }

    public class Indicator5Result
    {
        public SeqicRate Indicator5A { get; set; }
        public SeqicRate Indicator5B { get; set; }
        public SeqicRate Indicator5C { get; set; }
        public SeqicRate Indicator5D { get; set; }
    }

    // Indicator 6
    public class Indicator6Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public YesNo TransferOutIndicator { get; set; }
        public YesNo ReceivingIndicator { get; set; }
        public bool LowGcsIndicator { get; set; }
        public double TimeFromInjuryToArrival { get; set; }
    }

    public class Indicator6Result
    {
        public SeqicRate Indicator6 { get; set; }
    }

    // Indicator 7
    public class Indicator7Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public YesNo TransferOutIndicator { get; set; }
        public double TimeFromInjuryToArrival { get; set; }
    }

    public class Indicator7Result
    {
        public SeqicRate Indicator7 { get; set; }
    }

    // Indicator 8
    public class Indicator8Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public YesNo MortalityIndicator { get; set; }
        public RiskGroup RiskGroup { get; set; }
    }

    public class Indicator8Result
    {
        public SeqicRate Overall { get; set; }
        public Dictionary<RiskGroup, SeqicRate> RiskGroupScores { get; set; }
    }

    // Indicator 9
    public class Indicator9Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public YesNo TransferOutIndicator { get; set; }
        public string TransportMethod { get; set; }
        public YesNo TraumaTeamActivated { get; set; }
        public RiskGroup RiskGroup { get; set; }
        public double? EdLos { get; set; }
        public double? EdDecisionLos { get; set; }
        public double? EdDecisionDischargeLos { get; set; }
    }

    public class Indicator9MultiResult 
    {
        public SeqicRate Indicator9A { get; set; }
        public SeqicRate Indicator9B { get; set; }
        public SeqicRate Indicator9C { get; set; }
        public SeqicRate Indicator9D { get; set; }
        public SeqicRate Indicator9E { get; set; }
        public SeqicRate Indicator9F { get; set; }
    }

    public class Indicator9Result 
    {
        public Indicator9MultiResult Overall { get; set; }
        public Dictionary<YesNo, Indicator9MultiResult> Activations { get; set; }
        public Dictionary<RiskGroup, Indicator9MultiResult> RiskGroups { get; set; }
    }

    // Indicator 10
    public class Indicator10Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public YesNo TransferOutIndicator { get; set; }
        public TraumaTeamActivationLevel ActivationLevel { get; set; }
        public int? Iss { get; set; }
        public YesNo? Nfti { get; set; }
    }

    public class Indicator10Result 
    {
        public SeqicRate Indicator10A { get; set; }
        public SeqicRate Indicator10B { get; set; }
        public SeqicRate Indicator10C { get; set; }
    }

    // Indicator 11
    public class Indicator11Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public YesNo TransferOutIndicator { get; set; }
        public YesNo ReceivingIndicator { get; set; }
        public int Iss { get; set; }
        public double EdLos { get; set; }
    }

    public class Indicator11Result 
    {
        public SeqicRate Indicator11 { get; set; }
    }

    // Indicator 12
    public class Indicator12Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public string FacilityId { get; set; }
        public double DataEntryTime { get; set; }
    }

    public class Indicator12Result 
    {
        public SeqicRate Indicator12 { get; set; }
    }

    // Indicator 13
    public class Indicator13Input
    {
        public string UniqueIncidentId { get; set; }
        public TraumaLevel Level { get; set; }
        public double ValidityScore { get; set; }
    }

    public class Indicator13Result 
    {
        public SeqicRate Indicator13 { get; set; }
    }
}
