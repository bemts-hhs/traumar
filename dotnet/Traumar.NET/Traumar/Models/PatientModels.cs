namespace Traumar.Models
{
    /// <summary>
    /// 傷害類型（鈍傷/穿刺傷）
    /// </summary>
    public enum InjuryType
    {
        Blunt,
        Penetrating
    }

    /// <summary>
    /// 純輸入病患資料
    /// </summary>
    public class PatientInput
    {
        /// <summary>
        /// 傷害類型（鈍傷/穿刺傷）
        /// </summary>
        public InjuryType InjuryType { get; set; }

        /// <summary>
        /// 年齡（非負整數）
        /// </summary>
        public int Age { get; set; }

        /// <summary>
        /// 修正創傷評分 (Revised Trauma Score)
        /// </summary>
        public double Rts { get; set; }

        /// <summary>
        /// 傷害嚴重度評分（0–75）
        /// </summary>
        public int Iss { get; set; }
    }

    /// <summary>
    /// 含計算結果之病患紀錄
    /// </summary>
    public class PatientRecord
    {
        /// <summary>
        /// 傷害類型（鈍傷/穿刺傷）
        /// </summary>
        public InjuryType InjuryType { get; set; }

        /// <summary>
        /// 年齡（非負整數）
        /// </summary>
        public int Age { get; set; }

        /// <summary>
        /// 修正創傷評分 (Revised Trauma Score)
        /// </summary>
        public double Rts { get; set; }

        /// <summary>
        /// 傷害严重度评分（0–75）
        /// </summary>
        public int Iss { get; set; }

        /// <summary>
        /// 存活機率（由 ProbabilityOfSurvival 填入）
        /// </summary>
        public double Ps { get; set; }

        /// <summary>
        /// 實際結果：1=存活，0=死亡（A6 規範）
        /// </summary>
        public int Outcome { get; set; }
    }
}
