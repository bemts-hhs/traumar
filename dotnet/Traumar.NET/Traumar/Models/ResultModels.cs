namespace Traumar.Models
{
    /// <summary>
    /// RMM 計算結果
    /// </summary>
    public class RmmResult
    {
        /// <summary>
        /// Population RMM 點估計
        /// </summary>
        public double PopulationRMM { get; set; }

        /// <summary>
        /// Population CI 下限
        /// </summary>
        public double PopulationRMM_LL { get; set; }

        /// <summary>
        /// Population CI 上限
        /// </summary>
        public double PopulationRMM_UL { get; set; }

        /// <summary>
        /// Population CI 寬度
        /// </summary>
        public double PopulationCI { get; set; }

        /// <summary>
        /// Bootstrap RMM 點估計
        /// </summary>
        public double BootstrapRMM { get; set; }

        /// <summary>
        /// Bootstrap CI 下限
        /// </summary>
        public double BootstrapRMM_LL { get; set; }

        /// <summary>
        /// Bootstrap CI 上限
        /// </summary>
        public double BootstrapRMM_UL { get; set; }

        /// <summary>
        /// Bootstrap CI 寬度
        /// </summary>
        public double BootstrapCI { get; set; }
    }

    /// <summary>
    /// 績效評分
    /// </summary>
    public class TraumaPerformanceResult
    {
        /// <summary>
        /// W Score（每 100 名病患超基準存活人數）
        /// </summary>
        public double WScore { get; set; }

        /// <summary>
        /// M Score（各 bin 比例向量，長度與 R 版本一致）
        /// </summary>
        public double[] MScore { get; set; }

        /// <summary>
        /// Z Score
        /// </summary>
        public double ZScore { get; set; }
    }
}
