# 實作計畫

## 技術棧 (Tech Stack)
- 目標框架: .NET Framework 4.7.2
- 測試框架: xUnit + .NET 4.7.2
- 第三方套件: `MathNet.Numerics` (提供進階數學與統計運算)

## 架構 (Architecture)

| Namespace | 主要類別 | 對應檔案 |
|-----------|----------|----------|
| `Traumar.Models` | `PatientInput`, `PatientRecord`, `RmmResult`, `TraumaPerformanceResult`, `IndicatorNInput/Result` (N=1~13), `InjuryType` (enum) | `Models/PatientModels.cs`, `Models/ResultModels.cs`, `Models/SeqicModels.cs` |
| `Traumar.Core` (public) | `SurvivalCalculator`, `RmmCalculator`, `PerformanceCalculator` | `Core/SurvivalCalculator.cs`, `Core/RmmCalculator.cs`, `Core/PerformanceCalculator.cs` |
| `Traumar.Core` (internal) | `NonLinearBinsCalculator`, `BinStatistics` | `Core/NonLinearBinsCalculator.cs` |
| `Traumar.Seqic` | `Indicators1to4`, `Indicators5to8`, `Indicators9to13` | `Seqic/Indicators1to4.cs`, `Seqic/Indicators5to8.cs`, `Seqic/Indicators9to13.cs` |
| `Traumar.Sample` | (主控台測試程式範例) | `Traumar.Sample/Program.cs` |

## 階段 (Phases)
1. **Phase 1: Setup**: 初始化 .NET 4.7.2 Library 專案、xUnit 測試專案，修改 `.Rbuildignore` 以避免干擾 R 套件。
2. **Phase 2: Foundations**: 定義核心模型與 DTOs (`PatientInput`, `RmmResult` 等)。
3. **Phase 3: User Stories (US1)**: 實作存活率(Ps)核心計算與對應單元測試。
4. **Phase 4: User Stories (US2)**: 實作非線性分箱演算法、`CalculateRmm` 方法及其單元測試。
5. **Phase 5: User Stories (US3)**: 實作 `CalculateTraumaPerformance` (W/M/Z Score 等計算) 及其單元測試。
6. **Phase 6: User Stories (US4)**: 逐一實作 1~13 號 SEQIC 指標功能與其對應之單元測試。
7. **Phase 7: Polish**: 更新 API 對照表 `README.md`、R 套件 `NEWS.md`，並建立範例主控台專案驗證 DLL 引用。

## 待解決問題 (Unresolved Questions)
無。
