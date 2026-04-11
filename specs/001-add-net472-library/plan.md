# 實作計畫

## 技術棧 (Tech Stack)
- 目標框架: .NET Framework 4.7.2
- 測試框架: xUnit + .NET 4.7.2
- 第三方套件: `MathNet.Numerics` (提供進階數學與統計運算)

## 架構 (Architecture)
- **Traumar.Models**: 所有公開DTOs與Enums。
- **Traumar.Core**: 核心計算 (`ProbabilityOfSurvival`, `CalculateRmm`, `CalculateTraumaPerformance` 及 internal 分箱 `NonLinearBins`)。
- **Traumar.Seqic**: 13項 SEQIC 指標計算方法。

## 階段 (Phases)
1. **專案建立**: 初始化 .NET 4.7.2 Library (`traumar.net`) 與 xUnit 測試專案，引入 `MathNet.Numerics`。
2. **核心模型**: 定義 `Traumar.Models` 中的核心實體。
3. **基礎計算 (US1/US2/US3)**: 實作存活率(Ps)、非線性分箱、RMM、W/M/Z Score，以及對應單元測試。
4. **SEQIC 指標 (US4)**: 逐一實作 1~13 號 SEQIC 指標與對應 DTOs，以及對應單元測試。
5. **文件更新**: 建立包含 API 對照表的 `README.md`。

## 待解決問題 (Unresolved Questions)
無。
