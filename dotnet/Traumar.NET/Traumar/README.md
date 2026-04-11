# Traumar.NET

這是 `traumar` R 套件的官方 .NET (C#) 移植版本。本專案以 .NET Framework 4.7.2 為目標框架，將原本 R 版本中多數的核心創傷表現指標與機率預測邏輯封裝成強型別的 NuGet 類別庫，供外部直接參考使用。

## 特色
- **強型別 (Strongly Typed)**: 相較於 R 語言採用資料框 (Data Frame) 逐欄映射，.NET 版本所有模型與列舉（如 `TraumaLevel`, `TraumaType`, `YesNo`）皆經過嚴格的型別約束 (Enums)。
- **無狀態與高效計算**: 輸入資料集直接傳入靜態方法計算，並直接回傳結果，無額外的複雜依賴。
- **100% 原始 R 邏輯相容**: 所有計算邏輯包括非線性分箱演算法 (Non-linear bins algorithm) 與拔靴法 (Bootstrap CI) 皆完整移植。

## API 對照表 (R -> .NET)

| R 函數名稱 | .NET 類別與方法 | 說明 |
| --- | --- | --- |
| `probability_of_survival()` | `SurvivalCalculator.ProbabilityOfSurvival(PatientInput)` | 計算單筆病患的存活機率 (TRISS) |
| `non_linear_bins()` | *(Internal)* `NonLinearBinsCalculator.TryCalculateBins(...)` | RMM 的內部非線性分箱邏輯 |
| `relative_mortality()` | `RmmCalculator.CalculateRmm(PatientRecord[])` | 計算 Bootstrap 與 Population RMM |
| `trauma_performance()` | `PerformanceCalculator.CalculateTraumaPerformance(...)` | 計算 W-Score, M-Score, 及其對應的 Z-Score |
| `seqic_indicator_1()` | `Indicators1to4.CalculateIndicator1(...)` | SEQIC 1a-1f 指標 (創傷小組反應時間) |
| `seqic_indicator_2()` | `Indicators1to4.CalculateIndicator2(...)` | SEQIC 2 指標 (缺漏事故時間) |
| `seqic_indicator_3()` | `Indicators1to4.CalculateIndicator3(...)` | SEQIC 3 指標 (存活機率記錄完成度) |
| `seqic_indicator_4()` | `Indicators1to4.CalculateIndicator4(...)` | SEQIC 4a-4b 指標 (死亡未解剖、長期滯院) |
| `seqic_indicator_5()` | `Indicators5to8.CalculateIndicator5(...)` | SEQIC 5a-5d 指標 (毒物與酒精篩檢) |
| `seqic_indicator_6()` | `Indicators5to8.CalculateIndicator6(...)` | SEQIC 6 指標 (低GCS延遲到院) |
| `seqic_indicator_7()` | `Indicators5to8.CalculateIndicator7(...)` | SEQIC 7 指標 (延遲獲得決定性照護) |
| `seqic_indicator_8()` | `Indicators5to8.CalculateIndicator8(...)` | SEQIC 8 指標 (不同風險群之存活率) |
| `seqic_indicator_9()` | `Indicators9to13.CalculateIndicator9(...)` | SEQIC 9a-9f 指標 (急診處置延遲) |
| `seqic_indicator_10()` | `Indicators9to13.CalculateIndicator10(...)` | SEQIC 10a-10c 指標 (過度與不足分流) |
| `seqic_indicator_11()` | `Indicators9to13.CalculateIndicator11(...)` | SEQIC 11 指標 (輕傷過度分流) |
| `seqic_indicator_12()` | `Indicators9to13.CalculateIndicator12(...)` | SEQIC 12 指標 (登錄及時性) |
| `seqic_indicator_13()` | `Indicators9to13.CalculateIndicator13(...)` | SEQIC 13 指標 (登錄資料有效性) |

## 快速使用範例

```csharp
using Traumar.Models;
using Traumar.Core;
using System.Collections.Generic;

// 1. 建立個案資料 (計算存活機率)
var input = new PatientInput { Age = 45, InjuryType = InjuryType.Blunt, Iss = 16, Rts = 7.84 };
double ps = SurvivalCalculator.ProbabilityOfSurvival(input);

// 2. 準備集合資料 (計算績效與 RMM)
var dataset = new[] {
    new PatientRecord { Age = 45, InjuryType = InjuryType.Blunt, Iss = 16, Rts = 7.84, Outcome = 1, Ps = 0.95 },
    new PatientRecord { Age = 55, InjuryType = InjuryType.Penetrating, Iss = 25, Rts = 5.0, Outcome = 0, Ps = 0.40 }
};

var rmmResult = RmmCalculator.CalculateRmm(dataset);
var performanceResult = PerformanceCalculator.CalculateTraumaPerformance(dataset);
```
