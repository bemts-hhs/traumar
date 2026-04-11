# 資料模型 (Data Models)

## 命名空間: Traumar.Models

### Enums
- `InjuryType`: `Blunt` (鈍傷), `Penetrating` (穿刺傷)

### DTOs
- `PatientInput`: 純輸入。
  - `InjuryType` (InjuryType)
  - `Age` (int)
  - `Rts` (double)
  - `Iss` (int)

- `PatientRecord`: 計算所需紀錄。
  - `PatientInput` 屬性
  - `Ps` (double) - 存活率
  - `Outcome` (int) - 0或1

- `RmmResult`: RMM計算結果。
  - `PopulationRMM`, `PopulationRMM_LL`, `PopulationRMM_UL`, `PopulationCI` (double)
  - `BootstrapRMM`, `BootstrapRMM_LL`, `BootstrapRMM_UL`, `BootstrapCI` (double)

- `TraumaPerformanceResult`: 績效評分。
  - `WScore`, `MScore`, `ZScore` (double)

- `IndicatorNInput` / `IndicatorNResult` (N=1~13): SEQIC 13項指標專用模型 (欄位依據R源碼逐一對應實作)。

## 命名空間: Traumar.Core (Internal)

- `BinStatistics`: 非線性分箱統計 (供內部 RMM 或分箱計算使用)。
