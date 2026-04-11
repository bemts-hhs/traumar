# 資料模型 (Data Models)

> **唯一來源 (Single Source of Truth)**：所有模型定義以本文件為準；`spec.md` 核心實體區為交叉引用摘要。

---

## 命名空間: Traumar.Models

### Enums

- `InjuryType`: `Blunt` (鈍傷), `Penetrating` (穿刺傷)

---

### 核心 DTOs

#### `PatientInput` — 純輸入

| 屬性 | C# 型別 | 說明 |
|------|---------|------|
| `InjuryType` | `InjuryType` | 傷害類型（鈍傷/穿刺傷） |
| `Age` | `int` | 年齡（非負整數） |
| `Rts` | `double` | 修正創傷評分 (Revised Trauma Score) |
| `Iss` | `int` | 傷害嚴重度評分（0–75） |

#### `PatientRecord` — 含計算結果

| 屬性 | C# 型別 | 說明 |
|------|---------|------|
| `InjuryType` | `InjuryType` | 繼承自 PatientInput |
| `Age` | `int` | 繼承自 PatientInput |
| `Rts` | `double` | 繼承自 PatientInput |
| `Iss` | `int` | 繼承自 PatientInput |
| `Ps` | `double` | 存活機率（由 ProbabilityOfSurvival 填入） |
| `Outcome` | `int` | 實際結果：1=存活，0=死亡（A6 規範） |

#### `RmmResult` — RMM 計算結果

| 屬性 | C# 型別 | 說明 |
|------|---------|------|
| `PopulationRMM` | `double` | Population RMM 點估計 |
| `PopulationRMM_LL` | `double` | Population CI 下限 |
| `PopulationRMM_UL` | `double` | Population CI 上限 |
| `PopulationCI` | `double` | Population CI 寬度 |
| `BootstrapRMM` | `double` | Bootstrap RMM 點估計 |
| `BootstrapRMM_LL` | `double` | Bootstrap CI 下限 |
| `BootstrapRMM_UL` | `double` | Bootstrap CI 上限 |
| `BootstrapCI` | `double` | Bootstrap CI 寬度 |

> 以上 8 個屬性**永遠非 nullable**（Bootstrap CI 永遠執行）。

#### `TraumaPerformanceResult` — 績效評分

| 屬性 | C# 型別 | 說明 |
|------|---------|------|
| `WScore` | `double` | W Score（每 100 名病患超基準存活人數） |
| `MScore` | `double[]` | M Score（各 bin 比例向量，長度與 R 版本一致） |
| `ZScore` | `double` | Z Score |

---

### SEQIC 指標 DTOs（Traumar.Models.Seqic）

> 欄位命名以 R 函數參數名稱為準（對應 `seqic_indicator_N()` 函數具名參數）。
> 共用欄位（`level`, `unique_incident_id`, `included_levels`, `groups`, `calculate_ci`）各指標皆有；下表僅列各指標**特有**欄位。
> **架構決策 (1A/2A)**：
> - **輸入 Enums**：下表中所有分類型 `string` 欄位（例如 "Yes/No", "High/Moderate/Low", "Level 1/2"）在實作時需統一轉化為專屬 Enum（如 `RiskGroupEnum`），以獲取最佳型別安全。
> - **輸出 Nested DTOs**：針對含有多組數據（1a-1f）或子表（Overall, RiskGroup）的指標，其 `Result` 類別會採用巢狀類別 (Nested Class) 封裝集合以保持物件結構層次。

#### 指標 1 — Indicator1Input / Indicator1Result

**功能**：Trauma Team Response（反應時間，含 1a–1f 子指標）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `TraumaTeamActivationLevel` | `string` | `trauma_team_activation_level` | 啟動層級（Level 1/2） |
| `TraumaTeamPhysicianServiceType` | `string` | `trauma_team_physician_service_type` | 醫療提供者類型 |
| `ResponseTime` | `double?` | `response_time` | 反應時間（分鐘，可為 null） |
| `TraumaTeamActivationProvider` | `string` | `trauma_team_activation_provider` | 啟動提供者名稱 |

**Result 包含**：Numerator/Denominator/Rate 各 1a–1f 子指標（共 6 組）。

---

#### 指標 2 — Indicator2Input / Indicator2Result

**功能**：Missing Incident Time（缺漏事故時間率）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `IncidentTime` | `DateTime?` | `incident_time` | 事故發生時間（可為 null） |

---

#### 指標 3 — Indicator3Input / Indicator3Result

**功能**：Presence of Probability of Survival（Ps 記錄率）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `TraumaType` | `string` | `trauma_type` | 創傷類型（「Burn」者排除） |
| `ProbabilityOfSurvival` | `double?` | `probability_of_survival` | 存活機率（0–1，可為 null） |

---

#### 指標 4 — Indicator4Input / Indicator4Result

**功能**：Autopsy & Long LOS Without Autopsy（驗屍率 4a；長住院無驗屍率 4b）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `EdDisposition` | `string` | `ed_disposition` | 急診出口（「Deceased/Expired」=死亡） |
| `EdLos` | `double?` | `ed_LOS` | 急診住院時間（分鐘） |
| `HospitalDisposition` | `string` | `hospital_disposition` | 住院出口 |
| `HospitalLos` | `double?` | `hospital_LOS` | 住院時間（分鐘） |
| `Autopsy` | `string?` | `autopsy` | 驗屍（「Yes」/null） |

---

#### 指標 5 — Indicator5Input / Indicator5Result

**功能**：Alcohol & Drug Screening（酒精/藥物篩檢率）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `BloodAlcoholContent` | `double?` | `blood_alcohol_content` | 血液酒精濃度（≥0，null 表示未測） |
| `DrugScreen` | `string?` | `drug_screen` | 藥物篩檢結果（NTDB 值域） |

**Result 包含**：5a（BAC 測試率）、5b（BAC>0 率）、5c（藥物測試率）、5d（藥物陽性率）。

---

#### 指標 6 — Indicator6Input / Indicator6Result

**功能**：Delayed Arrival After Low GCS（低 GCS 延遲到院率）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `TransferOutIndicator` | `string` | `transfer_out_indicator` | 是否轉出（「No」/FALSE） |
| `ReceivingIndicator` | `string` | `receiving_indicator` | 是否轉入接收（「Yes」/TRUE） |
| `LowGcsIndicator` | `bool` | `low_GCS_indicator` | GCS < 9 旗標 |
| `TimeFromInjuryToArrival` | `double` | `time_from_injury_to_arrival` | 受傷到到院時間（分鐘） |

---

#### 指標 7 — Indicator7Input / Indicator7Result

**功能**：Delayed Arrival to Definitive Care（延遲到達確定照護）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `TransferOutIndicator` | `string` | `transfer_out_indicator` | 是否轉出（「No」/FALSE） |
| `TimeFromInjuryToArrival` | `double` | `time_from_injury_to_arrival` | 受傷到到院時間（分鐘） |

---

#### 指標 8 — Indicator8Input / Indicator8Result

**功能**：Survival by Risk Group（依風險群分層之存活率）

**Result**：回傳包含 `Overall` 與 `RiskGroup` 兩個子表的結構（對應 R 版本 list 輸出），各包含存活率及選用信賴區間。

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `MortalityIndicator` | `string` | `mortality_indicator` | 死亡指標（「Yes」/TRUE=死亡） |
| `RiskGroup` | `string` | `risk_group` | 風險群（High/Moderate/Low） |

---

#### 指標 9 — Indicator9Input / Indicator9Result

**功能**：ED Transfer Timeliness（急診轉院及時性，含 9a–9f 子指標）

**Result**：回傳包含 `Overall`, `Activations`, `Risk`, `ActivationsRisk` 四個子表的結構（對應 R 版本 list 輸出）。

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `TransferOutIndicator` | `string` | `transfer_out_indicator` | 是否轉出 |
| `TransportMethod` | `string` | `transport_method` | 交通方式（排除私家車等） |
| `TraumaTeamActivated` | `string` | `trauma_team_activated` | 是否啟動創傷小組 |
| `RiskGroup` | `string` | `risk_group` | 風險群 |
| `EdLos` | `double` | `ed_LOS` | 急診總住院時間（分鐘） |
| `EdDecisionLos` | `double` | `ed_decision_LOS` | 到決定轉院的時間（分鐘） |
| `EdDecisionDischargeLos` | `double` | `ed_decision_discharge_LOS` | 決定轉院到實際離院的時間（分鐘） |

---

#### 指標 10 — Indicator10Input / Indicator10Result

**功能**：Triage Appropriateness（分流適切性：10a 過少分流、10b 過度分流、10c Peng & Xiang 修正）

**Result**：回傳包含 `Seqic10` 及 `Diagnostics`（2×2 矩陣診斷統計）兩個子表的結構。

| 欄位名稱 | C# 型別 | 必填 | R 參數對應 | 說明 |
|---------|---------|------|-----------|------|
| `TransferOutIndicator` | `string` | ✓ | `transfer_out_indicator` | 是否轉出（No/FALSE） |
| `TraumaTeamActivationLevel` | `string` | ✓ | `trauma_team_activation_level` | 啟動層級 |
| `Iss` | `int?` | 二選一 | `iss` | ISS（0–75，與 Nfti 二選一） |
| `Nfti` | `string?` | 二選一 | `nfti` | NFTI 分類（Positive/Negative，與 Iss 二選一） |

---

#### 指標 11 — Indicator11Input / Indicator11Result

**功能**：Overtriage for Minor Trauma（輕傷過度分流率：ISS < 9 且急診 < 24hr）

| 欄位名稱 | C# 型別 | R 參數對應 | 說明 |
|---------|---------|-----------|------|
| `TransferOutIndicator` | `string` | `transfer_out_indicator` | 是否轉出（No/FALSE） |
| `ReceivingIndicator` | `string` | `receiving_indicator` | 是否轉入接收（Yes/TRUE） |
| `Iss` | `int` | `iss` | ISS（0–75） |
| `EdLos` | `double` | `ed_LOS` | 急診住院時間（分鐘，< 1440 = 24hr） |

---

#### 指標 12 — Indicator12Input / Indicator12Result

**功能**：Data Entry Timeliness（登錄及時性：出院後 N 天內完成登錄比率）

| 欄位名稱 | C# 型別 | 必填 | R 參數對應 | 說明 |
|---------|---------|------|-----------|------|
| `FacilityId` | `string` | ✓ | `facility_id` | 醫院識別碼 |
| `ExcludeFacilityList` | `string[]?` | 選填 | `exclude_facility_list` | 排除醫院清單 |
| `DataEntryTime` | `double` | ✓ | `data_entry_time` | 出院後至登錄的天數 |
| `DataEntryStandard` | `int` | ✓ | `data_entry_standard` | 可接受天數門檻（預設 60） |

---

#### 指標 13 — Indicator13Input / Indicator13Result

**功能**：Registry Record Validity（登錄資料有效性：validity_score ≥ 門檻的比率）

| 欄位名稱 | C# 型別 | 必填 | R 參數對應 | 說明 |
|---------|---------|------|-----------|------|
| `ValidityScore` | `double` | ✓ | `validity_score` | 記錄有效性分數（0–100） |
| `ValidityThreshold` | `double` | ✓ | `validity_threshold` | 有效性門檻（預設 85） |

---

## 命名空間: Traumar.Core (Internal)

- `BinStatistics`: 非線性分箱統計（供內部 RMM 或分箱計算使用），**不對外公開**。

  | 屬性 | C# 型別 | 說明 |
  |------|---------|------|
  | `BinMin` | `double` | 分箱下界 |
  | `BinMax` | `double` | 分箱上界 |
  | `ObservedSurvivors` | `int` | 實際存活數 |
  | `ExpectedSurvivors` | `double` | 預期存活數（Σ Ps） |
  | `PatientCount` | `int` | 分箱內病患總數 |
