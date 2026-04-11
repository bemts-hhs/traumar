# 功能規格：新增 .NET Framework 4.7.2 Library 專案

**功能分支**：`001-add-net472-library`
**建立日期**：2026-04-11
**狀態**：已澄清
**輸入**：「新增一個 .net framework 4.7.2 的 library 專案，將目前的程式改為 .net framework 4.7.2 的版本」

---

## 使用場景與測試 *(mandatory)*

### 使用者故事 1 — 計算個別病患存活率 (Priority: P1)

**描述**：  
臨床資料分析師擁有單一或批量病患的傷傷類型（鈍傷/穿刺傷）、年齡、修正創傷評分（RTS）、傷害嚴重度評分（ISS）資料，他需要呼叫 .NET Library 的方法，取得每位病患的 TRISS 存活機率（Ps），以便進一步計算績效指標。

**為何此優先**：Ps 是後續所有指標（W Score、RMM）的基礎輸入，必須先可用。

**獨立測試**：傳入至少 2 組已知輸入值（鈍傷與穿刺傷各一），驗證輸出 Ps 值與 R 語言參考實作的計算結果誤差在 ±0.0001 以內。

**驗收情境**：

1. **Given** 一組鈍傷病患資料（RTS=7.84、ISS=10、年齡=30），**When** 呼叫 `ProbabilityOfSurvival` 方法，**Then** 回傳 0 到 1 之間的浮點數，且與 R 套件參考值誤差 < 0.0001。
2. **Given** 一組穿刺傷病患資料（RTS=6.90、ISS=25、年齡=60），**When** 呼叫同一方法，**Then** 回傳合理的存活機率，與參考值誤差 < 0.0001。
3. **Given** `InjuryType` 採強型別 `enum { Blunt, Penetrating }`，傳入不合法值（如 `"Burn"`）為**編譯期錯誤**，不需要執行期驗證；此驗收條件由型別系統靜態保證。
4. **Given** 輸入年齡為負數或 ISS 超過 75，**When** 呼叫方法，**Then** 拋出明確的範圍例外。

---

### 使用者故事 2 — 計算相對死亡率指標（RMM） (Priority: P2)

**描述**：  
創傷中心品質改善人員擁有一批病患的 Ps 值與實際存活結果（0/1），他需要取得相對死亡率指標（RMM）及其 95% 信賴區間，以判斷創傷中心的表現是高於或低於國家基準。

**為何此優先**：RMM 為套件的核心輸出指標，依賴 US1 的 Ps 計算，但可獨立測試。

**獨立測試**：傳入已知分佈的 Ps 和 outcome 資料，驗證 RMM 結果落在 [-1, 1] 區間，且非線性分箱邏輯與 R 版本一致。

**驗收情境**：

1. **Given** 包含 Ps 欄與 outcome 欄（0/1）的病患資料集，**When** 呼叫 `CalculateRmm` 方法，**Then** 回傳包含以下屬性的 `RmmResult`：Population CI 組 —— `PopulationRMM`、`PopulationRMM_LL`、`PopulationRMM_UL`、`PopulationCI`；Bootstrap CI 組 —— `BootstrapRMM`、`BootstrapRMM_LL`、`BootstrapRMM_UL`、`BootstrapCI`。所有 RMM 值域皆截斷至 [-1, 1]。
2. **Given** 所有病患 outcome=1（全部存活），**When** 計算 RMM，**Then** RMM > 0（優於基準）。
3. **Given** outcome 欄包含除 0/1 以外的值，**When** 呼叫方法，**Then** 拋出明確的輸入驗證例外。

---

### 使用者故事 3 — W Score 與 M Score 計算 (Priority: P3)

**描述**：  
品質改善人員需要計算 W Score（每 100 名病患中優於基準的存活人數）與 M Score（資料分配與 MTOS 分佈的相似程度），以補充 RMM 指標的解讀。

**為何此優先**：提供完整指標套件，但對 Library 核心功能非阻塞性需求。

**獨立測試**：傳入已知 Ps 和 outcome 資料，驗證 W Score 為數值型結果，且 M Score 的計算邏輯與 R 版本 `trauma_performance()` 在相同輸入下的輸出結果一致（不自訂範圍限制）。

**驗收情境**：

1. **Given** 有效的 Ps 和 outcome 資料集（筆數 ≥ `minSampleSize`），**When** 呼叫 `CalculateTraumaPerformance` 方法，**Then** 回傳包含 `WScore`、`MScore`、`ZScore` 的結果物件。
2. **Given** 資料筆數低於 `minSampleSize`（預設 10），**When** 呼叫 `CalculateTraumaPerformance` 或 `CalculateRmm`，**Then** 立即拋出 `ArgumentException`，訊息說明實際筆數與門檻值（例：「資料量不足：需要至少 {minSampleSize} 筆，目前僅有 {actual} 筆」）。
3. **Given** 呼叫端將 `minSampleSize` 設為 1，**When** 傳入 3 筆資料，**Then** 方法正常執行（門檻可由呼叫端降低）。

---

### 使用者故事 4 — SEQIC 創傷系統品質指標計算 (Priority: P4)

**描述**：  
創傷系統品質管理分析師擁有創傷登記資料（反應時間、死亡紀錄、血液酒精濃度、分流結果、出院時間等），需要計算 13 項 SEQIC 標準化品質指標，以評估創傷中心或系統在各品質維度的表現，作為持續品質改善（CQI）的依據。每個指標方法接受**各自獨立的強型別輸入 DTO**（如 `Indicator1Input`），以確保型別安全並使 IDE 自動補全可用。

**為何此優先**：SEQIC 指標是 `traumar` 套件的重要功能群組，但與核心計算（Ps、RMM、W/M/Z Score）相互獨立，可在 US1~US3 完成後再實作。由於涵蓋 13 個功能，預估為最高工作量的使用者故事。

**獨立測試**：逐一傳入對應測試資料至各 SEQIC 指標方法，驗證每個指標的輸出值與 R 版本 `seqic_indicator_*()` 在相同輸入下的結果一致。

**驗收情境**：

1. **Given** 包含必要欄位的創傷登記資料（筆數 ≥ `minSampleSize`），**When** 呼叫對應指標的計算方法，**Then** 回傳與 R 版本 `seqic_indicator_*()` 在相同輸入下一致的強型別結果 DTO（`Indicator1Result`…`Indicator13Result`）（數值型指標誤差 < 0.0001；分類型指標完全吻合）。
2. **Given** 強型別 DTO 中某必填屬性（非 nullable 型別）在呼叫端被賦予非法值或超出值域，**When** 呼叫對應方法，**Then** 拋出明確的 `ArgumentException` 或 `ArgumentOutOfRangeException`，說明屬性名稱與合法值域。（「缺少欄位」與「資料型別不符」均為編譯期錯誤，由強型別 DTO 保證，不需執行期測試。）

> ⚠️ **範圍說明**：本 US 涵蓋 `seqic_indicator_1` 至 `seqic_indicator_13` 共 13 個品質指標。計畫階段應逐一評估各指標的輸入欄位與輸出格式複雜度。

---

### 邊界情境

- 傳入空集合（零筆資料）時，所有方法應拋出 `ArgumentException`。
- Ps 值為 0 或 1 的極端邊界情況：計算 Z-Score 時分母 `sqrt(sum(Ps*(1-Ps))) = 0`，.NET 版本必須主動偵測並拋出 `InvalidOperationException`（R 原始邏輯會靜默回傳 NaN，為 **BUG-002**）。
- 所有 outcome 皆為同一值（全存活或全死亡）時：R 版本**不拋出例外**，.NET 版本遠循相同行為，直接回傳數學結果（全存活 → RMM 譮为 1.0；全死亡 → RMM 譮为 -1.0）。
- 非常小的資料集（<10 筆）對非線性分箱的影響。
- Bootstrap 抒樣子集的 Ps 分佈過於集中導致分箱失敗時，.NET 版本應跨過該次抒樣並繼續（R 版本對此**沒有容錯機制**，為 **BUG-003**）。

---

## 需求 *(mandatory)*

### 功能需求

- **FR-001**：系統必須提供一個 .NET Framework 4.7.2 Class Library（`.dll`），可被其他 .NET 專案引用。
- **FR-002**：Library 必須提供 `ProbabilityOfSurvival` 方法，根據 TRISS 方法論（Boyd et al., 1987）計算病患存活機率。
- **FR-003**：Library 必須提供非線性分箱（Nonlinear Bins）演算法，實作 Napoli et al. (2017) 的分箱邏輯。
- **FR-004**：Library 必須提供 `CalculateRmm` 方法，完整計算相對死亡率指標（RMM）、Population CI 及 Bootstrap CI（95% 信賴區間）。Bootstrap CI 採重抽樣（Bootstrap sampling）實作，以種子值控制可重現性。方法必須接受以下參數：`minSampleSize`（int，預設 10，低於門檻拋出 `ArgumentException`）；`nSamples`（int，預設 100，對應 R 版本 `n_samples`）；`seed`（int?，可為 null，對應 R 版本 `seed`）。輸出欄位命名對應 R 輸出：Population CI 組 —— `population_RMM`、`population_RMM_LL`、`population_RMM_UL`、`population_CI`；Bootstrap CI 組 —— `bootstrap_RMM`、`bootstrap_RMM_LL`、`bootstrap_RMM_UL`、`bootstrap_CI`。若 Bootstrap 某次抑樣失敗（Ps 分佈過於集中），.NET 版本應跨過該次並繼續（對應 R **BUG-003** 的修待）。
- **FR-005**：Library 必須提供 `CalculateTraumaPerformance` 方法，計算 W Score、M Score 與 Z Score。方法必須接受 `minSampleSize`（int，預設 10）參數，低於門檻即拋出 `ArgumentException`。此外必須主動防範 Z-Score 除以零場景：當 `sum(Ps*(1-Ps)) = 0`（即全部 Ps = 0 或 1）時必須拋出 `InvalidOperationException`，不得靜默回傳 NaN（特定對應 R 版本 **BUG-002** 的修待）。
- **FR-006**：所有公開方法必須包含輸入驗證，對非法輸入拋出具說明性的例外（`ArgumentException` 或 `ArgumentOutOfRangeException`）。例外訊息必須包含實際值與合法值域。
- **FR-007**：Library 的所有計算結果必須與 R 套件 `traumar` 參考實作保持一致：數值型輸出誤差 < 0.0001；非數值型輸出（如 M Score 分佈比例、SEQIC 分類指標）必須依照 R 版本邏輯實作，不得自訂輸出範圍或格式。
- **FR-008**：Library 可引用第三方 NuGet 套件，但必須限於有積極維護的 .NET Framework 4.7.2 相容套件。每個新增依賴須在 `plan.md` 中列出名稱與理由。
- **FR-009**：Library 必須提供對應 13 項 SEQIC 品質指標的計算方法（對應 R 版本 `seqic_indicator_1()` 至 `seqic_indicator_13()`）。**每個指標方法接受各自獨立的強型別輸入 DTO（`Indicator1Input`…`Indicator13Input`），並回傳對應的強型別結果 DTO（`Indicator1Result`…`Indicator13Result`）**；輸入 DTO 的必填屬性使用非 nullable 型別，選填屬性使用 nullable 型別；輸出 DTO 的屬性名稱對應 R tibble 輸出欄位，數值型屬性使用 `double`，分類型屬性使用 `string`。**所有指標方法必須接受 `minSampleSize`（int，預設 10）參數**，低於門檻拋出 `ArgumentException`。各指標的計算邏輯與輸出結果必須與 R 套件 `traumar` 的對應函數在相同輸入下保持一致（FR-007 精度要求同樣適用）。
- **FR-010**：Library 目錄下必須包含 `README.md`，以表格形式說明所有 .NET 公開方法與對應 R 函數的映射關係，內容包含：.NET 方法名稱、對應 R 函數名稱、主要參數對照（含型別差異說明）及簡短範例用法。至少須涵蓋 `ProbabilityOfSurvival`、`CalculateRmm`、`CalculateTraumaPerformance`，以及全部 13 個 SEQIC 指標方法。

### 核心實體

- **PatientInput**：純輸入 DTO，包含 `InjuryType`（`enum InjuryType { Blunt, Penetrating }`）、`Age`（`int`）、`Rts`（`double`）、`Iss`（`int`）。不含計算結果欄位，確保語意明確。
- **PatientRecord**：包含 `PatientInput` 的全部屬性，加上 `Ps`（`double`，由 `ProbabilityOfSurvival` 計算填入）與 `Outcome`（`int`，0 或 1）。`ProbabilityOfSurvival` 接受 `PatientInput` 並回傳 `PatientRecord`，避免混淆「未計算的 Ps=0」與「確定死亡的 Ps=0」。
- **TraumaPerformanceResult**：W Score、M Score、Z Score 的結果容器。
- **RmmResult**：RMM 及信賴區間的結果容器。屬性名稱對應 R `rmm()` 輸出欄位：Population CI 組 —— `PopulationRMM`、`PopulationRMM_LL`、`PopulationRMM_UL`、`PopulationCI`；Bootstrap CI 組 —— `BootstrapRMM`、`BootstrapRMM_LL`、`BootstrapRMM_UL`、`BootstrapCI`（僅當 bootstrapCI = true 時填入）。
- **BinStatistics**：非線性分箱的單一分箱統計資料。
- **IndicatorNInput**（N = 1~13）：SEQIC 各指標各自獨立的強型別輸入 DTO，屬性命名對應 R 函數的參數欄位；必填屬性為非 nullable 型別，選填屬性為 nullable 型別。
- **IndicatorNResult**（N = 1~13）：SEQIC 各指標各自獨立的強型別輸出 DTO，屬性對應 R tibble 輸出欄位名稱；數值型屬性使用 `double`，分類型屬性使用 `string`。

---

## 成功標準 *(mandatory)*

### 可量測指標

- **SC-001**：`ProbabilityOfSurvival` 的計算結果與 R `traumar` 套件參考值的誤差絕對值 ≤ 0.0001（100% 的測試案例通過）。
- **SC-002**：`CalculateRmm` 輸出的 RMM 值始終落在 [-1, 1] 範圍內。
- **SC-003**：所有公開 API 方法對非法輸入均能在呼叫當下拋出例外，並附有清楚的錯誤描述（不回傳靜默的 NaN 或預設值）。
- **SC-004**：Library 的建置產物為可直接被 .NET Framework 4.7.2 或以上版本引用的 `.dll` 檔案，無需任何額外安裝步驟。
- **SC-005**：核心計算方法的單元測試覆蓋率達到至少 80%（含邊界情境）。測試專案使用 **xUnit + .NET Framework 4.7.2** 目標框架，確保在與 Library 完全相同的執行環境下進行驗證。

---

## 假設

- **A1**：R 套件目前的係數（TRISS Blunt/Penetrating coefficients）即為移植目標的正確值，不另行驗證文獻。
- **A2**：Bootstrap CI 於 .NET 版本中完整實作（對應 R 版本 `bootstrap_ci = TRUE`），以種子值（`int? seed`）控制可重現性；使用者可傳入 `null` 表示不設定種子。
- **A3**：Library 不需要提供資料視覺化（ggplot2 對應）功能，僅提供數值計算。
- **A4**：Library 專案位於**同一倉庫**的 `dotnet/Traumar.NET/` 子目錄。`.Rbuildignore` 必須排除 `^dotnet/` 以避免干擾 `R CMD check`。
- **A5**：單元測試專案使用 **xUnit + .NET Framework 4.7.2** 目標框架；不支援多目標框架建置。
- **A6 （Outcome 編碼規範）**：全部 .NET 方法對 outcome 的編碼統一採用 `non_linear_bins.R` 的規範：**`outcome = 1` 代表存活（alive）、`outcome = 0` 代表死亡（dead）**。R 套件的 `trauma_performance.r` 使用相反編碼（BUG-001），.NET 版本已修待此一致性問題，因此 `CalculateTraumaPerformance` 的 W-Score 與 Z-Score 公式需處對應調整，對應結果不得与未修待的 R 版本比對。

---

## 參考文獻

- Boyd CR, Tolson MA, Copes WS. (1987). Evaluating trauma care: the TRISS method. *J Trauma*, 27(4):370-8. PMID: 3106646.
- Napoli NJ et al. (2017). Relative mortality analysis. *IISE Transactions on Healthcare Systems Engineering*, 7(3):181–191. DOI: 10.1080/24725579.2017.1325948.
- Flora JD. (1978). A method for comparing survival of burn patients. *J Trauma*. DOI: 10.1097/00005373-197810000-00003.

---

## 澄清記錄

| # | 問題 | 使用者決策 | 影響項目 |
|---|------|-----------|----------|
| Q1 | Bootstrap CI 是否完整實作？ | **完整實作**，並開放允許第三方 NuGet 套件 | FR-008 修改、FR-004 補充 Bootstrap 要求、A2 更新 |
| Q2 | .NET 專案與 R 套件的共存方式？ | **同一倉庫**，放在 `dotnet/Traumar.NET/` 子目錄 | A4 更新，需修改 `.Rbuildignore` |
| Q3 | 資料量不足時的行為？ | **A+C**：接受 `minSampleSize` 參數（預設 10），低於門檻拋出 `ArgumentException` | FR-004、FR-005 加入參數定義，US3 驗收情境更新 |
| Q4 | SEQIC 指標函數是否在範圍內？ | **全部包含**：13 個 SEQIC 指標全數移植 | 新增 US4（P4）、FR-009；工作量顯著增加 |
| Q5 | Bootstrap 抽樣次數預設值？ | **100 次**（對應 R 版本 `n_samples = 100`），可由 `nSamples` 參數覆蓋 | FR-004 補充 `nSamples` 參數定義 |
| Q6 | M Score 有效範圍？ | **不限制**，以 R 版本 `trauma_performance()` 輸出邏輯為準 | US3 獨立測試描述更新、FR-007 強調邏輯一致性要求 |
| Q7 | SEQIC 13 個指標的輸入資料模型？ | **各指標獨立強型別 DTO**（`Indicator1Input`…`Indicator13Input`） | FR-009 補充 DTO 定義、US4 AC2 改為「必填屬性為非 nullable」、新增核心實體 `IndicatorNInput` |
| Q8 | `PatientRecord` 傷傷類型的資料型別？ | **`enum InjuryType { Blunt, Penetrating }`** | US1 AC3 改為編譯期保證、`PatientRecord` 實體描述更新、FR-006 執行期驗證不涵蓋傷傷類型 |
| Q9 | 單元測試專案的框架與目標框架？ | **xUnit + .NET Framework 4.7.2** | SC-005 補充測試框架說明、新增 A5 |
| Q10 | SEQIC 指標的回傳型別結構？ | **各指標獨立強型別結果 DTO**（`Indicator1Result`…`Indicator13Result`） | FR-009 補充回傳型別、US4 AC1 更新、新增核心實體 `IndicatorNResult` |
| Q11 | `PatientRecord` 是否分離輸入與輸出？ | **分離**：`PatientInput`（純輸入）+ `PatientRecord`（含 Ps 與 Outcome） | 核心實體新增 `PatientInput`、`ProbabilityOfSurvival` 簽名改為接受 `PatientInput` 回傳 `PatientRecord` |
| Q13 | `RmmResult` Bootstrap CI 屬性命名規則？ | **對應 R 輸出欄位**：`BootstrapRMM`、`BootstrapRMM_LL`、`BootstrapRMM_UL`、`BootstrapCI` | RmmResult 實體註明完整屬性清單、US2 AC1 补充 Bootstrap CI 屬性 |
| Q14 | 全同 outcome 的邊界行為？ | **與 R 版本一致**：不拋出例外，回傳數學結果；並發現 R 源碼 3 個 Bug（詳見 A6） | 邊界情境路徑更新、FR-005 加入 Z-Score 除以零保護、FR-004 加入 Bootstrap 容錯機制、新增 A6 |
| Q15 | SEQIC 指標方法是否需要 `minSampleSize`？ | **是**，預設 10，低於門檻拋出 `ArgumentException` | FR-009 补充 `minSampleSize` 參數定義 |
