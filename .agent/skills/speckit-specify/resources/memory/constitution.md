# Speckit 憲法（Constitution）

**版本**：1.0.0  
**建立日期**：2026-04-11  
**專案**：traumar — 創傷系統績效指標計算 R 套件  

---

## 1. 專案識別

| 項目 | 內容 |
|------|------|
| **套件名稱** | traumar |
| **語言／版本** | R（≥ 4.1） |
| **套件類型** | CRAN R 套件 |
| **授權** | MIT + file LICENSE |
| **上游倉庫** | https://github.com/bemts-hhs/traumar |

---

## 2. 核心原則（Governing Principles）

### GP-01 學術嚴謹性（Academic Rigor）
所有計算方法必須有對應的學術文獻支撐。新增指標時，必須在文件中引用文獻出處（作者、年份、DOI/PMID）。

### GP-02 單一責任函數（Single-Responsibility Functions）
每個匯出函數僅做一件事。複雜邏輯必須拆分為內部輔助函數，邏輯與 UI（訊息輸出）必須分離。

### GP-03 整潔資料輸入輸出（Tidy Data Compliance）
所有函數輸入應接受 `data.frame` 或 `tibble`，並使用 tidy eval（`rlang`）來處理欄位選取。輸出優先使用 `tibble`。

### GP-04 向後相容性優先（Backward Compatibility First）
改動既有函數的介面（參數名稱、預設值）屬於破壞性變更，須透過 `lifecycle` 包標示棄用流程，不可直接刪除。

### GP-05 可測試性（Testability）
所有新增或修改的邏輯必須有對應的 `testthat` 單元測試，位於 `tests/testthat/` 目錄。測試覆蓋率目標維持在現有水準或提升。

### GP-06 使用者友好的錯誤訊息（User-Friendly Errors）
錯誤與警告訊息必須使用 `cli` 套件格式化輸出，提供清楚的問題說明與修正建議。

### GP-07 複雜度控制（Complexity Gate）
- **禁止**：無充分理由新增套件依賴（需在 spec/plan 中說明必要性）。
- **禁止**：在一個 PR 中同時修改核心演算法與 API 介面。
- 新增依賴項須評估：是否可用現有依賴實作、CRAN 相容性、維護狀態。

---

## 3. 技術棧約束（Tech Stack Constraints）

### 程式語言
- **主語言**：R（≥ 4.1）
- **文件生成**：`roxygen2`（≥ 7.x）
- **測試框架**：`testthat`（edition 3）

### 核心依賴（不可移除）
| 套件 | 用途 |
|------|------|
| `dplyr` | 資料操作 |
| `rlang` | Tidy eval / 錯誤處理 |
| `cli` | 使用者訊息輸出 |
| `lifecycle` | API 棄用標記 |
| `ggplot2` | 視覺化輸出 |
| `tibble` | 資料框輸出 |

### 新增依賴審查門檻
新增任何 `Imports` 或 `Suggests` 依賴前，必須說明：
1. 為何現有依賴無法滿足需求
2. 該套件是否在 CRAN 上積極維護
3. 對套件安裝大小的影響

---

## 4. 程式碼風格規範（Code Style）

| 規範 | 說明 |
|------|------|
| **命名風格** | 函數使用 `snake_case`；內部函數前綴 `.` 或放於 `internal.R` |
| **參數命名** | 語義化英文，與 tidy eval 慣例一致（`*_col` 結尾用於欄位選取）。 |
| **程式碼註解** | 所有行內註解使用**繁體中文** |
| **文件語言** | roxygen 文件（`@description`、`@param`、`@examples`）使用英文（CRAN 要求）；`.agent/` 目錄下文件使用繁體中文 |
| **行寬** | 最多 100 字元 |

---

## 5. 目錄結構規範（Directory Layout）

```text
traumar.net/
├── R/                    # 所有匯出函數與內部函數
├── tests/
│   └── testthat/         # 單元測試（testthat edition 3）
├── man/                  # roxygen2 自動生成，不可手動編輯
├── DESCRIPTION           # 套件元資訊
├── NAMESPACE             # roxygen2 自動生成，不可手動編輯
├── NEWS.md               # 版本變更日誌
└── .agent/               # AI 輔助開發文件（不包含在套件建置中）
    ├── specs/            # 功能規格（spec.md、plan.md、tasks.md）
    └── skills/           # AI 技能設定
```

---

## 6. 功能規格守門條件（Spec Gate Conditions）

在建立任何功能的 `plan.md` 前，必須通過以下檢查：

- [ ] **學術依據**：新功能有對應的學術文獻或既有方法論支撐（GP-01）
- [ ] **介面設計**：不破壞現有函數介面，或已規劃棄用流程（GP-04）
- [ ] **依賴審查**：新增依賴已通過審查門檻（§3）
- [ ] **測試策略**：`tasks.md` 中包含對應的 `testthat` 測試任務（GP-05）
- [ ] **文件更新**：`NEWS.md` 與 roxygen 文件更新任務已納入（GP-01）

---

## 7. 成功標準預設值（Default Success Criteria）

若 spec.md 未特別說明，以下為預設成功標準：

- **SC-DEF-01**：所有現有 `R CMD check` 測試仍通過（0 errors, 0 warnings）。
- **SC-DEF-02**：新函數有完整的 roxygen 文件與至少 1 個 `@examples` 範例。
- **SC-DEF-03**：新邏輯有對應的 `testthat` 單元測試，覆蓋主要路徑與 1 個邊界案例。
- **SC-DEF-04**：函數錯誤訊息符合 `cli` 格式，能清楚描述錯誤原因。

---

## 8. 版本說明

| 版本 | 日期 | 說明 |
|------|------|------|
| 1.0.0 | 2026-04-11 | 初始版本，基於 traumar 套件現有架構建立 |
