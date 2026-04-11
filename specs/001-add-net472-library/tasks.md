# 實作任務 (Tasks)

## Phase 1: Setup
- [x] T001 [P] 建立 .NET 4.7.2 類別庫 `dotnet/Traumar.NET/Traumar` 及安裝 `MathNet.Numerics` (檔案: `dotnet/Traumar.NET/Traumar/Traumar.csproj`)
- [x] T002 [P] 建立 xUnit 測試專案 `dotnet/Traumar.NET/Traumar.Tests` 並連結主專案 (檔案: `dotnet/Traumar.NET/Traumar.Tests/Traumar.Tests.csproj`)
- [x] T003 修改 `.Rbuildignore` 排除 `^dotnet/` 以避免干擾 R CMD check，完成後須執行 `R CMD check --as-cran` 確認仍通過（0 errors, 0 warnings，對應 SC-DEF-01）(檔案: `.Rbuildignore`)

## Phase 2: Foundations
- [x] T004 建立 `PatientInput` 與 `PatientRecord` DTOs (檔案: `dotnet/Traumar.NET/Traumar/Models/PatientModels.cs`)
- [x] T005 建立 `RmmResult` 與 `TraumaPerformanceResult` DTOs (檔案: `dotnet/Traumar.NET/Traumar/Models/ResultModels.cs`)

## Phase 3: User Stories (US1)
- [x] T006 [US1] 實作 `ProbabilityOfSurvival` 核心計算邏輯 (檔案: `dotnet/Traumar.NET/Traumar/Core/SurvivalCalculator.cs`)
- [x] T007 [US1] 撰寫 `ProbabilityOfSurvival` 測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/SurvivalCalculatorTests.cs`)

## Phase 4: User Stories (US2)
- [x] T008 [US2] 建立 `BinStatistics` DTO (internal) 及 `NonLinearBins` 演算法 (檔案: `dotnet/Traumar.NET/Traumar/Core/NonLinearBinsCalculator.cs`)
- [x] T009 [US2] 撰寫 `NonLinearBins` 內部測試 (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/NonLinearBinsCalculatorTests.cs`)
- [x] T010 [US2] 實作 `CalculateRmm` 計算 (包含 Bootstrap CI 機制) (檔案: `dotnet/Traumar.NET/Traumar/Core/RmmCalculator.cs`)
- [x] T011 [US2] 撰寫 `CalculateRmm` 測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/RmmCalculatorTests.cs`)

## Phase 5: User Stories (US3)
- [x] T012 [US3] 實作 `CalculateTraumaPerformance` (W/M/Z Score、除以零防護，並依A6修正Outcome反轉公式) (檔案: `dotnet/Traumar.NET/Traumar/Core/PerformanceCalculator.cs`)
- [x] T013 [US3] 撰寫 `CalculateTraumaPerformance` 測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/PerformanceCalculatorTests.cs`)

## Phase 6: User Stories (US4 - SEQIC 1~13)
- [ ] T014 [US4] [P] 建立各指標 `IndicatorNInput` 與 `IndicatorNResult` DTOs (檔案: `dotnet/Traumar.NET/Traumar/Models/SeqicModels.cs`)
- [ ] T015 [US4] 實作 SEQIC 1~4 指標計算 (檔案: `dotnet/Traumar.NET/Traumar/Seqic/Indicators1to4.cs`)
- [ ] T016 [US4] 實作 SEQIC 5~8 指標計算 (檔案: `dotnet/Traumar.NET/Traumar/Seqic/Indicators5to8.cs`)
- [ ] T017 [US4] 實作 SEQIC 9~13 指標計算 (檔案: `dotnet/Traumar.NET/Traumar/Seqic/Indicators9to13.cs`)
- [ ] T018a [US4] 撰寫 SEQIC 指標 1~4 對應之測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Seqic/Indicators1to4Tests.cs`)
- [ ] T018b [US4] 撰寫 SEQIC 指標 5~8 對應之測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Seqic/Indicators5to8Tests.cs`)
- [ ] T018c [US4] 撰寫 SEQIC 指標 9~13 對應之測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Seqic/Indicators9to13Tests.cs`)

## Phase 7: Polish
- [ ] T019 更新 `README.md`，提供 API 對照表與說明 (檔案: `dotnet/Traumar.NET/Traumar/README.md`)
- [ ] T020 更新 R 套件 `NEWS.md`，記錄新增 `dotnet/Traumar.NET/` 子目錄及其用途 (檔案: `NEWS.md`，對應 Constitution §6 Gate Condition)
- [ ] T021 建立範例主控台專案，驗證 `Traumar.dll` 可在 .NET 4.7.2 環境正常引用，確認 SC-004 通過 (檔案: `dotnet/Traumar.NET/Traumar.Sample/Traumar.Sample.csproj`)
