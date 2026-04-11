# 實作任務 (Tasks)

## Phase 1: Setup
- [ ] T001 [P] 建立 .NET 4.7.2 類別庫 `dotnet/Traumar.NET/Traumar` 及安裝 `MathNet.Numerics` (檔案: `dotnet/Traumar.NET/Traumar/Traumar.csproj`)
- [ ] T002 [P] 建立 xUnit 測試專案 `dotnet/Traumar.NET/Traumar.Tests` 並連結主專案 (檔案: `dotnet/Traumar.NET/Traumar.Tests/Traumar.Tests.csproj`)
- [ ] T003 修改 `.Rbuildignore` 排除 `^dotnet/` 以避免干擾 R CMD check (檔案: `.Rbuildignore`)

## Phase 2: Foundations
- [ ] T004 建立 `PatientInput` 與 `PatientRecord` DTOs (檔案: `dotnet/Traumar.NET/Traumar/Models/PatientModels.cs`)
- [ ] T005 建立 `RmmResult` 與 `TraumaPerformanceResult` DTOs (檔案: `dotnet/Traumar.NET/Traumar/Models/ResultModels.cs`)

## Phase 3: User Stories (US1)
- [ ] T006 [US1] 實作 `ProbabilityOfSurvival` 核心計算邏輯 (檔案: `dotnet/Traumar.NET/Traumar/Core/SurvivalCalculator.cs`)
- [ ] T007 [US1] 撰寫 `ProbabilityOfSurvival` 測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/SurvivalCalculatorTests.cs`)

## Phase 4: User Stories (US2)
- [ ] T008 [US2] 建立 `BinStatistics` DTO (internal) 及 `NonLinearBins` 演算法 (檔案: `dotnet/Traumar.NET/Traumar/Core/NonLinearBinsCalculator.cs`)
- [ ] T009 [US2] 撰寫 `NonLinearBins` 內部測試 (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/NonLinearBinsCalculatorTests.cs`)
- [ ] T010 [US2] 實作 `CalculateRmm` 計算 (包含 Bootstrap CI 機制) (檔案: `dotnet/Traumar.NET/Traumar/Core/RmmCalculator.cs`)
- [ ] T011 [US2] 撰寫 `CalculateRmm` 測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/RmmCalculatorTests.cs`)

## Phase 5: User Stories (US3)
- [ ] T012 [US3] 實作 `CalculateTraumaPerformance` (W/M/Z Score、除以零防護，並依A6修正Outcome反轉公式) (檔案: `dotnet/Traumar.NET/Traumar/Core/PerformanceCalculator.cs`)
- [ ] T013 [US3] 撰寫 `CalculateTraumaPerformance` 測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Core/PerformanceCalculatorTests.cs`)

## Phase 6: User Stories (US4 - SEQIC 1~13)
- [ ] T014 [US4] [P] 建立各指標 `IndicatorNInput` 與 `IndicatorNResult` DTOs (檔案: `dotnet/Traumar.NET/Traumar/Models/SeqicModels.cs`)
- [ ] T015 [US4] 實作 SEQIC 1~4 指標計算 (檔案: `dotnet/Traumar.NET/Traumar/Seqic/Indicators1to4.cs`)
- [ ] T016 [US4] 實作 SEQIC 5~8 指標計算 (檔案: `dotnet/Traumar.NET/Traumar/Seqic/Indicators5to8.cs`)
- [ ] T017 [US4] 實作 SEQIC 9~13 指標計算 (檔案: `dotnet/Traumar.NET/Traumar/Seqic/Indicators9to13.cs`)
- [ ] T018 [US4] 撰寫 SEQIC 指標對應之測試案例 (含繁體中文例外驗證) (檔案: `dotnet/Traumar.NET/Traumar.Tests/Seqic/IndicatorTests.cs`)

## Phase 7: Polish
- [ ] T019 更新 `README.md`，提供 API 對照表與說明 (檔案: `dotnet/Traumar.NET/Traumar/README.md`)
