
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bemts-hhs/traumar/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/traumar)](https://CRAN.R-project.org/package=traumar)
![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/traumar) ![CRAN
monthly downloads](https://cranlogs.r-pkg.org/badges/last-month/traumar)
![CRAN weekly
downloads](https://cranlogs.r-pkg.org/badges/last-week/traumar)
[![Codecov test
coverage](https://codecov.io/gh/bemts-hhs/traumar/graph/badge.svg)](https://app.codecov.io/gh/bemts-hhs/traumar)
<!-- badges: end -->

# traumar <a href="https://bemts-hhs.github.io/traumar/"><img src="man/figures/logo.png" align="right" height="137" alt="traumar website" /></a>

持續品質改善 (CQI) 與流程改善 (PI) 是醫療照護的核心支柱，在受傷病患的照護中尤為重要。然而，醫院、創傷系統及其創傷計畫經理 (TPM) 往往缺乏獲取學術文獻中標準化品質指標的管道。{traumar} 套件藉由提供計算創傷中心或創傷系統有效性、效率及相對死亡率的工具，協助填補這一缺口。透過自動化這些計算，{traumar} 賦予醫院系統、創傷網絡及 TPM 更多力量，使其能專注於分析結果並推動病患照護的有意義改進。無論您是尋求強化 PI 倡議還是簡化 CQI 流程，{traumar} 都是提升創傷照護品質的寶貴資源。

## 安裝

您可以透過 [GitHub](https://github.com/bemts-hhs/traumar) 安裝開發版本的 `traumar`：

``` r
# install.packages("remotes")
remotes::install_github("bemts-hhs/traumar")
```

此外，您也可以透過以下方式安裝 CRAN 版本：

``` r
install.packages("traumar")
```

## 輔助函數

{traumar} 提供許多函數，協助您的資料分析旅程！特別是如果您目前無法獲取存活機率資料，{traumar} 提供 `probability_of_survival()` 函數，使用 TRISS 方法來計算。請參考套件文件 <https://bemts-hhs.github.io/traumar/>，其中包含各個函數的使用範例。

## 創傷系統評估與品質改善

{traumar} 包含允許使用者計算一或多個創傷中心，甚至整個創傷系統的效率與有效性的函數。`seqic_indicator_*` 系列函數提供使用者各項重要議題的分析邏輯與公式，例如：

- 外科醫生/醫師/中級照護提供者的反應時間
- 資料品質
- 死亡創傷病患的解剖率
- 血液酒精濃度測量 / 藥物檢測
- 不同風險群體的死亡率分析
- 病患抵達後的耗時：a) 實體出院, b) 醫療決定, c) 從醫療決定到實體出院的時間
- 創傷小組啟動 (Trauma team activations)
- 分流不足與過度分流 (Under and over triage)
- ...以及更多。請查看 {traumar} <https://bemts-hhs.github.io/traumar/> 文件以探索所有分析機會。

## 計算 W-Score

W-Score 告訴我們創傷中心平均每處理 100 個案例中，有多少超額存活（或死亡）人數。使用 R 語言，我們可以透過 {traumar} 套件來達成。

### 首先，我們為這些範例建立資料

``` r

# 產生範例資料
set.seed(123)

# 參數
n_patients <- 5000 # 病患總數

groups <- sample(x = LETTERS[1:2], size = n_patients, replace = TRUE) # 任意群組標籤

trauma_type_values <- sample(
  x = c("Blunt", "Penetrating"),
  size = n_patients,
  replace = TRUE
) # 創傷類型

rts_values <- sample(
  x = seq(from = 0, to = 7.8408, by = 0.005),
  size = n_patients,
  replace = TRUE
) # RTS 數值

ages <- sample(
  x = seq(from = 0, to = 100, by = 1),
  size = n_patients,
  replace = TRUE
) # 病患年齡

iss_scores <- sample(
  x = seq(from = 0, to = 75, by = 1),
  size = n_patients,
  replace = TRUE
) # ISS 分數

# 產生存活機率 (Ps)
Ps <- traumar::probability_of_survival(
  trauma_type = trauma_type_values,
  age = ages,
  rts = rts_values,
  iss = iss_scores
)

# 根據 Ps 模擬存活結果
survival_outcomes <- rbinom(n_patients, size = 1, prob = Ps)

# 建立資料框
data <- data.frame(Ps = Ps, survival = survival_outcomes, groups = groups) |>
  dplyr::mutate(death = dplyr::if_else(survival == 1, 0, 1))
```

### W-Score！

``` r

# 計算創傷表現 (W, M, Z 分數)
trauma_performance(data, Ps_col = Ps, outcome_col = death)
#> # A tibble: 1 × 9
#>   N_Patients N_Survivors N_Deaths Predicted_Survivors Predicted_Deaths
#>        <int>       <int>    <int>               <dbl>            <dbl>
#> 1       5000        1701     3299               1711.            3289.
#> # ℹ 4 more variables: Patient_Estimate <dbl>, W_Score <dbl>, M_Score <dbl>,
#> #   Z_Score <dbl>
```

## 比較病患組合的存活機率分佈與 Major Trauma Outcomes Study (MTOS)

使用文獻（Champion 等，1990；Flora，1978）定義的方法計算 M 與 Z 分數時，若存活機率分佈與 Major Trauma Outcomes Study 的分佈不夠相似，這些分數可能不具意義。{traumar} 提供了一種在資料分析腳本或主控台中檢查此項資訊的方法。`trauma_performance()` 函數會在幕後為您執行此操作，以便您了解對 Z 分數的信任程度。

``` r

# 比較當前病例組合與 MTOS 病例組合
trauma_case_mix(data, Ps_col = Ps, outcome_col = death)
#> # A tibble: 6 × 8
#>   Ps_range    current_fraction MTOS_distribution survivals predicted_survivals
#>   <chr>                  <dbl>             <dbl>     <int>               <dbl>
#> 1 0.00 - 0.25           0.541              0.01       2534                188.
#> 2 0.26 - 0.50           0.143              0.043       468                270.
#> 3 0.51 - 0.75           0.128              0           217                406.
#> 4 0.76 - 0.90           0.087              0.052        58                366.
#> 5 0.91 - 0.95           0.0508             0.053        18                238.
#> 6 0.96 - 1.00           0.0498             0.842         4                243.
#> # ℹ 3 more variables: deaths <int>, predicted_deaths <dbl>, count <int>
```

## 相對死亡率指標 (Relative Mortality Metric, RMM)

Napoli 等 (2017) 發表了計算創傷中心（或系統）表現的方法，克服了 W-Score 與 TRISS 方法論的問題。鑑於創傷中心處理的大多數病患其存活機率超過 90%，基於 W-Score 的表現評估可能僅能反映中心在處理低嚴重度病患時的表現。使用 Napoli 等 (2017) 的方法，可以計算出一個在解釋上與 W-Score 相似的分數，但透過建立分數範圍的非線性分箱 (non-linear bins) 並根據這些分箱的性質進行加權，解決了負偏態存活機率的問題。相對死亡率指標 (RMM) 的量表範圍從 -1 到 1。

- RMM 為 0 表示觀察到的死亡率與所有嚴重度層級的國家預期基準一致。
- RMM 大於 0 表示表現優於預期，即該中心的表現超過國家基準。
- RMM 小於 0 表示表現不如預期，即該中心的觀察死亡率高於預期基準。

## 非線性分箱演算法 (Non-Linear Binning Algorithm)

Napoli 等 (2017) 採取的方法中，很重要的一部份是修改 M-Score 對存活機率分佈進行線性分箱的做法，將其改為非線性。{traumar} 套件利用 Napoli 博士的方法為您執行此操作：

``` r

# 應用 nonlinear_bins 函數
results <- nonlinear_bins(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  divisor1 = 4,
  divisor2 = 4,
  threshold_1 = 0.9,
  threshold_2 = 0.99
)

# 查看演算法建立的區間
results$intervals
#> [1] 0.0002015449 0.0256191282 0.1455317587 0.4842820556 0.9003870455
#> [6] 0.9285354475 0.9518925450 0.9722272703 0.9968989233

# 查看分箱統計
results$bin_stats
#> # A tibble: 8 × 13
#>   bin_number bin_start bin_end    mean      sd Pred_Survivors_b Pred_Deaths_b
#>        <int>     <dbl>   <dbl>   <dbl>   <dbl>            <dbl>         <dbl>
#> 1          1  0.000202  0.0256 0.00935 0.00722             10.4       1106.  
#> 2          2  0.0256    0.146  0.0732  0.0345              81.7       1033.  
#> 3          3  0.146     0.484  0.293   0.0959             327.         788.  
#> 4          4  0.484     0.900  0.697   0.124              777.         337.  
#> 5          5  0.900     0.929  0.916   0.00790            114.          10.5 
#> 6          6  0.929     0.952  0.940   0.00680            117.           7.50
#> 7          7  0.952     0.972  0.963   0.00564            120.           4.65
#> 8          8  0.972     0.997  0.984   0.00686            162.           2.67
#> # ℹ 6 more variables: AntiS_b <dbl>, AntiM_b <dbl>, alive <int>, dead <int>,
#> #   count <int>, percent <dbl>
```

## RMM 函數

RMM 對高嚴重度病患非常敏感，這意味著如果創傷中心在處理這些病患時遇到困難，將反映在 RMM 中。相比之下，W-Score 可能會因為 MTOS 分佈中低嚴重度病患的影響而掩蓋表現的下滑。{traumar} 套件使用 Napoli 等 (2017) 的非線性分箱方法自動計算 RMM 單一分數。`rmm()` 與 `rm_bin_summary()` 函數內部呼叫 `nonlinear_bins()` 來產生非線性分箱過程。該函數使用 `n_samples` 的拔靴採樣 (bootstrap sampling) 來模擬 RMM 分佈並估計 95% 信賴區間。此外，也會提供 `data` 中母體的 RMM 及其對應的信賴區間。

``` r

# rmm() 函數範例用法
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  n_samples = 250,
  Divisor1 = 4,
  Divisor2 = 4
)
#> # A tibble: 1 × 8
#>   population_RMM_LL population_RMM population_RMM_UL population_CI
#>               <dbl>          <dbl>             <dbl>         <dbl>
#> 1            -0.106        0.00247             0.111         0.108
#> # ℹ 4 more variables: bootstrap_RMM_LL <dbl>, bootstrap_RMM <dbl>,
#> #   bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>

# 有時轉換資料格式 (Pivoting) 會很有幫助
rmm(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  n_samples = 250,
  Divisor1 = 4,
  Divisor2 = 4,
  pivot = TRUE
)
#> # A tibble: 8 × 2
#>   stat                  value
#>   <chr>                 <dbl>
#> 1 population_RMM_LL -0.106   
#> 2 population_RMM     0.00247 
#> 3 population_RMM_UL  0.111   
#> 4 population_CI      0.108   
#> 5 bootstrap_RMM_LL   0.000492
#> 6 bootstrap_RMM      0.00218 
#> 7 bootstrap_RMM_UL   0.00386 
#> 8 bootstrap_CI       0.00168

# 透過非線性分箱範圍計算 RMM
# rm_bin_summary() 函數
rm_bin_summary(
  data = data,
  Ps_col = Ps,
  outcome_col = survival,
  Divisor1 = 4,
  Divisor2 = 4,
  n_samples = 250
)
#> # A tibble: 8 × 19
#>   bin_number  TA_b  TD_b   N_b  EM_b AntiS_b AntiM_b bin_start bin_end midpoint
#>        <int> <int> <int> <int> <dbl>   <dbl>   <dbl>     <dbl>   <dbl>    <dbl>
#> 1          1     9  1107  1116 0.992 0.00935  0.991   0.000202  0.0256   0.0129
#> 2          2    72  1043  1115 0.935 0.0732   0.927   0.0256    0.146    0.0856
#> 3          3   300   815  1115 0.731 0.293    0.707   0.146     0.484    0.315 
#> 4          4   805   309  1114 0.277 0.697    0.303   0.484     0.900    0.692 
#> 5          5   115    10   125 0.08  0.916    0.0844  0.900     0.929    0.914 
#> 6          6   116     9   125 0.072 0.940    0.0600  0.929     0.952    0.940 
#> 7          7   119     6   125 0.048 0.963    0.0372  0.952     0.972    0.962 
#> 8          8   165     0   165 0     0.984    0.0162  0.972     0.997    0.985 
#> # ℹ 9 more variables: R_b <dbl>, population_RMM_LL <dbl>, population_RMM <dbl>,
#> #   population_RMM_UL <dbl>, population_CI <dbl>, bootstrap_RMM_LL <dbl>,
#> #   bootstrap_RMM <dbl>, bootstrap_RMM_UL <dbl>, bootstrap_CI <dbl>
```

## 行為準則

請注意，traumar 專案發布時附有 [貢獻者行為準則](https://bemts-hhs.github.io/traumar/CODE_OF_CONDUCT.html)。參與本專案即代表您同意遵守其條款。
