# Prediction Model Project 1
**Postoperative Recurrence Risk Model for Hepatocellular Carcinoma (HCC)**  
Department of Gastroenterological Surgery I, Hokkaido University

---

## 📁 Repository Structure
01_Data/ # 入力データ（ローカル。GitHubには原則上げない）
├─ BR分類で生存解析のデータupdate20250825.xlsx
└─ BR分類で生存解析のデータupdate20251016.xlsx

02_Document/ # 補助ドキュメント（空/今後用）

03_Output/ # 解析の成果物（自動生成）
├─ figures/ # 図（校正・DCA・ROC・変数重要度 等）
│ ├─ calibration_plot.pdf
│ ├─ combined_evaluation.pdf
│ ├─ dca.pdf, dca_comparison.pdf
│ ├─ model_comparison.pdf
│ ├─ model_performance.csv
│ ├─ roc_curves.pdf, roc_curves_all_models(_fixed).pdf
│ ├─ variable_frequency.pdf
│ └─ variable_importance.pdf
├─ logs/
│ ├─ hcc_analysis_20251109.log
│ ├─ hcc_analysis_20251111.log
│ └─ hcc_analysis_20251113.log
├─ models/
│ ├─ additional_analysis_results.rds
│ ├─ all_models.rds
│ └─ data_split.rds
├─ tables/
│ ├─ all_model_coefficients.csv
│ ├─ best_model_metrics.csv
│ ├─ model_performance_complete.csv
│ ├─ model_performance_comprehensive_fixed.csv
│ ├─ model_performance_detailed.csv
│ ├─ model_performance_summary.csv
│ ├─ table1_baseline_characteristics.csv
│ └─ table2_model_coefficients.csv
├─ processed_data.rds
├─ session_info_20251109.txt
└─ session_info_20251113.txt

04_Script/
├─ R/ # 実行スクリプト（.R / .Rmd 両方）
│ ├─ 00_setup.R(.Rmd) # パッケージ/設定/関数読み込み
│ ├─ 01_data_loader.R(.Rmd) # データ読み込み
│ ├─ 02_data_cleaner.R(.Rmd) # 前処理・欠損対応
│ ├─ 03_feature_engineer.R(.Rmd)# 特徴量作成
│ ├─ 04_data_splitter.R(.Rmd) # 学習/検証データの分割（data_split.rds 出力）
│ ├─ 05_model_builder.R(.Rmd) # 複数モデルの学習（all_models.rds など）
│ ├─ 06_model_evaluator.R(.Rmd) # 性能評価・ROC・較正・DCA（figures/tables 出力）
│ ├─ 07_visualizer.R(.Rmd) # 可視化まとめ（変数重要度等）
│ └─ additional_analysis.R # 追加解析
├─ config/
│ ├─ settings.yaml # 実行設定（パス・乱数種など）
│ └─ variables.yaml # 変数定義/マッピング
├─ main.Rmd # 一括実行ノート（必要なら main.R を追加）
└─ old_version/ # 旧版Rmd群（履歴保管）

05_Reference/
├─ BLmodel1_Chan_ERASL-pre:ERASL-post.pdf
└─ BLmodel2_Shim_AnnSurg.pdf

Prediction model_Project1.Rproj
ds02_variables.csv


---

## ⚙️ Quick Start

```r
# 1) 依存環境を復元
install.packages("renv"); renv::restore()

# 2) 順に実行（.R を推奨。Rmdは knit でも可）
source("04_Script/R/00_setup.R")
source("04_Script/R/01_data_loader.R")
source("04_Script/R/02_data_cleaner.R")
source("04_Script/R/03_feature_engineer.R")
source("04_Script/R/04_data_splitter.R")
source("04_Script/R/05_model_builder.R")    # ← 複数モデルの学習
source("04_Script/R/06_model_evaluator.R")  # ← 性能評価（ROC/較正/DCAほか）
source("04_Script/R/07_visualizer.R")       # ← 重要度/頻度などの図

# 3) 追加で、mainノートを使う場合
rmarkdown::render("04_Script/main.Rmd")     # or source("04_Script/main.R") を用意


🧠 Purpose

This repository builds and validates a multivariable recurrence risk model for surgically treated hepatocellular carcinoma (HCC).
It performs time-to-event modeling (Cox, Fine–Gray), internal validation, calibration, decision curve analysis, and outputs risk-stratified survival plots.

🧠 What the pipeline does

Data handling: クリーニング・正規化・特徴量生成（02–03）

Split: 時間分割/固定分割で 03_Output/models/data_split.rds を作成（04）

Modeling: LASSO/Cox/（他）複数モデルを学習・保存（05）

Evaluation: ROC（単独/全モデル比較）、較正（Calibration-in-the-large/ slope, LOESS）、
Decision Curve Analysis、要約表を 03_Output/figures / tables へ（06）

Visualization: 変数重要度、使用頻度の図化（07）

Logs & Session info: 実行ログは 03_Output/logs/、session_info_*.txt に保存

📊 Competing-Risk（再発 vs 他因死）

KM/Log-rank：km_event（再発=1/他=0）を用いた RFS 曲線

CIF/Gray/Fine–Gray：event_type（0=打切り, 1=再発, 2=再発なし死亡）を用いた競合リスク解析

図・要約は 03_Output/figures/ と 03_Output/tables/ に自動保存（下のスニペット参照）


📁 Key Outputs
| Path                           | 内容例                                                                             |
| ------------------------------ | ------------------------------------------------------------------------------- |
| `03_Output/figures/`           | `calibration_plot.pdf`, `dca.pdf`, `roc_curves*.pdf`, `variable_importance.pdf` |
| `03_Output/tables/`            | `model_performance_*.csv`, `table1_*.csv`                                       |
| `03_Output/models/`            | `all_models.rds`, `data_split.rds`                                              |
| `03_Output/logs/`              | 実行ログ（日時入り）                                                                      |
| `03_Output/session_info_*.txt` | R環境の再現用情報                                                                       |


🔧 Config

04_Script/config/settings.yaml：入出力パス、seed、主要フラグ

04_Script/config/variables.yaml：変数名・カテゴリの定義

📦 Main packages

survival, survminer, cmprsk, riskRegression, glmnet, rms, timeROC, ggplot2, dplyr, readxl, yaml, cowplot ほか
renv::restore() で環境再現。

✍️ Author

Takuto Yoshida, MD, MPH
Gastroenterological Surgery I, Hokkaido University🔧 Config

04_Script/config/settings.yaml：入出力パス、seed、主要フラグ

04_Script/config/variables.yaml：変数名・カテゴリの定義

📦 Main packages

survival, survminer, cmprsk, riskRegression, glmnet, rms, timeROC, ggplot2, dplyr, readxl, yaml, cowplot ほか
renv::restore() で環境再現。

✍️ Author

Takuto Yoshida, MD, MPH
Gastroenterological Surgery I, Hokkaido University🔧 Config

04_Script/config/settings.yaml：入出力パス、seed、主要フラグ

04_Script/config/variables.yaml：変数名・カテゴリの定義

📦 Main packages

survival, survminer, cmprsk, riskRegression, glmnet, rms, timeROC, ggplot2, dplyr, readxl, yaml, cowplot ほか
renv::restore() で環境再現。

✍️ Author

Takuto Yoshida, MD, MPH
Gastroenterological Surgery I, Hokkaido University🔧 Config

04_Script/config/settings.yaml：入出力パス、seed、主要フラグ

04_Script/config/variables.yaml：変数名・カテゴリの定義

📦 Main packages

survival, survminer, cmprsk, riskRegression, glmnet, rms, timeROC, ggplot2, dplyr, readxl, yaml, cowplot ほか
renv::restore() で環境再現。

✍️ Author

Takuto Yoshida, MD, MPH
Gastroenterological Surgery I, Hokkaido University
(Last updated: 2025-11-13)