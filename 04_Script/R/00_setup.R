knitr::opts_chunk$set(echo = TRUE)

################################################################################
# 00_setup.R - プロジェクト初期設定とパッケージ管理
# 
# Description: 
#   HCC再発予測モデルプロジェクトの環境設定
#   必要パッケージの自動インストールと読み込み
#   グローバル設定とユーティリティ関数の定義
#
# Author: Takuto Yoshida
# Date: 2024-11-07
################################################################################

# ========================================================================
# 1. パッケージ管理
# ========================================================================
# 下記はroxygen2というRパッケージ開発の際の記法。関数の説明を書く部分
#' 必要パッケージの自動インストールと読み込み
#' @param packages 文字ベクトル：パッケージ名のリスト
#' @param repos リポジトリURL（デフォルト: CRAN）
#' @return 無し（副作用：パッケージの読み込み）
ensure_packages <- function(packages, 
                          repos = "https://cloud.r-project.org/") {
  
  # インストール済みパッケージの確認
  installed <- rownames(installed.packages())
  to_install <- setdiff(packages, installed)
  
  # 未インストールパッケージがあれば自動インストール
  if (length(to_install) > 0) {
    message("========================================")
    message("Installing missing packages...")
    message("========================================")
    
    for (pkg in to_install) {
      message(sprintf("  Installing: %s", pkg))
      install.packages(pkg, repos = repos, quiet = TRUE)
    }
    
    message("Installation completed.")
  }
  
  # パッケージの読み込み
  loaded <- character()
  failed <- character()
  
  for (pkg in packages) {
    if (suppressPackageStartupMessages(require(pkg, character.only = TRUE))) {
      loaded <- c(loaded, pkg)
    } else {
      failed <- c(failed, pkg)
    }
  }
  
  # 結果の報告
  if (length(failed) > 0) {
    warning("Failed to load packages: ", paste(failed, collapse = ", "))
  }
  
  invisible(NULL)
}

# ========================================================================
# 2. 必要パッケージの定義と読み込み
# ========================================================================

# コアパッケージ
core_packages <- c(
  # データ操作
  "tidyverse",      # dplyr, tidyr, ggplot2等を含む
  "data.table",     # 高速データ処理
  
  # データ読み込み
  "readxl",         # Excel読み込み
  "yaml",           # YAML設定ファイル読み込み
  "readr",          # CSV読み込み（高速）
  
  # データ処理
  "lubridate",      # 日付処理
  "stringr",        # 文字列処理
  "forcats",        # 因子処理
  
  # 統計・モデリング
  "survival",       # 生存分析
  "glmnet",        # LASSO/Ridge回帰
  "MASS",          # stepAIC
  "caret",         # 機械学習フレームワーク
  
  # 評価・診断
  "survminer",     # 生存分析可視化
  "pROC",          # ROC分析
  "timeROC",       # 時間依存ROC
  "rms",           # 回帰モデリング戦略
  "prodlim",       # 生存確率推定
  "cmprsk",        # 競合リスク
  "riskRegression", # リスク予測
  
  # 可視化
  "ggplot2",       # グラフ作成
  "plotly",        # インタラクティブプロット
  "ggsci",         # 科学論文用カラーパレット
  "patchwork",     # 図の結合
  "gt",            # 美しいテーブル作成
  
  # レポート
  "knitr",         # 動的レポート生成
  "rmarkdown",     # Markdownレポート
  
  # その他ユーティリティ
  "here",          # プロジェクトパス管理
  "logger"         # ログ管理
)

# オプションパッケージ（エラーが出ても続行）
optional_packages <- c(
  "DataExplorer",  # EDA
  "VIM",           # 欠損値可視化
  "mice",          # 多重代入法
  "sandwich",      # ロバスト標準誤差
  "lmtest",        # 統計検定
  "tableone"       # Table 1作成
)

# パッケージの読み込み実行
message("\n=== Loading Core Packages ===")
ensure_packages(core_packages)

message("\n=== Loading Optional Packages ===")
suppressWarnings({
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("Optional package '%s' not available. Skipping.", pkg))
    } else {
      library(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
})

# ========================================================================
# 3. グローバル設定
# ========================================================================

#' プロジェクトのグローバル設定を行う関数
#' @param config_path 設定ファイルのパス
setup_global_options <- function(config_path = NULL) {
  
  # R全般の設定
  options(
    stringsAsFactors = FALSE,        # 文字列を自動的に因子にしない
    scipen = 10,                      # 科学的記数法の抑制
    digits = 4,                       # 表示桁数
    max.print = 1000,                 # 最大表示行数
    warn = 1,                         # 警告を即座に表示
    tidyverse.quiet = TRUE,           # tidyverseの起動メッセージ抑制
    dplyr.summarise.inform = FALSE   # dplyrのグループ化メッセージ抑制
  )
  
  # ggplot2のデフォルトテーマ設定
  if ("ggplot2" %in% .packages()) {
    theme_set(
      theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          strip.background = element_rect(fill = "grey90", color = NA),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        )
    )
  }
  
  # 日本語フォント設定（Mac/Windows対応）
  if (Sys.info()["sysname"] == "Darwin") {  # Mac
    if ("ggplot2" %in% .packages()) {
      theme_update(text = element_text(family = "HiraKakuProN-W3"))
    }
  } else if (Sys.info()["sysname"] == "Windows") {  # Windows
    if ("ggplot2" %in% .packages()) {
      theme_update(text = element_text(family = "Yu Gothic"))
    }
  }
  
  # 乱数シード設定（設定ファイルから読み込み）
  if (!is.null(config_path) && file.exists(config_path)) {
    config <- yaml::read_yaml(config_path)
    if (!is.null(config$model$seed)) {
      set.seed(config$model$seed)
      message(sprintf("Random seed set to: %d", config$model$seed))
    }
  } else {
    set.seed(20251102)  # デフォルトシード
    message("Using default random seed: 20251102")
  }
  
  invisible(NULL)
}

# ========================================================================
# 4. プロジェクトパス設定
# ========================================================================

#' プロジェクトのパス構造を設定
#' @param base_dir ベースディレクトリ（NULL時は設定ファイルから）
#' @return パスのリスト
setup_project_paths <- function(base_dir = NULL) {
  
  # 設定ファイルの読み込み試行
  config_file <- here::here("config", "settings.yaml")
  
  if (is.null(base_dir) && file.exists(config_file)) {
    config <- yaml::read_yaml(config_file)
    base_dir <- config$paths$base_dir
  }
  
  # ベースディレクトリの決定
  if (is.null(base_dir)) {
    base_dir <- here::here()  # プロジェクトルート
  }
  
  # パス構造の定義
  paths <- list(
    base = base_dir,
    data = file.path(base_dir, "01_Data"),
    document = file.path(base_dir, "02_Document"),
    output = file.path(base_dir, "03_Output"),
    script = file.path(base_dir, "04_Script"),
    reference = file.path(base_dir, "05_Reference"),
    config = file.path(base_dir, "04_Script", "config"),
    R = file.path(base_dir, "04_Script", "R"),
    
    # 出力用サブディレクトリ
    output_tables = file.path(base_dir, "03_Output", "tables"),
    output_figures = file.path(base_dir, "03_Output", "figures"),
    output_models = file.path(base_dir, "03_Output", "models"),
    output_reports = file.path(base_dir, "03_Output", "reports")
  )
  
  # 出力ディレクトリの作成（存在しなければ）
  output_dirs <- grep("^output", names(paths), value = TRUE)
  for (dir_name in output_dirs) {
    dir_path <- paths[[dir_name]]
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      message(sprintf("Created directory: %s", dir_path))
    }
  }
  
  return(paths)
}

# ========================================================================
# 5. ログ設定
# ========================================================================

#' ログシステムの初期化（修正版）
#' @param log_file ログファイルのパス
#' @param log_level ログレベル（DEBUG, INFO, WARN, ERROR）
setup_logging <- function(log_file = NULL, log_level = "INFO") {
  
  if (!requireNamespace("logger", quietly = TRUE)) {
    message("Logger package not available. Skipping logging setup.")
    return(invisible(NULL))
  }
  
  # ログファイルパスの設定
  if (is.null(log_file)) {
    paths <- setup_project_paths()
    log_dir <- file.path(paths$output, "logs")
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # 日付付きログファイル名
    log_file <- file.path(
      log_dir, 
      sprintf("hcc_analysis_%s.log", format(Sys.Date(), "%Y%m%d"))
    )
  }
  
  # loggerの設定（修正版）
  tryCatch({
    # 新しいバージョンの書き方
    logger::log_threshold(log_level)
    logger::log_appender(logger::appender_tee(log_file))
  }, error = function(e) {
    # エラーが出たら古いバージョンの書き方を試す
    tryCatch({
      log_levels <- c(DEBUG = 400, INFO = 600, WARN = 700, ERROR = 800)
      logger::log_threshold(log_levels[log_level])
      logger::log_appender(logger::appender_tee(log_file))
    }, error = function(e2) {
      # それでもダメなら警告を出してログをスキップ
      message("Warning: Could not configure logger. Continuing without logging.")
      return(invisible(NULL))
    })
  })
  
  # 開始メッセージ（エラーが出ないようにtryCatchで囲む）
  tryCatch({
    logger::log_info("========================================")
    logger::log_info("HCC Prediction Model Analysis Started")
    logger::log_info("Time: {Sys.time()}")
    logger::log_info("R Version: {R.version.string}")
    logger::log_info("Platform: {Sys.info()['sysname']} {Sys.info()['release']}")
    logger::log_info("========================================")
  }, error = function(e) {
    # ログが失敗してもメッセージは表示
    message("========================================")
    message("HCC Prediction Model Analysis Started")
    message(paste("Time:", Sys.time()))
    message(paste("R Version:", R.version.string))
    message("========================================")
  })
  
  invisible(NULL)
}

# ========================================================================
# 6. ユーティリティ関数
# ========================================================================

#' セッション情報の記録
#' この分析が、どのような環境で実行されたか詳細な情報を日付入りテキストファイルに保存する関数
#' @param output_dir 出力ディレクトリ
save_session_info <- function(output_dir = NULL) {
  
  if (is.null(output_dir)) {
    paths <- setup_project_paths()
    output_dir <- paths$output
  }
  
  session_file <- file.path(
    output_dir, 
    sprintf("session_info_%s.txt", format(Sys.Date(), "%Y%m%d"))
  )
  
  sink(session_file)
  cat("=== Session Information ===\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  print(sessionInfo())
  sink()
  
  message(sprintf("Session info saved to: %s", session_file))
  invisible(NULL)
}

#' メモリ使用状況の確認
check_memory_usage <- function() {
  # ガベージコレクション実行
  gc_result <- gc(verbose = FALSE)
  
  # 使用メモリ（MB）
  used_mb <- sum(gc_result[, "used"]) / (1024^2) * gc_result[1, "(Mb)"]
  max_mb <- sum(gc_result[, "max used"]) / (1024^2) * gc_result[1, "(Mb)"]
  
  message(sprintf("Memory usage: %.1f MB (max: %.1f MB)", used_mb, max_mb))
  
  return(invisible(list(used = used_mb, max = max_mb)))
}

# ========================================================================
# 7. 初期化実行
# ========================================================================

#' プロジェクトの完全初期化
#' @param config_path 設定ファイルのパス
#' @param verbose 詳細出力フラグ
initialize_project <- function(config_path = NULL, verbose = TRUE) {
  
  if (verbose) {
    message("\n========================================")
    message("Initializing HCC Prediction Model Project")
    message("========================================\n")
  }
  
  # 1. グローバルオプション設定
  setup_global_options(config_path)
  
  # 2. プロジェクトパス設定
  paths <- setup_project_paths()
  
  # 3. ログ設定
  setup_logging()
  
  # 4. メモリチェック
  check_memory_usage()
  
  # 5. セッション情報保存
  if (verbose) {
    save_session_info()
  }
  
  if (verbose) {
    message("\n✅ Project initialization completed successfully!")
    message("========================================\n")
  }
  
  return(invisible(paths))
}

# ========================================================================
# スクリプト実行時の自動初期化（オプション）
# ========================================================================

# このスクリプトが直接実行された場合のみ初期化を行う
if (sys.nframe() == 0) {
  initialize_project(verbose = TRUE)
}
