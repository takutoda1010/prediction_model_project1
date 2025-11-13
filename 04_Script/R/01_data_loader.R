knitr::opts_chunk$set(echo = TRUE)

################################################################################
# 01_data_loader.R - データ読み込みと初期処理
# 
# Description: 
#   Excelデータの読み込み、列名マッピング、基本的な前処理
#   データセットのバージョン管理（ds00, ds01, ds02...）
#
# Author: Takuto Yoshida
# Date: 2024-11-07
################################################################################

# ========================================================================
# 1. データ読み込み関数
# ========================================================================

#' Excelファイルからデータを読み込む
#' @param config 設定リスト（YAMLから読み込んだもの）
#' @param sheet シート番号または名前（デフォルト: 1）
#' @param guess_max 型推定に使用する最大行数
#' @return データフレーム
load_raw_data <- function(config, sheet = NULL, guess_max = 100000) {
  
  # 設定からファイルパスを構築
  if (is.null(sheet)) {
    sheet <- ifelse(!is.null(config$paths$sheet), config$paths$sheet, 1)
  }
  
  data_path <- file.path(
    config$paths$base_dir,
    config$paths$data_dir,
    config$paths$main_data
  )
  
  # ファイルの存在確認
  if (!file.exists(data_path)) {
    stop(sprintf("Data file not found: %s", data_path))
  }
  
  message(sprintf("\n📁 Loading data from: %s", basename(data_path)))
  
  # Excelファイル読み込み
  tryCatch({
    df_raw <- suppressWarnings(
      readxl::read_excel(
        path = data_path,
        sheet = sheet,
        guess_max = guess_max,
        .name_repair = "minimal"
      )
    )
    
    message(sprintf("✅ Data loaded: %d rows × %d columns", 
                   nrow(df_raw), ncol(df_raw)))
    
    return(df_raw)
    
  }, error = function(e) {
    stop(sprintf("Error loading Excel file: %s", e$message))
  })
}

# ========================================================================
# 2. データセット管理クラス
# ========================================================================

#' データセット管理用のS3クラス
#' @description ds00, ds01, ds02...の形式でデータセットを管理
DatasetManager <- function(base_data = NULL) {
  
  # 内部ストレージ
  # 金庫とカウンターを作る。（親クラスの環境）
  datasets <- list()
  current_version <- -1
  
  # ベースデータがあれば ds00 として保存
  if (!is.null(base_data)) {
    datasets[["ds00"]] <- base_data
    current_version <- 0
  }
  
  # オブジェクト構造
  structure(
    list(
      datasets = datasets,
      current_version = current_version,
      
      # メソッド定義
      # 新しいバージョンのデータセットをマネージャーに保存する
      add_version = function(data, version = NULL) {
        if (is.null(version)) {
          version <- current_version + 1
        }
        ds_name <- sprintf("ds%02d", version)
        # 親クラスの金庫に無理やりデータを追加する
        datasets[[ds_name]] <<- data
        # 親クラスのカウンターを無理やり書き換える
        current_version <<- version
        message(sprintf("📌 Created dataset: %s", ds_name))
        invisible(data)
      },
      
      get_version = function(version = NULL) {
        if (is.null(version)) {
          version <- current_version
        }
        ds_name <- sprintf("ds%02d", version)
        if (ds_name %in% names(datasets)) {
          return(datasets[[ds_name]])
        } else {
          stop(sprintf("Dataset %s not found", ds_name))
        }
      },
      
      list_versions = function() {
        names(datasets)
      },
      
      get_current = function() {
        if (current_version >= 0) {
          return(datasets[[sprintf("ds%02d", current_version)]])
        } else {
          stop("No dataset available")
        }
      },
      
      get_all = function() {
        datasets
      },
      
      summary = function() {
        if (length(datasets) == 0) {
          cat("No datasets loaded\n")
        } else {
          cat("\n=== Dataset Summary ===\n")
          for (ds_name in names(datasets)) {
            df <- datasets[[ds_name]]
            cat(sprintf("%s: %d rows × %d columns\n", 
                       ds_name, nrow(df), ncol(df)))
          }
          cat(sprintf("\nCurrent version: ds%02d\n", current_version))
        }
      }
    ),
    class = "DatasetManager"
  )
}

# ========================================================================
# 3. 列名処理関数
# ========================================================================

#' 列名の正規化
#' @param df データフレーム
#' @return 列名が正規化されたデータフレーム
normalize_column_names <- function(df) {
  
  # 全角スペースと連続スペースを半角スペース1つに変換
  names(df) <- names(df) %>%
    stringr::str_replace_all("[\u3000\\s]+", " " ) %>% #\u3000: 全角スペース, \s: 半角スペース
    stringr::str_trim()
  
  return(df)
}

#' 列名マッピングの適用
#' @param df データフレーム
#' @param variables 変数定義（variables.yamlから読み込んだもの）
#' @return マッピング適用後のデータフレーム
# 新しいdfがちゃんと元のデータから正しくコピーできているかわかりやすいよう古い名前も残している。
apply_column_mapping <- function(df, variables) {
  
  if (is.null(variables$column_mappings)) {
    warning("No column mappings found in variables configuration")
    return(df)
  }
  
  # マッピングの適用
  mapping_applied <- 0
  mapping_failed <- character()
  
  for (orig_name in names(variables$column_mappings)) {
    mapping_info <- variables$column_mappings[[orig_name]]
    new_name <- mapping_info$name
    
    if (orig_name %in% names(df)) {
      # 新しい列を作成（元の列も保持）
      df[[new_name]] <- df[[orig_name]]
      mapping_applied <- mapping_applied + 1
    } else {
      mapping_failed <- c(mapping_failed, orig_name)
    }
  }
  
  message(sprintf("📋 Column mapping: %d applied, %d not found", 
                 mapping_applied, length(mapping_failed)))
  
  if (length(mapping_failed) > 0 && length(mapping_failed) <= 10) {
    message("   Not found: ", paste(mapping_failed, collapse = ", "))
  } else if (length(mapping_failed) > 10) {
    message(sprintf("   Not found: %d columns (too many to display)", 
                   length(mapping_failed)))
  }
  
  return(df)
}

# ========================================================================
# 4. 型変換関数
# ========================================================================

#' データ型の自動調整
#' @param df データフレーム
#' @param variables 変数定義
#' @return 型調整後のデータフレーム
adjust_data_types <- function(df, variables = NULL) {
  
  # readrによる自動型推定
  df_converted <- suppressMessages(
    readr::type_convert(df, locale = readr::locale(encoding = "UTF-8"))
  )
  
  # 変数定義に基づく型の調整
  if (!is.null(variables) && !is.null(variables$column_mappings)) {
    
    for (mapping in variables$column_mappings) {
      col_name <- mapping$name
      col_type <- mapping$type
      
      if (col_name %in% names(df_converted)) {
        
        # 型に応じた変換
        if (col_type == "character") {
          df_converted[[col_name]] <- as.character(df_converted[[col_name]])
          
        } else if (col_type == "numeric") {
          df_converted[[col_name]] <- suppressWarnings(
            as.numeric(df_converted[[col_name]])
          )
          
        } else if (col_type == "integer") {
          df_converted[[col_name]] <- suppressWarnings(
            as.integer(df_converted[[col_name]])
          )
          
        } else if (col_type == "logical") {
          # カスタム変換関数を使用（00_setup.Rから）
          if (exists("to_tf")) {
            df_converted[[col_name]] <- to_tf(df_converted[[col_name]])
          }
          
        } else if (col_type == "date") {
          # カスタム変換関数を使用
          if (exists("to_date_excel_mixed")) {
            df_converted[[col_name]] <- to_date_excel_mixed(df_converted[[col_name]])
          }
          
        } else if (col_type %in% c("factor", "ordered")) {
          df_converted[[col_name]] <- factor(df_converted[[col_name]])
          
          # レベルの設定
          if (!is.null(mapping$levels)) {
            df_converted[[col_name]] <- factor(
              df_converted[[col_name]], 
              levels = mapping$levels
            )
          }
          
          # 順序因子の場合
          if (col_type == "ordered") {
            df_converted[[col_name]] <- ordered(df_converted[[col_name]])
          }
        }
      }
    }
  }
  
  return(df_converted)
}

# ========================================================================
# 5. 列名エイリアス処理
# ========================================================================

#' 列名エイリアスの適用
#' @param df データフレーム
#' @return エイリアス適用後のデータフレーム
apply_column_aliases <- function(df) {
  
  # よく使われるエイリアスの定義
  aliases <- list(
    "再発有無" = "再発有無TRUE/FALSEの二群",
    "StageI+II or III+IV" = "StageI+II or III+IV　非癌部肝(旧入力）",
    "合併症胸水" = "合併症胸水 0-2の順序変数",
    "合併症出血" = "合併症出血 0-2の順序変数",
    "合併症胆汁漏" = "合併症胆汁漏順序変数",
    "合併症消化管" = "合併症消化管順序変数",
    "合併症術創" = "合併症術創順序変数"
  )
  
  # エイリアスの適用
  for (short_name in names(aliases)) {
    long_name <- aliases[[short_name]]
    
    if (short_name %in% names(df) && !long_name %in% names(df)) {
      names(df)[names(df) == short_name] <- long_name
    }
  }
  
  return(df)
}

# ========================================================================
# 6. データ検証関数
# ========================================================================

#' データの基本的な検証
#' @param df データフレーム
#' @param required_cols 必須列のベクトル
#' @return 検証結果（論理値）
validate_data <- function(df, required_cols = NULL) {
  
  validation_passed <- TRUE
  messages <- character()
  
  # データフレームの確認
  if (!is.data.frame(df)) {
    messages <- c(messages, "❌ Input is not a data frame")
    validation_passed <- FALSE
  }
  
  # 空のデータフレームチェック
  if (nrow(df) == 0) {
    messages <- c(messages, "❌ Data frame has no rows")
    validation_passed <- FALSE
  }
  
  if (ncol(df) == 0) {
    messages <- c(messages, "❌ Data frame has no columns")
    validation_passed <- FALSE
  }
  
  # 必須列の確認
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      messages <- c(messages, 
                   sprintf("❌ Missing required columns: %s", 
                          paste(missing_cols, collapse = ", ")))
      validation_passed <- FALSE
    }
  }
  
  # 結果の表示
  if (validation_passed) {
    message("✅ Data validation passed")
  } else {
    message("Data validation failed:")
    for (msg in messages) {
      message("  ", msg)
    }
  }
  
  return(validation_passed)
}

# ========================================================================
# 7. 統合データ読み込み関数
# ========================================================================

#' データ読み込みパイプライン
#' @param config_path 設定ファイルのパス
#' @param variables_path 変数定義ファイルのパス
#' @return DatasetManagerオブジェクト
load_and_prepare_data <- function(config_path = "config/settings.yaml",
                                  variables_path = "config/variables.yaml") {
  
  message("\n========================================")
  message("Starting Data Loading Pipeline")
  message("========================================")
  
  # 1. 設定ファイルの読み込み
  message("\n📄 Loading configuration files...")
  config <- yaml::read_yaml(config_path)
  variables <- yaml::read_yaml(variables_path)
  
  # 2. DatasetManagerの初期化
  dm <- DatasetManager()
  
  # 3. 生データの読み込み (ds00)
  message("\n📊 Loading raw data...")
  df_raw <- load_raw_data(config)
  dm$add_version(df_raw, version = 0)
  
  # 4. 列名の正規化 (ds01)
  message("\n🔧 Normalizing column names...")
  df_normalized <- normalize_column_names(df_raw)
  df_normalized <- apply_column_aliases(df_normalized)
  dm$add_version(df_normalized, version = 1)
  
  # 5. 列名マッピングの適用 (ds02)
  message("\n🔄 Applying column mapping...")
  df_mapped <- apply_column_mapping(df_normalized, variables)
  dm$add_version(df_mapped, version = 2)
  
  # 6. データ型の調整 (ds03)
  message("\n📐 Adjusting data types...")
  df_typed <- adjust_data_types(df_mapped, variables)
  dm$add_version(df_typed, version = 3)
  
  # 7. データ検証
  message("\n✔️ Validating data...")
  
  # 必須列の定義（variables.yamlから取得）
  required_cols <- NULL
  if (!is.null(variables$variable_sets$essential)) {
    required_cols <- variables$variable_sets$essential
  }
  
  validate_data(df_typed, required_cols)
  
  # 8. サマリー表示
  dm$summary()
  
  message("\n✅ Data loading pipeline completed!")
  message("========================================\n")
  
  return(dm)
}

# ========================================================================
# 8. ヘルパー関数（元のスクリプトから移植）
# ========================================================================

#' Excel日付の変換
#' @param x 日付列
#' @return Date型のベクトル
to_date_excel_mixed <- function(x) {
  if (inherits(x, "Date")) return(x)
  
  s <- trimws(as.character(x))
  s[s %in% c("", "NA", "N/A", "na", "n/a")] <- NA
  
  # Excelシリアル値
  num <- suppressWarnings(as.numeric(s))
  dnum <- as.Date(num, origin = "1899-12-30")
  
  # 文字列日付の正規化
  s2 <- s %>%
    stringr::str_replace_all("[.]", "/") %>%
    stringr::str_replace_all("年|月", "/") %>%
    stringr::str_replace_all("日", "") %>%
    stringr::str_replace_all("/", "-")
  
  dchr <- suppressWarnings(lubridate::ymd(s2))
  
  as.Date(dplyr::coalesce(dchr, dnum))
}

#' TRUE/FALSE変換
#' @param x 論理値を表す列
#' @return logical型のベクトル
to_tf <- function(x) {
  z <- trimws(as.character(x))
  z <- chartr("＋－", "+-", z)
  
  rec <- c(
    "TRUE" = "TRUE", "T" = "TRUE", "有" = "TRUE", 
    "Yes" = "TRUE", "yes" = "TRUE", "1" = "TRUE", "+" = "TRUE",
    "FALSE" = "FALSE", "F" = "FALSE", "無" = "FALSE", 
    "No" = "FALSE", "no" = "FALSE", "0" = "FALSE", "-" = "FALSE"
  )
  
  as.logical(rec[z])
}

#' 時間を分に変換
#' @param x 時間を表す列
#' @return 分単位の数値ベクトル
to_minutes_from_time <- function(x) {
  s <- stringr::str_squish(as.character(x))
  s <- stringr::str_replace_all(s, "：", ":")
  
  has_colon <- stringr::str_detect(s, ":")
  
  parts <- stringr::str_split_fixed(s, ":", 3)
  hh <- suppressWarnings(as.numeric(parts[, 1]))
  mm <- suppressWarnings(as.numeric(parts[, 2]))
  ss <- suppressWarnings(as.numeric(parts[, 3]))
  
  from_colon <- ifelse(
    has_colon, 
    hh * 60 + mm + ifelse(is.na(ss), 0, round(ss / 60)), 
    NA_real_
  )
  
  num <- suppressWarnings(as.numeric(s))
  from_frac <- ifelse(!has_colon & !is.na(num), num * 1440, NA_real_)
  
  round(dplyr::coalesce(from_colon, from_frac))
}
