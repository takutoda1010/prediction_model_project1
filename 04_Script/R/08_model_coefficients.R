knitr::opts_chunk$set(echo = TRUE)

################################################################################
# 08_model_coefficients.R - モデル係数の抽出と集計
#
# Description:
#   保存済みモデル (03_Output/models/all_models.rds) から係数を抽出
#   モデルタイプ (coxph, glm, cv.glmnet) ごとに helper 関数で整形
#   モデル名を含む tidy データフレームを作成し CSV に保存
#
# Author: Takuto Yoshida
# Date: 2024-11-07
################################################################################

# ========================================================================
# 0. パッケージ読み込み
# ========================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(readr)
  library(survival)
  library(glmnet)
})

# ========================================================================
# 1. ヘルパー関数: coxph
# ========================================================================

#' coxph モデルから係数を抽出
#' @param model coxph オブジェクト
#' @param model_name モデル名
#' @return tibble
extract_coefs_coxph <- function(model, model_name) {

  if (is.null(model)) {
    return(tibble(
      model = character(),
      term = character(),
      estimate = double(),
      exp_estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double(),
      conf_low = double(),
      conf_high = double(),
      lambda = double()
    ))
  }

  summary_obj <- summary(model)
  coef_mat <- summary_obj$coefficients

  if (is.null(coef_mat) || nrow(coef_mat) == 0) {
    return(tibble(
      model = character(),
      term = character(),
      estimate = double(),
      exp_estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double(),
      conf_low = double(),
      conf_high = double(),
      lambda = double()
    ))
  }

  conf_mat <- tryCatch({
    suppressWarnings(confint(model))
  }, error = function(e) {
    NULL
  })

  conf_low <- rep(NA_real_, nrow(coef_mat))
  conf_high <- rep(NA_real_, nrow(coef_mat))

  if (!is.null(conf_mat)) {
    conf_low <- as.numeric(conf_mat[, 1])
    conf_high <- as.numeric(conf_mat[, 2])
  }

  tibble(
    model = model_name,
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, "coef"]),
    exp_estimate = as.numeric(coef_mat[, "exp(coef)"]),
    std_error = as.numeric(coef_mat[, "se(coef)"]),
    statistic = as.numeric(coef_mat[, "z"]),
    p_value = as.numeric(coef_mat[, "Pr(>|z|)"]),
    conf_low = conf_low,
    conf_high = conf_high,
    lambda = NA_real_
  )
}

# ========================================================================
# 2. ヘルパー関数: glm
# ========================================================================

#' glm モデルから係数を抽出
#' @param model glm オブジェクト
#' @param model_name モデル名
#' @return tibble
extract_coefs_glm <- function(model, model_name) {

  if (is.null(model)) {
    return(tibble(
      model = character(),
      term = character(),
      estimate = double(),
      exp_estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double(),
      conf_low = double(),
      conf_high = double(),
      lambda = double()
    ))
  }

  summary_obj <- summary(model)
  coef_mat <- summary_obj$coefficients

  if (is.null(coef_mat) || nrow(coef_mat) == 0) {
    return(tibble(
      model = character(),
      term = character(),
      estimate = double(),
      exp_estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double(),
      conf_low = double(),
      conf_high = double(),
      lambda = double()
    ))
  }

  conf_mat <- tryCatch({
    suppressWarnings(confint(model))
  }, error = function(e) {
    NULL
  })

  conf_low <- rep(NA_real_, nrow(coef_mat))
  conf_high <- rep(NA_real_, nrow(coef_mat))

  if (!is.null(conf_mat)) {
    conf_low <- as.numeric(conf_mat[, 1])
    conf_high <- as.numeric(conf_mat[, 2])
  }

  tibble(
    model = model_name,
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, "Estimate"]),
    exp_estimate = exp(as.numeric(coef_mat[, "Estimate"])),
    std_error = as.numeric(coef_mat[, "Std. Error"]),
    statistic = as.numeric(coef_mat[, 3]),
    p_value = as.numeric(coef_mat[, 4]),
    conf_low = conf_low,
    conf_high = conf_high,
    lambda = NA_real_
  )
}

# ========================================================================
# 3. ヘルパー関数: cv.glmnet / glmnet
# ========================================================================

#' cv.glmnet / glmnet モデルから係数を抽出
#' @param model cv.glmnet もしくは glmnet オブジェクト
#' @param model_name モデル名
#' @param lambda_value 使用する lambda 値（NULL の場合は自動）
#' @return tibble
extract_coefs_cv_glmnet <- function(model, model_name, lambda_value = NULL) {

  if (is.null(model)) {
    return(tibble(
      model = character(),
      term = character(),
      estimate = double(),
      exp_estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double(),
      conf_low = double(),
      conf_high = double(),
      lambda = double()
    ))
  }

  lambda_used <- lambda_value

  if (inherits(model, "cv.glmnet")) {
    if (is.null(lambda_used)) {
      lambda_used <- model$lambda.min
    }
  } else if (inherits(model, "glmnet")) {
    if (is.null(lambda_used)) {
      lambda_used <- if (!is.null(model$lambda) && length(model$lambda) > 0) {
        model$lambda[length(model$lambda)]
      } else {
        NA_real_
      }
    }
  }

  coef_mat <- tryCatch({
    as.matrix(stats::coef(model, s = lambda_used))
  }, error = function(e) {
    if (inherits(model, "glmnet")) {
      as.matrix(stats::coef(model))
    } else {
      stop(e)
    }
  })

  if (nrow(coef_mat) == 0) {
    return(tibble(
      model = character(),
      term = character(),
      estimate = double(),
      exp_estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double(),
      conf_low = double(),
      conf_high = double(),
      lambda = double()
    ))
  }

  tibble(
    model = model_name,
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, 1]),
    exp_estimate = exp(as.numeric(coef_mat[, 1])),
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    lambda = lambda_used
  ) %>%
    filter(term != "(Intercept)")
}

# ========================================================================
# 4. ディスパッチ関数
# ========================================================================

#' モデルクラスに応じて適切な extractor を適用
#' @param model モデルオブジェクト
#' @param model_name モデル名
#' @return tibble
extract_coefs <- function(model, model_name) {

  if (inherits(model, "coxph")) {
    return(extract_coefs_coxph(model, model_name))
  }

  if (inherits(model, "glm")) {
    return(extract_coefs_glm(model, model_name))
  }

  if (inherits(model, "cv.glmnet") || inherits(model, "glmnet")) {
    return(extract_coefs_cv_glmnet(model, model_name))
  }

  if (inherits(model, "lasso_cox") && !is.null(model$glmnet_model)) {
    return(extract_coefs_cv_glmnet(model$glmnet_model, model_name, lambda_value = model$lambda_min))
  }

  if (inherits(model, "lasso_cox") && !is.null(model$cv_fit)) {
    return(extract_coefs_cv_glmnet(model$cv_fit, model_name, lambda_value = model$lambda_min))
  }

  if (inherits(model, "univariate_cox") && !is.null(model$model)) {
    return(extract_coefs_coxph(model$model, model_name))
  }

  if (inherits(model, "rfe_cox") && !is.null(model$model)) {
    return(extract_coefs_coxph(model$model, model_name))
  }

  warning(sprintf("Unsupported model class for '%s': %s", model_name, paste(class(model), collapse = ", ")))

  tibble(
    model = character(),
    term = character(),
    estimate = double(),
    exp_estimate = double(),
    std_error = double(),
    statistic = double(),
    p_value = double(),
    conf_low = double(),
    conf_high = double(),
    lambda = double()
  )
}

# ========================================================================
# 5. モデル読み込みと処理
# ========================================================================

models_path <- "03_Output/models/all_models.rds"
output_dir <- "03_Output/tables"
output_path <- file.path(output_dir, "model_coefficients_by_model.csv")

if (!file.exists(models_path)) {
  stop(sprintf("Model file not found: %s", models_path))
}

models <- readRDS(models_path)

if (!is.list(models) || length(models) == 0) {
  stop("Loaded object is not a non-empty list of models")
}

coef_table <- purrr::imap_dfr(models, extract_coefs)

# 列の型と順序を整える
coef_table <- coef_table %>%
  mutate(
    model = as.character(model),
    term = as.character(term),
    estimate = as.numeric(estimate),
    exp_estimate = as.numeric(exp_estimate),
    std_error = as.numeric(std_error),
    statistic = as.numeric(statistic),
    p_value = as.numeric(p_value),
    conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high),
    lambda = as.numeric(lambda)
  ) %>%
  select(model, term, estimate, exp_estimate, std_error, statistic, p_value, conf_low, conf_high, lambda)

# 出力ディレクトリの作成
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

readr::write_csv(coef_table, output_path)

message(sprintf("Saved model coefficients to: %s", output_path))
