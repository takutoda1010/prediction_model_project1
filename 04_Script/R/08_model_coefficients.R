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

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits, big.mark = ",")
  )
}

format_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))
  )
}

format_ci <- function(low, high, digits = 3) {
  ifelse(
    is.na(low) | is.na(high),
    "",
    paste0(format_num(low, digits), " to ", format_num(high, digits))
  )
}

empty_coef_tibble <- function() {
  tibble(
    model = character(),
    model_type = character(),
    term = character(),
    estimate = double(),
    exp_estimate = double(),
    std_error = double(),
    statistic = double(),
    p_value = double(),
    conf_low = double(),
    conf_high = double(),
    exp_conf_low = double(),
    exp_conf_high = double(),
    lambda = double()
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

quote_terms <- function(terms) {
  if (length(terms) == 0) {
    character()
  } else {
    paste0("`", gsub("`", "\\`", terms, fixed = TRUE), "`")
  }
}

build_surv_object <- function(y) {
  if (inherits(y, "Surv")) {
    return(y)
  }

  y_df <- as.data.frame(y)

  if (ncol(y_df) < 2) {
    return(NULL)
  }

  if (ncol(y_df) == 2) {
    return(survival::Surv(y_df[[1]], y_df[[2]]))
  }

  survival::Surv(y_df[[1]], y_df[[2]], y_df[[3]])
}

# ========================================================================
# 1. ヘルパー関数: coxph
# ========================================================================

#' coxph モデルから係数を抽出
#' @param model coxph オブジェクト
#' @param model_name モデル名
#' @return tibble
extract_coefs_coxph <- function(model, model_name) {

  if (is.null(model)) {
    return(empty_coef_tibble())
  }

  summary_obj <- summary(model)
  coef_mat <- summary_obj$coefficients

  if (is.null(coef_mat) || nrow(coef_mat) == 0) {
    return(empty_coef_tibble())
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
    model_type = "coxph",
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, "coef"]),
    exp_estimate = as.numeric(coef_mat[, "exp(coef)"]),
    std_error = as.numeric(coef_mat[, "se(coef)"]),
    statistic = as.numeric(coef_mat[, "z"]),
    p_value = as.numeric(coef_mat[, "Pr(>|z|)"]),
    conf_low = conf_low,
    conf_high = conf_high,
    exp_conf_low = exp(conf_low),
    exp_conf_high = exp(conf_high),
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
    return(empty_coef_tibble())
  }

  summary_obj <- summary(model)
  coef_mat <- summary_obj$coefficients

  if (is.null(coef_mat) || nrow(coef_mat) == 0) {
    return(empty_coef_tibble())
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
    model_type = "glm",
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, "Estimate"]),
    exp_estimate = exp(as.numeric(coef_mat[, "Estimate"])),
    std_error = as.numeric(coef_mat[, "Std. Error"]),
    statistic = as.numeric(coef_mat[, 3]),
    p_value = as.numeric(coef_mat[, 4]),
    conf_low = conf_low,
    conf_high = conf_high,
    exp_conf_low = exp(conf_low),
    exp_conf_high = exp(conf_high),
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
    return(empty_coef_tibble())
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
    return(empty_coef_tibble())
  }

  tibble(
    model = model_name,
    model_type = "cv.glmnet",
    term = rownames(coef_mat),
    estimate = as.numeric(coef_mat[, 1]),
    exp_estimate = exp(as.numeric(coef_mat[, 1])),
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    exp_conf_low = NA_real_,
    exp_conf_high = NA_real_,
    lambda = lambda_used
  ) %>%
    filter(term != "(Intercept)") %>%
    filter(!is.na(estimate) & !near(estimate, 0))
}

# ========================================================================
# 3-b. ヘルパー関数: lasso_cox
# ========================================================================

fit_lasso_post_coxph <- function(model) {

  selected_vars <- model$selected_vars %||% attr(model, "selected_vars")

  if (is.null(selected_vars) || length(selected_vars) == 0) {
    return(NULL)
  }

  if (is.null(model$X) || is.null(model$y)) {
    return(NULL)
  }

  available_vars <- intersect(colnames(model$X), selected_vars)

  if (length(available_vars) == 0) {
    return(NULL)
  }

  design_matrix <- model$X[, available_vars, drop = FALSE]
  surv_obj <- build_surv_object(model$y)

  if (is.null(surv_obj)) {
    return(NULL)
  }

  design_df <- as.data.frame(design_matrix)
  rhs_terms <- quote_terms(colnames(design_df))

  if (length(rhs_terms) == 0) {
    return(NULL)
  }

  if (attr(surv_obj, "type") %in% c("right", "counting")) {
    surv_df <- as.data.frame(surv_obj)
  } else {
    surv_df <- as.data.frame(model$y)
  }

  if (ncol(surv_df) == 2) {
    fitting_df <- cbind(design_df, time = surv_df[[1]], status = surv_df[[2]])
    formula <- as.formula(
      paste0("survival::Surv(time, status) ~ ", paste(rhs_terms, collapse = " + "))
    )
  } else if (ncol(surv_df) >= 3) {
    fitting_df <- cbind(
      design_df,
      time_start = surv_df[[1]],
      time_stop = surv_df[[2]],
      status = surv_df[[3]]
    )
    formula <- as.formula(
      paste0(
        "survival::Surv(time_start, time_stop, status) ~ ",
        paste(rhs_terms, collapse = " + ")
      )
    )
  } else {
    return(NULL)
  }

  tryCatch(
    survival::coxph(formula, data = fitting_df),
    error = function(e) NULL
  )
}

#' lasso_cox モデルから係数を抽出
#' @param model lasso_cox オブジェクト
#' @param model_name モデル名
#' @return tibble
extract_coefs_lasso_cox <- function(model, model_name) {

  if (is.null(model)) {
    return(empty_coef_tibble())
  }

  lambda_used <- model$lambda_min %||% model$lambda %||%
    if (!is.null(model$cv_fit) && !is.null(model$cv_fit$lambda.min)) {
      model$cv_fit$lambda.min
    } else if (!is.null(model$glmnet_model) && !is.null(model$glmnet_model$lambda)) {
      tail(model$glmnet_model$lambda, 1)
    } else {
      NA_real_
    }

  if (!is.null(model$fit) && inherits(model$fit, "coxph")) {
    return(
      extract_coefs_coxph(model$fit, model_name) %>%
        mutate(
          model_type = "lasso_post_coxph",
          lambda = as.numeric(lambda_used)
        )
    )
  }

  if (!is.null(model$model) && inherits(model$model, "coxph")) {
    return(
      extract_coefs_coxph(model$model, model_name) %>%
        mutate(
          model_type = "lasso_post_coxph",
          lambda = as.numeric(lambda_used)
        )
    )
  }

  post_fit <- fit_lasso_post_coxph(model)

  if (!is.null(post_fit)) {
    return(
      extract_coefs_coxph(post_fit, model_name) %>%
        mutate(
          model_type = "lasso_post_coxph",
          lambda = as.numeric(lambda_used)
        )
    )
  }

  if (!is.null(model$glmnet_model)) {
    return(
      extract_coefs_cv_glmnet(
        model$glmnet_model,
        model_name,
        lambda_value = lambda_used
      ) %>%
        mutate(
          model_type = "lasso_glmnet",
          lambda = as.numeric(lambda_used)
        )
    )
  }

  if (!is.null(model$cv_fit)) {
    return(
      extract_coefs_cv_glmnet(
        model$cv_fit,
        model_name,
        lambda_value = lambda_used
      ) %>%
        mutate(
          model_type = "lasso_glmnet",
          lambda = as.numeric(lambda_used)
        )
    )
  }

  warning(sprintf("LASSO model '%s' does not contain recognizable coefficient data", model_name))
  empty_coef_tibble()
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

  if (inherits(model, "lasso_cox")) {
    return(extract_coefs_lasso_cox(model, model_name))
  }

  if (inherits(model, "univariate_cox") && !is.null(model$model)) {
    return(extract_coefs_coxph(model$model, model_name))
  }

  if (inherits(model, "rfe_cox") && !is.null(model$model)) {
    return(extract_coefs_coxph(model$model, model_name))
  }

  warning(sprintf("Unsupported model class for '%s': %s", model_name, paste(class(model), collapse = ", ")))

  empty_coef_tibble()
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
    model_type = as.character(model_type),
    term = as.character(term),
    estimate = as.numeric(estimate),
    exp_estimate = as.numeric(exp_estimate),
    std_error = as.numeric(std_error),
    statistic = as.numeric(statistic),
    p_value = as.numeric(p_value),
    conf_low = as.numeric(conf_low),
    conf_high = as.numeric(conf_high),
    exp_conf_low = as.numeric(exp_conf_low),
    exp_conf_high = as.numeric(exp_conf_high),
    lambda = as.numeric(lambda)
  ) %>%
  select(model, model_type, term, estimate, exp_estimate, std_error, statistic, p_value, conf_low, conf_high, exp_conf_low, exp_conf_high, lambda)

pretty_table <- coef_table %>%
  mutate(
    Beta = format_num(estimate),
    SE = format_num(std_error),
    Statistic = format_num(statistic),
    `P-value` = format_p(p_value),
    `95% CI (Beta)` = format_ci(conf_low, conf_high),
    `Exp(Beta)` = format_num(exp_estimate),
    `95% CI (Exp(Beta))` = format_ci(exp_conf_low, exp_conf_high),
    Lambda = format_num(lambda, digits = 5)
  ) %>%
  select(
    Model = model,
    `Model Type` = model_type,
    Term = term,
    Beta,
    SE,
    Statistic,
    `P-value`,
    `95% CI (Beta)`,
    `Exp(Beta)`,
    `95% CI (Exp(Beta))`,
    Lambda
  )

# 出力ディレクトリの作成
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

readr::write_csv(pretty_table, output_path)

message(sprintf("Saved model coefficients to: %s", output_path))
