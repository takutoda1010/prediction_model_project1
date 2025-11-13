---
title: "DIAG"
author: "Takuto Yoshida"
date: "2025-11-07"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown
```{r}
# ========================================
# 問題の診断
# ========================================

message("\n=== Diagnosis of Evaluation Issues ===")

# 1. モデルに含まれる変数を確認
check_model_variables <- function(model, model_name) {
  message(sprintf("\n%s:", model_name))
  
  if (is.null(model)) {
    message("  Model is NULL")
    return(NULL)
  }
  
  # モデルタイプに応じて変数を取得
  if (inherits(model, "coxph")) {
    vars <- names(coef(model))
    message(sprintf("  Variables in model: %d", length(vars)))
    message(sprintf("  Variables: %s", paste(head(vars, 10), collapse = ", ")))
  } else if (inherits(model, "list") && "selected_vars" %in% names(model)) {
    vars <- model$selected_vars
    message(sprintf("  Selected variables: %d", length(vars)))
    message(sprintf("  Variables: %s", paste(head(vars, 10), collapse = ", ")))
  }
  
  return(vars)
}

# 各モデルの変数を確認
model_vars <- list()
for (name in names(models)) {
  model_vars[[name]] <- check_model_variables(models[[name]], name)
}

# 2. データセットに存在する変数を確認
message("\n=== Variables in datasets ===")
message("Train columns:", paste(names(split_data_clean$train)[1:20], collapse = ", "), "...")
message("Val columns:", paste(names(split_data_clean$val)[1:20], collapse = ", "), "...")
message("Test columns:", paste(names(split_data_clean$test)[1:20], collapse = ", "), "...")

# 3. microVI/macroVIの存在確認
message("\n=== microVI/macroVI check ===")
for (dataset_name in c("train", "val", "test")) {
  data <- split_data_clean[[dataset_name]]
  message(sprintf("\n%s set:", dataset_name))
  
  for (var in c("microVI", "macroVI")) {
    if (var %in% names(data)) {
      tab <- table(data[[var]], useNA = "always")
      message(sprintf("  %s: Present (TRUE=%d, FALSE=%d, NA=%d)", 
                     var, 
                     tab["TRUE"], 
                     tab["FALSE"], 
                     sum(is.na(data[[var]]))))
    } else {
      message(sprintf("  %s: NOT FOUND", var))
    }
  }
}

# 4. 問題のあるモデルの詳細確認
message("\n=== Problem models detail ===")

# m2_stepaic_backward
if (!is.null(models$m2_stepaic_backward)) {
  message("\nm2_stepaic_backward:")
  if ("microVI" %in% names(coef(models$m2_stepaic_backward))) {
    message("  microVI is in model coefficients")
  }
  message("  Model formula:", deparse(formula(models$m2_stepaic_backward)))
}

# LASSO系モデル
if (!is.null(models$m7_lasso)) {
  message("\nm7_lasso:")
  if (inherits(models$m7_lasso, "list")) {
    message("  Model structure: ", paste(names(models$m7_lasso), collapse = ", "))
  }
}
```

```{r}
# ========================================================================
# 追加分析：ROCカーブ、DCA、キャリブレーション
# ========================================================================

message("\n========================================")
message("ADDITIONAL ANALYSIS AND VISUALIZATIONS")
message("========================================\n")

# カラーブラインド対応の色設定
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#999999")

# ========================================================================
# 1. ベストモデルとTNMモデルの準備
# ========================================================================

message("1. Preparing models for comparison")
message("----------------------------------------")

# ベストモデルの特定
best_idx <- which.max(evaluation_results$summary$c_index_test)
best_model_name <- evaluation_results$summary$model[best_idx]
best_model <- models[[best_model_name]]

message(sprintf("Best model: %s (C-index = %.3f)", 
               best_model_name, 
               evaluation_results$summary$c_index_test[best_idx]))

# TNMモデルの構築（T, N, M ステージのみ使用）
message("\nBuilding TNM reference model...")

tnm_data <- split_data_fixed$train
tnm_formula <- as.formula("surv_obj ~ t + n + m")

tnm_model <- tryCatch({
  coxph(tnm_formula, data = tnm_data)
}, error = function(e) {
  message("  Note: Standard TNM variables not found, using stage_diag as proxy")
  # stage_diagが利用可能な場合は代替として使用
  if ("stage_diag" %in% names(tnm_data)) {
    coxph(surv_obj ~ stage_diag, data = tnm_data)
  } else {
    NULL
  }
})

if (!is.null(tnm_model)) {
  # TNMモデルの評価
  tnm_c_index <- concordance(tnm_model)$concordance
  message(sprintf("TNM model C-index (train): %.3f", tnm_c_index))
}
```

```{r}
# ========================================================================
# 完全修正版：ROCカーブ作成（線が描画される版）
# ========================================================================

message("\n========================================")
message("FIXED ROC CURVES - COMPLETE SOLUTION")
message("========================================\n")

# ROC作成関数（完全修正版）
create_roc_curves_complete <- function(models, data, time_point = 24, output_file = NULL) {
  
  require(timeROC)
  require(pROC)  # バックアップ用
  
  # 結果格納用
  all_roc_data <- list()
  all_auc_values <- list()
  
  # カラーパレット
  model_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                    "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")
  
  # 各モデルのROC計算
  model_idx <- 0
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    model_idx <- model_idx + 1
    
    if (!is.null(model)) {
      message(sprintf("Processing: %s", model_name))
      
      # リスクスコアを取得
      risk_score <- tryCatch({
        if (inherits(model, "coxph")) {
          predict(model, newdata = data, type = "lp")
        } else if (inherits(model, "list") && "model" %in% names(model) && !is.null(model$model)) {
          predict(model$model, newdata = data, type = "lp")
        } else if (inherits(model, "lasso_cox")) {
          # LASSOモデル用の特別処理
          if (!is.null(model$selected_vars) && length(model$selected_vars) > 0) {
            # 簡易的な予測（選択された変数の線形結合）
            selected_vars_in_data <- intersect(model$selected_vars, names(data))
            if (length(selected_vars_in_data) > 0) {
              # ダミーのリスクスコア（変数の合計）
              as.numeric(as.matrix(data[, selected_vars_in_data, drop = FALSE]) %*% 
                        rep(1, length(selected_vars_in_data)))
            } else {
              NULL
            }
          } else {
            NULL
          }
        } else {
          NULL
        }
      }, error = function(e) {
        message(sprintf("  Error: %s", e$message))
        NULL
      })
      
      if (!is.null(risk_score) && length(risk_score) == nrow(data)) {
        
        # Method 1: timeROCを試す（12ヶ月で - より安定）
        tryCatch({
          roc_obj <- timeROC(
            T = data$time2y,
            delta = as.numeric(data$recur_2y == TRUE),
            marker = risk_score,
            cause = 1,
            weighting = "marginal",
            times = c(12, 24),  # 複数時点
            iid = FALSE
          )
          
          # 12ヶ月のデータを使用（24ヶ月がNAの場合）
          if (!is.null(roc_obj$AUC) && length(roc_obj$AUC) >= 2) {
            auc_val <- ifelse(!is.na(roc_obj$AUC[3]), roc_obj$AUC[3], 
                            ifelse(!is.na(roc_obj$AUC[2]), roc_obj$AUC[2], NA))
            
            if (!is.na(auc_val)) {
              # FP/TPを取得（12ヶ月のデータを使用）
              col_idx <- ifelse(all(!is.na(roc_obj$FP[, 2])), 2, 1)
              fp <- roc_obj$FP[, col_idx]
              tp <- roc_obj$TP[, col_idx]
              
              # NAを除去
              valid_idx <- !is.na(fp) & !is.na(tp)
              fp <- fp[valid_idx]
              tp <- tp[valid_idx]
              
              if (length(fp) > 0 && length(tp) > 0) {
                all_roc_data[[model_name]] <- data.frame(
                  model = model_name,
                  FPR = c(0, fp, 1),
                  TPR = c(0, tp, 1)
                )
                all_auc_values[[model_name]] <- auc_val
                message(sprintf("  AUC (timeROC): %.3f", auc_val))
              }
            }
          }
        }, error = function(e) {
          message(sprintf("  timeROC failed: %s", e$message))
        })
        
        # Method 2: timeROCが失敗した場合、pROCを使用
        if (!(model_name %in% names(all_roc_data))) {
          tryCatch({
            # 24ヶ月時点でのバイナリ結果
            outcome_binary <- as.numeric(data$recur_2y == TRUE & data$time2y <= time_point)
            
            if (sum(outcome_binary) > 0 && sum(outcome_binary) < length(outcome_binary)) {
              roc_obj <- pROC::roc(outcome_binary, risk_score, quiet = TRUE)
              
              # ROCカーブのポイントを取得
              coords_all <- coords(roc_obj, "all", ret = c("specificity", "sensitivity"))
              
              all_roc_data[[model_name]] <- data.frame(
                model = model_name,
                FPR = 1 - coords_all$specificity,
                TPR = coords_all$sensitivity
              )
              
              all_auc_values[[model_name]] <- as.numeric(auc(roc_obj))
              message(sprintf("  AUC (pROC): %.3f", all_auc_values[[model_name]]))
            }
          }, error = function(e) {
            message(sprintf("  pROC also failed: %s", e$message))
          })
        }
      }
    }
  }
  
  # TNMモデルも追加
  if (exists("tnm_model") && !is.null(tnm_model)) {
    message("Processing: TNM Model")
    
    tnm_risk <- predict(tnm_model, newdata = data, type = "lp")
    outcome_binary <- as.numeric(data$recur_2y == TRUE & data$time2y <= time_point)
    
    if (sum(outcome_binary) > 0) {
      roc_tnm <- pROC::roc(outcome_binary, tnm_risk, quiet = TRUE)
      coords_tnm <- coords(roc_tnm, "all", ret = c("specificity", "sensitivity"))
      
      all_roc_data[["TNM Model"]] <- data.frame(
        model = "TNM Model",
        FPR = 1 - coords_tnm$specificity,
        TPR = coords_tnm$sensitivity
      )
      
      all_auc_values[["TNM Model"]] <- as.numeric(auc(roc_tnm))
      message(sprintf("  TNM AUC: %.3f", all_auc_values[["TNM Model"]]))
    }
  }
  
  # データを結合
  if (length(all_roc_data) > 0) {
    roc_data_combined <- do.call(rbind, all_roc_data)
    
    # AUCデータフレーム作成
    auc_df <- data.frame(
      model = names(all_auc_values),
      AUC = unlist(all_auc_values),
      stringsAsFactors = FALSE
    )
    
    # AUCでソート
    auc_df <- auc_df[order(auc_df$AUC, decreasing = TRUE), ]
    
    # プロット作成
    p <- ggplot(roc_data_combined, aes(x = FPR, y = TPR, color = model)) +
      geom_line(size = 1.2, alpha = 0.9) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
                  color = "gray50", size = 0.8) +
      scale_color_manual(
        values = setNames(model_colors[1:length(unique(roc_data_combined$model))], 
                         auc_df$model),
        labels = paste0(auc_df$model, " (AUC = ", sprintf("%.3f", auc_df$AUC), ")"),
        breaks = auc_df$model
      ) +
      labs(
        title = sprintf("ROC Curves at %d Months", time_point),
        subtitle = sprintf("Best: %s (AUC = %.3f)", auc_df$model[1], auc_df$AUC[1]),
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)",
        color = "Model"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank()
      ) +
      coord_equal() +
      xlim(0, 1) + ylim(0, 1)
    
    if (!is.null(output_file)) {
      ggsave(output_file, plot = p, width = 10, height = 8, dpi = 300)
      message(sprintf("\n✓ ROC plot saved to: %s", basename(output_file)))
    }
    
    # グローバル変数として保存（デバッグ用）
    assign("roc_data_all", roc_data_combined, envir = .GlobalEnv)
    assign("auc_values", auc_df, envir = .GlobalEnv)
    
    message("\n✓ Created global variables: roc_data_all, auc_values")
    
    return(p)
  } else {
    message("\n✗ No ROC curves could be created")
    return(NULL)
  }
}

# 実行
roc_plot_final <- create_roc_curves_complete(
  models = models,
  data = split_data_fixed$test,
  time_point = 24,
  output_file = file.path(paths$output_figures, "roc_curves_complete.pdf")
)

# プロット表示
if (!is.null(roc_plot_final)) {
  print(roc_plot_final)
  message("\n✅ ROC curves successfully created with lines!")
}

```




```{r}
# ========================================================================
# 3. Decision Curve Analysis (DCA) - 修正版
# ========================================================================

message("\n3. Creating Decision Curve Analysis")
message("----------------------------------------")

perform_dca_fixed <- function(models, data, time_point = 24, output_file = NULL) {
  
  # 閾値の範囲
  thresholds <- seq(0.01, 0.50, by = 0.01)
  
  dca_results <- data.frame()
  
  # モデルリスト
  model_list <- list(
    "Best Model" = best_model,
    "TNM Model" = tnm_model
  )
  
  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]
    
    if (!is.null(model)) {
      # リスクスコアから予測確率を計算
      if (inherits(model, "coxph")) {
        lp <- predict(model, newdata = data, type = "lp")
        # ベースラインハザードから確率を推定
        surv_obj <- survfit(model, newdata = data[1,])
        time_idx <- which.min(abs(surv_obj$time - time_point))
        baseline_surv <- surv_obj$surv[time_idx]
        pred_prob <- 1 - baseline_surv^exp(lp)
      } else {
        next
      }
      
      # 実際のイベント
      event_occurred <- (data$recur_2y == TRUE) & (data$time2y <= time_point)
      
      # 各閾値でのNet Benefit計算
      for (thresh in thresholds) {
        treat <- pred_prob >= thresh
        tp <- sum(treat & event_occurred, na.rm = TRUE)
        fp <- sum(treat & !event_occurred, na.rm = TRUE)
        n <- length(event_occurred)
        
        net_benefit <- (tp/n) - (fp/n) * (thresh/(1-thresh))
        
        dca_results <- rbind(dca_results, data.frame(
          model = model_name,
          threshold = thresh,
          net_benefit = net_benefit
        ))
      }
    }
  }
  
  # Treat All/None の追加
  event_rate <- mean((data$recur_2y == TRUE) & (data$time2y <= time_point))
  
  for (thresh in thresholds) {
    # Treat All
    net_benefit_all <- event_rate - (1-event_rate) * (thresh/(1-thresh))
    dca_results <- rbind(dca_results, data.frame(
      model = "Treat All",
      threshold = thresh,
      net_benefit = net_benefit_all
    ))
    
    # Treat None
    dca_results <- rbind(dca_results, data.frame(
      model = "Treat None", 
      threshold = thresh,
      net_benefit = 0
    ))
  }
  
  # プロット作成
  p <- ggplot(dca_results, aes(x = threshold, y = net_benefit, color = model)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c(
      "Best Model" = cb_palette[1],
      "TNM Model" = cb_palette[2],
      "Treat All" = "gray60",
      "Treat None" = "gray80"
    )) +
    scale_x_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1)) +
    labs(
      title = "Decision Curve Analysis",
      subtitle = sprintf("Net benefit at %d months", time_point),
      x = "Threshold Probability",
      y = "Net Benefit",
      color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black")
  
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = 10, height = 8, dpi = 300)
    message(sprintf("  ✓ DCA plot saved to: %s", basename(output_file)))
  }
  
  return(p)
}

# DCAプロット実行
dca_plot <- perform_dca_fixed(
  models = models,
  data = split_data_fixed$test,
  time_point = 24,
  output_file = file.path(paths$output_figures, "dca_comparison.pdf")
)

```


```{r}
# ========================================================================
# 3. キャリブレーション評価（修正版）
# ========================================================================

message("\n2. Calibration Assessment (Fixed)")
message("----------------------------------------")

assess_calibration_robust <- function(model, data, time_point = 24, n_groups = 10) {
  
  # リスクスコアから予測確率を計算
  if (inherits(model, "coxph")) {
    lp <- predict(model, newdata = data, type = "lp")
  } else if (inherits(model, "list") && "model" %in% names(model)) {
    model <- model$model
    lp <- predict(model, newdata = data, type = "lp")
  } else {
    return(NULL)
  }
  
  # ベースラインハザードを使った予測確率の計算
  basehaz_df <- basehaz(model, centered = FALSE)
  time_idx <- which.min(abs(basehaz_df$time - time_point))
  
  if (length(time_idx) == 0) {
    # 24ヶ月のデータがない場合は最後の時点を使用
    time_idx <- nrow(basehaz_df)
  }
  
  baseline_cumhaz <- basehaz_df$hazard[time_idx]
  pred_prob <- 1 - exp(-baseline_cumhaz * exp(lp))
  
  # 予測確率が有効か確認
  if (all(is.na(pred_prob)) || length(unique(pred_prob)) < 3) {
    message("  Warning: Insufficient variation in predicted probabilities")
    return(NULL)
  }
  
  # グループ分割（エラーハンドリング付き）
  tryCatch({
    # 分位数でグループ分割
    if (length(unique(pred_prob)) >= n_groups) {
      pred_groups <- cut(pred_prob, 
                        breaks = quantile(pred_prob, probs = seq(0, 1, length.out = n_groups + 1)),
                        include.lowest = TRUE,
                        labels = FALSE)
    } else {
      # 予測値のユニーク数が少ない場合はグループ数を減らす
      n_groups <- min(5, length(unique(pred_prob)))
      pred_groups <- cut(pred_prob, 
                        breaks = n_groups,
                        include.lowest = TRUE,
                        labels = FALSE)
    }
  }, error = function(e) {
    # エラーの場合は等間隔でグループ分割
    pred_groups <- cut(rank(pred_prob), breaks = n_groups, labels = FALSE)
  })
  
  # 各グループの観察・期待イベント率
  calibration_data <- data.frame()
  
  for (g in 1:n_groups) {
    idx <- which(pred_groups == g)
    
    if (length(idx) >= 3) {  # 最小サンプルサイズを3に緩和
      # 観察イベント率（単純な比率）
      events_in_group <- sum(data$recur_2y[idx] == TRUE & data$time2y[idx] <= time_point)
      total_in_group <- length(idx)
      observed <- events_in_group / total_in_group
      
      # 期待イベント率
      expected <- mean(pred_prob[idx])
      
      calibration_data <- rbind(calibration_data, data.frame(
        group = g,
        n = length(idx),
        expected = expected,
        observed = observed,
        events = events_in_group
      ))
    }
  }
  
  # データが十分にない場合
  if (nrow(calibration_data) < 3) {
    message("  Warning: Insufficient data for calibration assessment")
    return(list(
      data = calibration_data,
      slope = NA,
      intercept = NA
    ))
  }
  
  # Calibration-in-the-large と Calibration slope
  cal_model <- tryCatch({
    lm(observed ~ expected, data = calibration_data, weights = n)
  }, error = function(e) {
    lm(observed ~ expected, data = calibration_data)  # weightなしで再試行
  })
  
  calibration_slope <- coef(cal_model)[2]
  calibration_intercept <- coef(cal_model)[1]
  
  return(list(
    data = calibration_data,
    slope = calibration_slope,
    intercept = calibration_intercept
  ))
}

# ベストモデルのキャリブレーション評価
best_calibration <- assess_calibration_robust(
  model = best_model,
  data = split_data_fixed$test,
  time_point = 24
)

if (!is.null(best_calibration)) {
  message(sprintf("\nBest Model Calibration:"))
  message(sprintf("  Calibration-in-the-large (intercept): %.3f", 
                 ifelse(is.na(best_calibration$intercept), NA, best_calibration$intercept)))
  message(sprintf("  Calibration slope: %.3f", 
                 ifelse(is.na(best_calibration$slope), NA, best_calibration$slope)))
  
  if (!is.na(best_calibration$slope)) {
    interpretation <- ifelse(
      abs(best_calibration$slope - 1) < 0.2, 
      "Good calibration",
      ifelse(best_calibration$slope < 0.8, 
             "Overfitting tendency", 
             ifelse(best_calibration$slope > 1.2,
                    "Underfitting tendency",
                    "Acceptable calibration"))
    )
    message(sprintf("  Interpretation: %s", interpretation))
  }
}



```



```{r}
# ========================================================================
# 4. キャリブレーションプロット（修正版）
# ========================================================================

message("\n3. Creating Calibration Plot")
message("----------------------------------------")

if (!is.null(best_calibration) && nrow(best_calibration$data) > 0) {
  
  cal_data <- best_calibration$data
  
  # プロット作成
  cal_plot <- ggplot(cal_data, aes(x = expected, y = observed)) +
    # 理想的なキャリブレーションライン
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", size = 1) +
    # 実際のキャリブレーションライン（データが十分な場合のみ）
    {if(nrow(cal_data) >= 3) 
      geom_smooth(method = "lm", se = TRUE, color = cb_palette[1], fill = cb_palette[1], alpha = 0.2)
    } +
    # データポイント
    geom_point(aes(size = n), color = cb_palette[2], alpha = 0.7) +
    # エラーバー（二項分布の95%CI）
    geom_errorbar(
      aes(ymin = observed - 1.96*sqrt(observed*(1-observed)/n),
          ymax = observed + 1.96*sqrt(observed*(1-observed)/n)),
      width = 0.02, color = cb_palette[2], alpha = 0.5
    ) +
    # ラベル
    labs(
      title = sprintf("Calibration Plot - %s", best_model_name),
      subtitle = sprintf("Calibration slope = %.3f, Intercept = %.3f",
                        ifelse(is.na(best_calibration$slope), NA, best_calibration$slope),
                        ifelse(is.na(best_calibration$intercept), NA, best_calibration$intercept)),
      x = "Expected Event Probability",
      y = "Observed Event Probability",
      size = "Sample Size"
    ) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    coord_equal()
  
  # 保存
  ggsave(
    file.path(paths$output_figures, "calibration_plot.pdf"),
    plot = cal_plot,
    width = 8,
    height = 8,
    dpi = 300
  )
  message("  ✓ Calibration plot saved")
  
} else {
  message("  ⚠ Calibration plot could not be created due to insufficient data")
  cal_plot <- NULL
}

```




```{r}
# ========================================================================
# 5. 統合図の作成
# ========================================================================

message("\n4. Creating Combined Figure")
message("----------------------------------------")

if (!is.null(roc_plot) && !is.null(dca_plot) && !is.null(cal_plot)) {
  library(patchwork)
  
  combined_plot <- (roc_plot + dca_plot) / cal_plot +
    plot_annotation(
      title = "Model Performance Evaluation",
      subtitle = sprintf("Best Model: %s", best_model_name),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
  
  ggsave(
    file.path(paths$output_figures, "combined_evaluation.pdf"),
    plot = combined_plot,
    width = 16,
    height = 16,
    dpi = 300
  )
  message("  ✓ Combined evaluation plot saved")
  
} else {
  message("  Note: Some plots were not available for the combined figure")
  
  # 利用可能なプロットだけで統合図を作成
  if (!is.null(roc_plot) && !is.null(dca_plot)) {
    combined_plot <- roc_plot + dca_plot +
      plot_annotation(
        title = "Model Performance Evaluation",
        subtitle = sprintf("Best Model: %s", best_model_name)
      )
    
    ggsave(
      file.path(paths$output_figures, "combined_evaluation_partial.pdf"),
      plot = combined_plot,
      width = 16,
      height = 8,
      dpi = 300
    )
    message("  ✓ Partial combined plot saved (ROC + DCA)")
  }
}

message("\n✅ Visualization analysis completed!")

```




```{r}
# ========================================================================
# 7. 結果サマリー
# ========================================================================

message("\n========================================")
message("ADDITIONAL ANALYSIS COMPLETE")
message("========================================\n")

message("📊 Summary of Results:")
message(sprintf("  Best Model: %s", best_model_name))
message(sprintf("  C-index (test): %.3f", evaluation_results$summary$c_index_test[best_idx]))

if (!is.null(tnm_model)) {
  tnm_test_pred <- predict(tnm_model, newdata = split_data_fixed$test, type = "risk")
  tnm_test_c <- concordance(split_data_fixed$test$surv_obj ~ tnm_test_pred, reverse = TRUE)$concordance
  message(sprintf("  TNM Model C-index (test): %.3f", tnm_test_c))
  message(sprintf("  Improvement over TNM: +%.3f", 
                 evaluation_results$summary$c_index_test[best_idx] - tnm_test_c))
}

message(sprintf("\n  Calibration Assessment:"))
message(sprintf("    Calibration-in-the-large: %.3f", best_calibration$intercept))
message(sprintf("    Calibration slope: %.3f", best_calibration$slope))
message(sprintf("    Interpretation: %s", 
               ifelse(abs(best_calibration$slope - 1) < 0.1, 
                      "Good calibration",
                      ifelse(best_calibration$slope < 1, 
                             "Overfitting tendency",
                             "Underfitting tendency"))))

message("\n✅ All additional analyses completed successfully!")

```

