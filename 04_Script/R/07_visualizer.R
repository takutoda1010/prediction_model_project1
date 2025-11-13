knitr::opts_chunk$set(echo = TRUE)

################################################################################
# 07_visualizer.R - 可視化処理
# 
# Description: 
#   モデル評価結果の可視化
#   ROC曲線、キャリブレーションプロット、DCA、生存曲線、変数重要度
#   サマリーテーブルの作成
#
# Author: Takuto Yoshida
# Date: 2024-11-07
################################################################################

# ========================================================================
# 1. ROC曲線
# ========================================================================

#' ROC曲線をプロット
#' @param evaluation_results 評価結果
#' @param dataset データセット名（"test", "val", "train"）
#' @param time_point 評価時点
#' @param save_path 保存先パス（NULL時は保存しない）
#' @return ggplotオブジェクト
plot_roc_curves <- function(evaluation_results, 
                           dataset = "test",
                           time_point = 24,
                           save_path = NULL) {
  
  message("\n📊 Plotting ROC curves...")
  
  # データ準備
  roc_data <- data.frame()
  auc_values <- data.frame()
  
  # 各モデルのROCデータ収集
  for (model_name in names(evaluation_results$results)) {
    model_result <- evaluation_results$results[[model_name]]
    
    if (!is.null(model_result) && !is.null(model_result[[dataset]]$time_auc)) {
      roc_obj <- model_result[[dataset]]$time_auc$roc_object
      
      if (!is.null(roc_obj)) {
        # 指定時点のROC
        time_idx <- which(roc_obj$times == time_point)
        
        if (length(time_idx) > 0) {
          # ROC曲線のポイント
          fpr <- c(0, roc_obj$FP[, time_idx], 1)
          tpr <- c(0, roc_obj$TP[, time_idx], 1)
          
          roc_data <- rbind(roc_data, data.frame(
            model = model_name,
            fpr = fpr,
            tpr = tpr
          ))
          
          # AUC値
          auc_val <- roc_obj$AUC[time_idx]
          auc_values <- rbind(auc_values, data.frame(
            model = model_name,
            auc = auc_val
          ))
        }
      }
    }
  }
  
  # AUCでソート
  if (nrow(auc_values) > 0) {
    model_order <- auc_values[order(auc_values$auc, decreasing = TRUE), "model"]
    roc_data$model <- factor(roc_data$model, levels = model_order)
    
    # ラベルにAUCを追加
    auc_labels <- sapply(model_order, function(m) {
      auc <- auc_values[auc_values$model == m, "auc"]
      sprintf("%s (AUC=%.3f)", m, auc)
    })
    levels(roc_data$model) <- auc_labels
  }
  
  # プロット作成
  p <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
    geom_line(size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
    labs(
      title = sprintf("ROC Curves at %d Months - %s Set", time_point, stringr::str_to_title(dataset)),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)",
      color = "Model"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    scale_color_brewer(palette = "Set1")
  
  # 保存
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 8, height = 8, dpi = 300)
    message(sprintf("  ✓ ROC plot saved to: %s", save_path))
  }
  
  return(p)
}

# ========================================================================
# 2. キャリブレーションプロット
# ========================================================================

#' キャリブレーションプロット
#' @param evaluation_results 評価結果
#' @param model_names プロットするモデル名（NULL時は上位3モデル）
#' @param dataset データセット名
#' @param save_path 保存先パス
#' @return ggplotオブジェクト
plot_calibration <- function(evaluation_results,
                            model_names = NULL,
                            dataset = "test",
                            save_path = NULL) {
  
  message("\n📊 Plotting calibration curves...")
  
  # モデル選択
  if (is.null(model_names)) {
    # C-indexの上位3モデル
    summary_df <- evaluation_results$summary
    col_name <- paste0("c_index_", dataset)
    if (col_name %in% names(summary_df)) {
      summary_df <- summary_df[order(summary_df[[col_name]], decreasing = TRUE), ]
      model_names <- head(summary_df$model, 3)
    } else {
      model_names <- names(evaluation_results$results)[1:min(3, length(evaluation_results$results))]
    }
  }
  
  # データ準備
  cal_data <- data.frame()
  
  for (model_name in model_names) {
    model_result <- evaluation_results$results[[model_name]]
    
    if (!is.null(model_result) && !is.null(model_result[[dataset]]$calibration$groups)) {
      groups <- model_result[[dataset]]$calibration$groups
      groups$model <- model_name
      cal_data <- rbind(cal_data, groups)
    }
  }
  
  if (nrow(cal_data) == 0) {
    warning("No calibration data available")
    return(NULL)
  }
  
  # プロット作成
  p <- ggplot(cal_data, aes(x = expected, y = observed)) +
    geom_point(aes(size = n, color = model), alpha = 0.7) +
    geom_smooth(aes(color = model), method = "loess", se = TRUE, alpha = 0.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    scale_x_continuous(limits = c(0, max(cal_data$expected) * 1.1), 
                      labels = scales::percent) +
    scale_y_continuous(limits = c(0, max(cal_data$observed, na.rm = TRUE) * 1.1), 
                      labels = scales::percent) +
    labs(
      title = sprintf("Calibration Plot - %s Set", stringr::str_to_title(dataset)),
      x = "Expected Event Rate",
      y = "Observed Event Rate",
      color = "Model",
      size = "Sample Size"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    scale_color_brewer(palette = "Set1")
  
  # 保存
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 8, height = 6, dpi = 300)
    message(sprintf("  ✓ Calibration plot saved to: %s", save_path))
  }
  
  return(p)
}

# ========================================================================
# 3. Decision Curve Analysis プロット
# ========================================================================

#' DCAプロット
#' @param evaluation_results 評価結果
#' @param model_names プロットするモデル名
#' @param threshold_range 閾値の範囲
#' @param save_path 保存先パス
#' @return ggplotオブジェクト
plot_dca <- function(evaluation_results,
                    model_names = NULL,
                    threshold_range = c(0.05, 0.30),
                    save_path = NULL) {
  
  message("\n📊 Plotting Decision Curve Analysis...")
  
  # モデル選択
  if (is.null(model_names)) {
    # 上位3モデル
    summary_df <- evaluation_results$summary
    summary_df <- summary_df[order(summary_df$c_index_test, decreasing = TRUE), ]
    model_names <- head(summary_df$model, 3)
  }
  
  # データ準備
  dca_data <- data.frame()
  
  for (model_name in model_names) {
    model_result <- evaluation_results$results[[model_name]]
    
    if (!is.null(model_result) && !is.null(model_result$test$dca)) {
      dca <- model_result$test$dca
      dca <- dca[dca$threshold >= threshold_range[1] & 
                 dca$threshold <= threshold_range[2], ]
      
      # Long format
      dca_long <- rbind(
        data.frame(
          model = model_name,
          threshold = dca$threshold,
          net_benefit = dca$net_benefit_model,
          strategy = model_name
        )
      )
      
      dca_data <- rbind(dca_data, dca_long)
    }
  }
  
  # Treat all/none追加（1つのモデルから）
  if (nrow(dca_data) > 0) {
    first_model <- evaluation_results$results[[model_names[1]]]$test$dca
    first_model <- first_model[first_model$threshold >= threshold_range[1] & 
                              first_model$threshold <= threshold_range[2], ]
    
    dca_data <- rbind(
      dca_data,
      data.frame(
        model = "All",
        threshold = first_model$threshold,
        net_benefit = first_model$net_benefit_all,
        strategy = "Treat All"
      ),
      data.frame(
        model = "None",
        threshold = first_model$threshold,
        net_benefit = first_model$net_benefit_none,
        strategy = "Treat None"
      )
    )
  }
  
  # プロット作成
  p <- ggplot(dca_data, aes(x = threshold, y = net_benefit, color = strategy)) +
    geom_line(size = 1.2) +
    scale_x_continuous(limits = threshold_range, labels = scales::percent) +
    labs(
      title = "Decision Curve Analysis",
      x = "Threshold Probability",
      y = "Net Benefit",
      color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(values = c(
      RColorBrewer::brewer.pal(length(model_names), "Set1"),
      "gray50", "gray70"
    ))
  
  # 保存
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 8, height = 6, dpi = 300)
    message(sprintf("  ✓ DCA plot saved to: %s", save_path))
  }
  
  return(p)
}

# ========================================================================
# 4. 生存曲線（リスクグループ別）
# ========================================================================

#' リスクグループ別生存曲線
#' @param model モデルオブジェクト
#' @param data データ
#' @param n_groups リスクグループ数
#' @param save_path 保存先パス
#' @return survminerプロット
plot_survival_curves <- function(model, 
                                data,
                                n_groups = 3,
                                save_path = NULL) {
  
  message("\n📊 Plotting survival curves by risk groups...")
  
  if (!requireNamespace("survminer", quietly = TRUE)) {
    warning("survminer package not available")
    return(NULL)
  }
  
  # リスクスコアの計算
  if (inherits(model, "coxph")) {
    risk_score <- predict(model, newdata = data, type = "risk")
  } else {
    # 予測確率を使用
    risk_score <- get_predictions(model, data, time_point = 24)
  }
  
  # リスクグループの作成
  if (n_groups == 2) {
    risk_groups <- cut(risk_score, 
                      breaks = c(-Inf, median(risk_score, na.rm = TRUE), Inf),
                      labels = c("Low Risk", "High Risk"))
  } else if (n_groups == 3) {
    tertiles <- quantile(risk_score, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    risk_groups <- cut(risk_score, 
                      breaks = tertiles,
                      labels = c("Low Risk", "Intermediate Risk", "High Risk"),
                      include.lowest = TRUE)
  } else {
    quantiles <- quantile(risk_score, 
                         probs = seq(0, 1, length.out = n_groups + 1), 
                         na.rm = TRUE)
    risk_groups <- cut(risk_score, 
                      breaks = quantiles,
                      labels = paste0("Group ", 1:n_groups),
                      include.lowest = TRUE)
  }
  
  data$risk_group <- risk_groups
  
  # Kaplan-Meier生存曲線
  km_fit <- survival::survfit(
    survival::Surv(time2y, recur_2y) ~ risk_group, 
    data = data
  )
  
  # プロット
  p <- survminer::ggsurvplot(
    km_fit,
    data = data,
    risk.table = TRUE,
    risk.table.height = 0.25,
    pval = TRUE,
    pval.method = TRUE,
    conf.int = TRUE,
    xlim = c(0, 24),
    break.time.by = 6,
    xlab = "Time (months)",
    ylab = "Recurrence-free Probability",
    title = "Survival Curves by Risk Groups",
    palette = "jco",
    legend.title = "Risk Group",
    legend.labs = levels(risk_groups),
    ggtheme = theme_minimal(base_size = 12)
  )
  
  # 保存
  if (!is.null(save_path)) {
    pdf(save_path, width = 10, height = 8)
    print(p)
    dev.off()
    message(sprintf("  ✓ Survival plot saved to: %s", save_path))
  }
  
  return(p)
}

# ========================================================================
# 5. 変数重要度プロット
# ========================================================================

#' 変数重要度をプロット
#' @param models モデルリスト
#' @param top_n 表示する上位変数数
#' @param save_path 保存先パス
#' @return ggplotオブジェクト
plot_variable_importance <- function(models, 
                                    top_n = 20,
                                    save_path = NULL) {
  
  message("\n📊 Plotting variable importance...")
  
  # 各モデルから変数重要度を抽出
  importance_data <- data.frame()
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    if (!is.null(model)) {
      # Coxモデル
      if (inherits(model, "coxph")) {
        coef_summary <- summary(model)$coefficients
        importance <- data.frame(
          model = model_name,
          variable = rownames(coef_summary),
          importance = abs(coef_summary[, "z"]),
          coefficient = coef_summary[, "coef"],
          hr = exp(coef_summary[, "coef"])
        )
        
      # LASSOモデル
      } else if (inherits(model, "lasso_cox")) {
        coef_matrix <- as.matrix(coef(model$glmnet_model, s = model$lambda_min))
        non_zero <- which(coef_matrix[, 1] != 0)
        if (length(non_zero) > 0) {
          importance <- data.frame(
            model = model_name,
            variable = rownames(coef_matrix)[non_zero],
            importance = abs(coef_matrix[non_zero, 1]),
            coefficient = coef_matrix[non_zero, 1],
            hr = exp(coef_matrix[non_zero, 1])
          )
        }
        
      # その他のモデル
      } else {
        selected_vars <- attr(model, "selected_vars")
        if (!is.null(selected_vars)) {
          importance <- data.frame(
            model = model_name,
            variable = selected_vars,
            importance = 1,
            coefficient = NA,
            hr = NA
          )
        }
      }
      
      if (exists("importance")) {
        importance_data <- rbind(importance_data, importance)
      }
    }
  }
  
  # 変数の出現頻度を計算
  var_freq <- table(importance_data$variable)
  var_freq <- sort(var_freq, decreasing = TRUE)
  
  # 上位変数を選択
  top_vars <- names(head(var_freq, top_n))
  
  # データフィルタリング
  plot_data <- importance_data[importance_data$variable %in% top_vars, ]
  
  # 平均重要度で並べ替え
  var_order <- aggregate(importance ~ variable, data = plot_data, FUN = mean)
  var_order <- var_order[order(var_order$importance, decreasing = TRUE), ]
  plot_data$variable <- factor(plot_data$variable, levels = rev(var_order$variable))
  
  # プロット作成
  p <- ggplot(plot_data, aes(x = variable, y = importance, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(
      title = sprintf("Top %d Variable Importance Across Models", min(top_n, length(unique(plot_data$variable)))),
      x = "Variable",
      y = "Importance (|z-score| or |coefficient|)",
      fill = "Model"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    scale_fill_brewer(palette = "Set1")
  
  # 保存
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 10, height = 8, dpi = 300)
    message(sprintf("  ✓ Variable importance plot saved to: %s", save_path))
  }
  
  return(p)
}

# ========================================================================
# 6. モデル比較サマリーテーブル
# ========================================================================

#' 美しいサマリーテーブルを作成
#' @param evaluation_results 評価結果
#' @param output_path 保存先パス
#' @return gtテーブルオブジェクト
create_summary_table <- function(evaluation_results, 
                                output_path = NULL) {
  
  message("\n📊 Creating summary table...")
  
  if (!requireNamespace("gt", quietly = TRUE)) {
    warning("gt package not available")
    return(evaluation_results$summary)
  }
  
  # データ準備
  summary_df <- evaluation_results$summary
  
  # 列名を整形
  names(summary_df) <- c(
    "Model", "N Variables",
    "C-index (Train)", "C-index (Val)", "C-index (Test)",
    "Brier Score"
  )
  
  # gtテーブル作成
  gt_table <- gt::gt(summary_df) %>%
    gt::tab_header(
      title = "Model Performance Comparison",
      subtitle = "2-Year Recurrence Prediction"
    ) %>%
    gt::fmt_number(
      columns = c("C-index (Train)", "C-index (Val)", "C-index (Test)", "Brier Score"),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns = "N Variables",
      decimals = 0
    ) %>%
    gt::data_color(
      columns = "C-index (Test)",
      colors = scales::col_numeric(
        palette = c("white", "lightgreen"),
        domain = c(0.5, 1)
      )
    ) %>%
    gt::data_color(
      columns = "Brier Score",
      colors = scales::col_numeric(
        palette = c("lightgreen", "white"),
        domain = c(0, 0.5)
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        columns = "Model",
        rows = 1  # 最良モデル
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Bold indicates best performing model on test set",
      locations = gt::cells_column_labels(columns = "Model")
    )
  
  # 保存
  if (!is.null(output_path)) {
    gt::gtsave(gt_table, output_path)
    message(sprintf("  ✓ Summary table saved to: %s", output_path))
  }
  
  return(gt_table)
}

# ========================================================================
# 7. 統合可視化関数
# ========================================================================

#' 全評価結果を可視化
#' @param evaluation_results 評価結果
#' @param models モデルリスト
#' @param output_dir 出力ディレクトリ
#' @param config 設定リスト
#' @return プロットのリスト
visualize_all_results <- function(evaluation_results,
                                 models,
                                 output_dir = "03_Output/figures",
                                 config = NULL) {
  
  message("\n========================================")
  message("Creating All Visualizations")
  message("========================================")
  
  # 出力ディレクトリ作成
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  plots <- list()
  
  # 1. ROC曲線
  tryCatch({
    plots$roc <- plot_roc_curves(
      evaluation_results,
      dataset = "test",
      time_point = 24,
      save_path = file.path(output_dir, "roc_curves.pdf")
    )
  }, error = function(e) {
    message(sprintf("  ❌ Error in ROC plot: %s", e$message))
  })
  
  # 2. キャリブレーション
  tryCatch({
    plots$calibration <- plot_calibration(
      evaluation_results,
      dataset = "test",
      save_path = file.path(output_dir, "calibration.pdf")
    )
  }, error = function(e) {
    message(sprintf("  ❌ Error in calibration plot: %s", e$message))
  })
  
  # 3. DCA
  tryCatch({
    plots$dca <- plot_dca(
      evaluation_results,
      save_path = file.path(output_dir, "dca.pdf")
    )
  }, error = function(e) {
    message(sprintf("  ❌ Error in DCA plot: %s", e$message))
  })
  
  # 4. 変数重要度
  tryCatch({
    plots$importance <- plot_variable_importance(
      models,
      top_n = 20,
      save_path = file.path(output_dir, "variable_importance.pdf")
    )
  }, error = function(e) {
    message(sprintf("  ❌ Error in importance plot: %s", e$message))
  })
  
  # 5. サマリーテーブル
  tryCatch({
    plots$table <- create_summary_table(
      evaluation_results,
      output_path = file.path(output_dir, "summary_table.html")
    )
  }, error = function(e) {
    message(sprintf("  ❌ Error in summary table: %s", e$message))
  })
  
  # 6. 最良モデルの生存曲線
  if (!is.null(evaluation_results$summary) && nrow(evaluation_results$summary) > 0) {
    best_model_name <- evaluation_results$summary[1, "model"]
    best_model <- models[[best_model_name]]
    
    if (!is.null(best_model)) {
      tryCatch({
        plots$survival <- plot_survival_curves(
          best_model,
          evaluation_results$data$test,
          n_groups = 3,
          save_path = file.path(output_dir, sprintf("survival_curves_%s.pdf", best_model_name))
        )
        message(sprintf("  Created survival curves for best model: %s", best_model_name))
      }, error = function(e) {
        message(sprintf("  ❌ Error in survival plot: %s", e$message))
      })
    }
  }
  
  message("\n✅ Visualization completed!")
  message("========================================\n")
  
  return(plots)
}

# ========================================================================
# 8. パフォーマンス比較プロット
# ========================================================================

#' モデルパフォーマンスの比較プロット
#' @param evaluation_results 評価結果
#' @param save_path 保存先パス
#' @return ggplotオブジェクト
plot_model_comparison <- function(evaluation_results,
                                 save_path = NULL) {
  
  message("\n📊 Plotting model performance comparison...")
  
  # データ準備
  summary_df <- evaluation_results$summary
  
  # Long format変換
  comparison_data <- tidyr::pivot_longer(
    summary_df,
    cols = starts_with("c_index"),
    names_to = "dataset",
    values_to = "c_index"
  )
  
  comparison_data$dataset <- gsub("c_index_", "", comparison_data$dataset)
  comparison_data$dataset <- factor(
    comparison_data$dataset, 
    levels = c("train", "val", "test")
  )
  
  # プロット
  p <- ggplot(comparison_data, aes(x = model, y = c_index, fill = dataset)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      title = "Model Performance Comparison",
      subtitle = "C-index across different datasets",
      x = "Model",
      y = "C-index",
      fill = "Dataset"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    scale_fill_brewer(palette = "Set2")
  
  # 保存
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = 12, height = 6, dpi = 300)
    message(sprintf("  ✓ Comparison plot saved to: %s", save_path))
  }
  
  return(p)
}
