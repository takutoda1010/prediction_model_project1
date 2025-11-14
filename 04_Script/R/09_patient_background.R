#!/usr/bin/env Rscript
# ============================================================================
# 09_patient_background.R
#
# Summarise baseline patient characteristics for the final modelling dataset
# (overall, train, validation, and test splits) using the table1 package.
# Continuous variables are reported as Median (IQR) and categorical variables as
# counts with percentages.
# ============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("table1", quietly = TRUE)) {
    stop("Package 'table1' is required. Please install it before running.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it before running.")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' is required. Please install it before running.")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required. Please install it before running.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required. Please install it before running.")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required. Please install it before running.")
  }
})

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(table1)
library(survival)

# ----------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------

find_project_root <- function(marker = "Prediction model_Project1.Rproj") {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    candidate <- file.path(current, marker)
    if (file.exists(candidate)) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(sprintf("Project root marker '%s' not found from working directory.", marker))
    }
    current <- parent
  }
}

fix_zero_times <- function(data) {
  if (!is.null(data) && "time2y" %in% names(data)) {
    zero_idx <- which(data$time2y <= 0)
    if (length(zero_idx) > 0) {
      data$time2y[zero_idx] <- 0.1
    }
  }
  data
}

clean_names_final <- function(data) {
  if (is.null(data)) {
    return(data)
  }
  names(data) <- names(data) |>
    stringr::str_replace_all(" ", "_") |>
    stringr::str_replace_all("\\+", "_plus") |>
    stringr::str_replace_all("±", "_pm")
  data
}

clean_for_evaluation <- function(data) {
  if (is.null(data)) {
    return(data)
  }

  vars_to_remove <- c(
    "first_visit_dt", "adm_dt",
    "BMI_category", "BMI_categoryNormal", "BMI_categoryOverweight", "BMI_categoryObese",
    "im", "im1", "Pringle",
    "fc_inf_numeric", "sm_numeric", "sf_numeric", "fc_numeric",
    "最大径", "個数", "性別"
  )

  data <- data[, setdiff(names(data), vars_to_remove), drop = FALSE]

  numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_vars <- setdiff(numeric_vars, "recur_date")
  for (var in numeric_vars) {
    if (anyNA(data[[var]])) {
      med_val <- stats::median(data[[var]], na.rm = TRUE)
      if (is.finite(med_val)) {
        data[[var]][is.na(data[[var]])] <- med_val
      }
    }
  }

  factor_vars <- names(data)[vapply(data, function(x) is.factor(x) || is.character(x), logical(1))]
  factor_vars <- setdiff(factor_vars, c("recur_date", "death_cause", "death_cause4"))
  for (var in factor_vars) {
    if (anyNA(data[[var]])) {
      tab <- table(data[[var]], useNA = "no")
      if (length(tab) > 0) {
        mode_val <- names(tab)[which.max(tab)]
        data[[var]][is.na(data[[var]])] <- mode_val
      }
    }
  }

  logical_vars <- names(data)[vapply(data, is.logical, logical(1))]
  for (var in logical_vars) {
    if (anyNA(data[[var]])) {
      data[[var]][is.na(data[[var]])] <- FALSE
    }
  }

  data
}

prepare_clean_split_data <- function(split_data, variables_to_use = NULL) {
  if (is.null(split_data)) {
    stop("split_data is NULL; cannot prepare clean datasets.")
  }

  if (is.null(variables_to_use) && exists("models") && !is.null(attr(models, "train_data"))) {
    train_data_used <- attr(models, "train_data")
    variables_to_use <- setdiff(names(train_data_used), c("surv_obj"))
  }

  clean_dataset <- function(data, name) {
    if (is.null(data)) {
      return(NULL)
    }

    if (!is.null(variables_to_use)) {
      required_vars <- unique(c("time2y", "recur_2y", variables_to_use))
      available_vars <- intersect(required_vars, names(data))
      data <- data[, available_vars, drop = FALSE]
    }

    dup_patterns <- c("TRUETRUE", "FALSEFALSE", "TRUEFALSE", "FALSETRUE")
    for (pattern in dup_patterns) {
      bad_vars <- grep(pattern, names(data), value = TRUE)
      if (length(bad_vars) > 0) {
        data <- data[, setdiff(names(data), bad_vars), drop = FALSE]
      }
    }

    if (all(c("time2y", "recur_2y") %in% names(data))) {
      data$surv_obj <- survival::Surv(
        time = data$time2y,
        event = as.numeric(as.character(data$recur_2y) %in% c("TRUE", "1"))
      )
    }

    data
  }

  result <- list(
    train = clean_dataset(split_data$train, "train"),
    val = clean_dataset(split_data$val, "val"),
    test = clean_dataset(split_data$test, "test"),
    meta = split_data$meta,
    indices = split_data$indices
  )

  class(result) <- c("data_split", "list")
  result
}

coerce_to_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(factor(x, levels = c(FALSE, TRUE), labels = c("No", "Yes")))
  }
  if (is.numeric(x)) {
    ux <- sort(unique(stats::na.omit(x)))
    if (identical(ux, c(0, 1))) {
      return(factor(x, levels = c(0, 1), labels = c("No", "Yes")))
    }
    return(factor(x, levels = ux))
  }
  factor(x)
}

render_median_iqr <- function(x) {
  if (!length(x) || all(is.na(x))) {
    return(c("Median (IQR)" = "NA"))
  }
  qs <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 2)
  c("Median (IQR)" = sprintf("%.2f (%.2f–%.2f)", qs[2], qs[1], qs[3]))
}

render_n_percent <- function(x) {
  s <- table1::stats.default(x)
  if (is.null(dim(s)) || nrow(s) == 0) {
    return(c("" = ""))
  }
  lines <- sprintf("%s (%.1f%%)", format(s[, "freq"], trim = TRUE), s[, "percent"])
  c("" = "", lines)
}

normalize_names <- function(x) {
  gsub("[^a-z]", "", tolower(x))
}

# ----------------------------------------------------------------------------
# Main workflow
# ----------------------------------------------------------------------------

project_root <- find_project_root()
message(sprintf("Project root: %s", project_root))

data_split_path <- file.path(project_root, "03_Output", "models", "data_split.rds")
if (!file.exists(data_split_path)) {
  stop(sprintf("Data split file not found: %s", data_split_path))
}

split_data_raw <- readRDS(data_split_path)
if (!is.list(split_data_raw)) {
  stop("Unexpected format in data_split.rds. Expected a list with train/val/test.")
}

split_data_clean <- split_data_raw
split_data_clean$train <- fix_zero_times(split_data_clean$train)
split_data_clean$val <- fix_zero_times(split_data_clean$val)
split_data_clean$test <- fix_zero_times(split_data_clean$test)

split_data_clean$train <- clean_names_final(split_data_clean$train)
split_data_clean$val <- clean_names_final(split_data_clean$val)
split_data_clean$test <- clean_names_final(split_data_clean$test)

split_data_clean$train <- clean_for_evaluation(split_data_clean$train)
split_data_clean$val <- clean_for_evaluation(split_data_clean$val)
split_data_clean$test <- clean_for_evaluation(split_data_clean$test)

split_data_final <- prepare_clean_split_data(split_data_clean)

set_labels <- c(train = "Train", val = "Validation", test = "Test")

combined_list <- purrr::imap(set_labels, function(label, name) {
  data <- split_data_final[[name]]
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(NULL)
  }
  data$dataset <- factor(label, levels = set_labels)
  data
}) |> purrr::compact()

if (length(combined_list) == 0) {
  stop("No datasets found in the split data.")
}

combined_data <- bind_rows(combined_list)
combined_data$dataset <- factor(combined_data$dataset, levels = set_labels)

if ("surv_obj" %in% names(combined_data)) {
  combined_data$surv_obj <- NULL
}

vars_continuous <- c(
  "age", "alb", "tbil", "ast", "alt", "plt",
  "pt_inr", "afp", "pivka2", "max_diam_cm", "count",
  "ALBIscore", "BMI", "icg_r15"
)

vars_categorical <- c(
  "sex", "hbv", "child_pugh", "liver_damage",
  "stage_diag", "ALBIgrade", "microVI", "macroVI",
  "fc", "fc_inf", "sm", "t", "n", "m", "recur_2y"
)

vars_continuous <- intersect(vars_continuous, names(combined_data))
vars_categorical <- intersect(vars_categorical, names(combined_data))

if (length(vars_continuous) == 0 && length(vars_categorical) == 0) {
  stop("None of the expected variables were found in the combined dataset.")
}

if (length(vars_categorical) > 0) {
  combined_data <- combined_data |>
    mutate(across(all_of(vars_categorical), coerce_to_factor))
}

var_labels <- c(
  "age" = "Age (years)",
  "sex" = "Sex",
  "alb" = "Albumin (g/dL)",
  "tbil" = "Total bilirubin (mg/dL)",
  "ast" = "AST (IU/L)",
  "alt" = "ALT (IU/L)",
  "plt" = "Platelet count (×10⁴/μL)",
  "pt_inr" = "PT-INR",
  "afp" = "AFP (ng/mL)",
  "pivka2" = "PIVKA-II (mAU/mL)",
  "max_diam_cm" = "Maximum tumor diameter (cm)",
  "count" = "Number of tumors",
  "ALBIscore" = "ALBI score",
  "BMI" = "BMI (kg/m²)",
  "icg_r15" = "ICG-R15 (%)",
  "child_pugh" = "Child-Pugh class",
  "liver_damage" = "Liver damage grade",
  "stage_diag" = "Stage",
  "ALBIgrade" = "ALBI grade",
  "microVI" = "Microvascular invasion",
  "macroVI" = "Macrovascular invasion",
  "fc" = "Fibrous capsule",
  "fc_inf" = "FC infiltration",
  "sm" = "Surgical margin",
  "t" = "T stage",
  "n" = "N stage",
  "m" = "M stage",
  "recur_2y" = "2-year recurrence"
)

for (var in intersect(names(var_labels), names(combined_data))) {
  table1::label(combined_data[[var]]) <- var_labels[[var]]
}

table_formula <- reformulate(
  termlabels = c(vars_continuous, vars_categorical),
  response = NULL
)

tbl1 <- table1::table1(
  formula = update(table_formula, ~ . | dataset),
  data = combined_data,
  overall = "Overall",
  render.continuous = render_median_iqr,
  render.categorical = render_n_percent,
  render.missing = NULL
)

tbl1_df <- as.data.frame(tbl1, row.names = FALSE, stringsAsFactors = FALSE)

if (!all(c("label", "level") %in% names(tbl1_df))) {
  stop("Unexpected structure from table1 output; expected 'label' and 'level' columns.")
}

normalized_names <- normalize_names(names(tbl1_df))
overall_col <- names(tbl1_df)[normalized_names == "overall"]
train_col <- names(tbl1_df)[normalized_names %in% c("train", "training")]
val_col <- names(tbl1_df)[normalized_names %in% c("validation", "val")]
test_col <- names(tbl1_df)[normalized_names == "test"]
pvalue_col <- names(tbl1_df)[normalized_names %in% c("pvalue", "pval")]
missing_col <- names(tbl1_df)[normalized_names == "missing"]

selected_cols <- c(
  "Variable",
  overall_col,
  train_col,
  val_col,
  test_col,
  pvalue_col,
  missing_col
)
selected_cols <- selected_cols[selected_cols %in% c("Variable", names(tbl1_df))]
selected_cols <- unique(selected_cols)

patient_background <- tbl1_df |>
  mutate(
    Variable = ifelse(level == "", label, paste0("  ", level))
  ) |>
  select(all_of(selected_cols))

rename_if_present <- function(name, new_name) {
  if (length(name) == 1 && name %in% names(patient_background)) {
    setNames(new_name, name)
  } else {
    NULL
  }
}

col_renames <- c(
  rename_if_present(overall_col, "Overall"),
  rename_if_present(train_col, "Train"),
  rename_if_present(val_col, "Validation"),
  rename_if_present(test_col, "Test"),
  rename_if_present(pvalue_col, "P_value"),
  rename_if_present(missing_col, "Missing")
)

patient_background <- patient_background |>
  rename(!!!col_renames)

output_dir <- file.path(project_root, "03_Output", "tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

output_path <- file.path(output_dir, "table1_patient_background.csv")
readr::write_csv(patient_background, output_path)
message(sprintf("✓ Patient background table saved to %s", output_path))

message("\nPreview of patient background table (first 20 rows):")
print(utils::head(patient_background, 20))

