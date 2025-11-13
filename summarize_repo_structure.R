# ==== Script: summarize_repo_structure.R ====

# (1) パッケージ
if (!require("fs")) install.packages("fs")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

library(fs)
library(dplyr)
library(stringr)

# (2) 現在のプロジェクトルートを取得
root <- getwd()
message("📁 Project root: ", root)

# (3) ディレクトリ構成を一覧
repo_tree <- dir_tree(root, recurse = TRUE, max_depth = 3)

# (4) フォルダ別のファイルサマリー
repo_summary <- tibble(
  file = dir_ls(root, recurse = TRUE, type = "file", regexp = "\\.(R|Rmd|csv|xlsx|png|pdf|txt|rds)$")
) %>%
  mutate(
    folder = str_remove(file, paste0("^", root, "/")),
    folder = str_extract(folder, "^[^/]+")
  ) %>%
  group_by(folder) %>%
  summarise(
    n_files = n(),
    examples = paste0(basename(head(file, 5)), collapse = ", ")
  )

# (5) 出力
cat("\n\n===== Directory Tree (depth=3) =====\n")
print(repo_tree)

cat("\n\n===== Folder Summary =====\n")
print(repo_summary, n = Inf)

# (6) 特にスクリプト/出力フォルダを詳しく
cat("\n\n===== Scripts in 04_Script =====\n")
print(dir_ls(file.path(root, "04_Script"), recurse = TRUE, regexp = "\\.(R|Rmd)$"))

cat("\n\n===== Outputs in 03_Output =====\n")
print(dir_ls(file.path(root, "03_Output"), recurse = TRUE))
