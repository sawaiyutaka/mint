# ============================================================
# Table 1 exported to Excel
#
# Input:
#   D:/mint/data_xlsx/merged_selected_age_corrected.xlsx
#
# Exposure:
#   G3
#   G3_rev = 0 - G3
#
# Group:
#   Lower  = G3_rev <= observed median
#   Higher = G3_rev >  observed median
#
# Table 1 columns:
#   Overall, Lower, Higher
#
# Important:
#   Overall includes participants with missing G3 / G3_rev_group.
#   Lower and Higher include only participants with non-missing G3.
#
# Variables in Table 1:
#   age_corrected
#   WHO5_all_100
#   childscreen24M
#   G3
#   G3_rev
# ============================================================

# install.packages(c("readxl", "dplyr", "openxlsx"))

library(readxl)
library(dplyr)
library(openxlsx)

# =========================
# 1. File settings
# =========================

input_file <- "D:/mint/data_xlsx/merged_selected_age_corrected.xlsx"

output_dir <- "D:/mint/results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

table1_excel_file <- file.path(
  output_dir,
  "table1_G3_rev_group_overall_lower_higher.xlsx"
)

# =========================
# 2. Variable settings
# =========================

id_var <- "users_id"

af_vars <- c("AF1", "AF2", "AF3", "AF4", "AF5", "AF6")

outcome <- "childscreen24M"

exposure_cont  <- "G3"
exposure_rev   <- "G3_rev"
exposure_group <- "G3_rev_group"

age_cov  <- "age_corrected"
who5_cov <- "WHO5_all_100"

required_columns <- c(
  id_var,
  af_vars,
  exposure_cont,
  age_cov,
  paste0("D", 1:5)
)

# =========================
# 3. Read data
# =========================

df_raw <- read_excel(input_file)
names(df_raw) <- trimws(names(df_raw))

cat("Rows read:", nrow(df_raw), "\n")
cat("Columns read:", ncol(df_raw), "\n")

missing_columns <- setdiff(required_columns, names(df_raw))

if (length(missing_columns) > 0) {
  stop(
    "The following columns are missing from the dataset: ",
    paste(missing_columns, collapse = ", ")
  )
}

# =========================
# 4. Construct WHO5_all_100
# =========================
# 添付コードと同じ作成方法：
# WHO5_raw = sum(6 - D1:D5)
# WHO5_all_100 = WHO5_raw * 4
#
# D1-D5のいずれかがNAの場合、WHO5_all_100もNAになります。

df <- df_raw %>%
  rowwise() %>%
  mutate(
    WHO5_raw = sum(6 - c(D1, D2, D3, D4, D5)),
    WHO5_all_100 = WHO5_raw * 4
  ) %>%
  ungroup() %>%
  as.data.frame()

cat("WHO5_all_100 constructed successfully.\n")

# =========================
# 5. Create continuous outcome: childscreen24M
# =========================

to_hours <- function(x) {
  ifelse(
    is.na(x), NA,
    ifelse(
      x == 1, 0,
      ifelse(
        x == 2, 0.5,
        ifelse(
          x == 3, 1.5,
          ifelse(
            x == 4, 2.5,
            ifelse(
              x == 5, 4,
              ifelse(
                x == 6, 6,
                ifelse(x == 7, 7, NA)
              )
            )
          )
        )
      )
    )
  )
}

df[af_vars] <- lapply(df[af_vars], function(x) as.numeric(x))

df$AF1_h <- to_hours(df$AF1)
df$AF2_h <- to_hours(df$AF2)
df$AF3_h <- to_hours(df$AF3)
df$AF4_h <- to_hours(df$AF4)
df$AF5_h <- to_hours(df$AF5)
df$AF6_h <- to_hours(df$AF6)

df[[outcome]] <- ifelse(
  is.na(df$AF1) | is.na(df$AF2) | is.na(df$AF3) |
    is.na(df$AF4) | is.na(df$AF5) | is.na(df$AF6),
  NA,
  (
    (df$AF1_h + df$AF3_h + df$AF5_h) * 5 +
      (df$AF2_h + df$AF4_h + df$AF6_h) * 2
  ) / 7
)

cat("Continuous outcome variable created:", outcome, "\n")

# =========================
# 6. Create G3_rev and G3_rev_group
# =========================

df[[exposure_cont]] <- as.numeric(df[[exposure_cont]])

df[[exposure_rev]] <- 0 - df[[exposure_cont]]

cat(
  exposure_cont,
  "was reverse-scored as",
  exposure_rev,
  ": 0 -",
  exposure_cont,
  "\n"
)

g3_rev_median <- median(df[[exposure_rev]], na.rm = TRUE)

cat("Median of", exposure_rev, ":", g3_rev_median, "\n")

df[[exposure_group]] <- NA_character_

idx_obs <- !is.na(df[[exposure_rev]])

df[[exposure_group]][
  idx_obs & df[[exposure_rev]] <= g3_rev_median
] <- "Lower"

df[[exposure_group]][
  idx_obs & df[[exposure_rev]] > g3_rev_median
] <- "Higher"

df[[exposure_group]] <- factor(
  df[[exposure_group]],
  levels = c("Lower", "Higher")
)

# =========================
# 7. Type conversion
# =========================

df[[age_cov]]       <- as.numeric(df[[age_cov]])
df[[who5_cov]]      <- as.numeric(df[[who5_cov]])
df[[outcome]]       <- as.numeric(df[[outcome]])
df[[exposure_cont]] <- as.numeric(df[[exposure_cont]])
df[[exposure_rev]]  <- as.numeric(df[[exposure_rev]])

# =========================
# 8. Create Table 1 summary function
# =========================

format_mean_sd <- function(x, digits = 2) {
  x_obs <- x[!is.na(x)]
  
  if (length(x_obs) == 0) {
    return(NA_character_)
  }
  
  paste0(
    round(mean(x_obs), digits),
    " (",
    round(sd(x_obs), digits),
    ")"
  )
}

format_median_minmax <- function(x, digits = 2) {
  x_obs <- x[!is.na(x)]
  
  if (length(x_obs) == 0) {
    return(NA_character_)
  }
  
  paste0(
    round(median(x_obs), digits),
    " [",
    round(min(x_obs), digits),
    ", ",
    round(max(x_obs), digits),
    "]"
  )
}

summarise_continuous <- function(data, var, var_label, digits = 2) {
  
  x <- data[[var]]
  
  data.frame(
    variable = var_label,
    statistic = c(
      "N observed",
      "Mean (SD)",
      "Median [min, max]",
      "Missing"
    ),
    value = c(
      sum(!is.na(x)),
      format_mean_sd(x, digits = digits),
      format_median_minmax(x, digits = digits),
      sum(is.na(x))
    ),
    stringsAsFactors = FALSE
  )
}

# =========================
# 9. Create datasets for each column
# =========================

df_overall <- df

df_lower <- df %>%
  filter(.data[[exposure_group]] == "Lower")

df_higher <- df %>%
  filter(.data[[exposure_group]] == "Higher")

# =========================
# 10. Create Table 1 for each column
# =========================

table1_vars <- data.frame(
  var = c(
    age_cov,
    who5_cov,
    outcome,
    exposure_cont,
    exposure_rev
  ),
  label = c(
    "Maternal age, years",
    "WHO-5 score, 0-100",
    "Average daily screen time at 24 months, hours/day",
    "G3",
    "G3 reverse-scored"
  ),
  stringsAsFactors = FALSE
)

make_table1_column <- function(data, column_name) {
  
  out <- bind_rows(
    lapply(
      seq_len(nrow(table1_vars)),
      function(i) {
        summarise_continuous(
          data = data,
          var = table1_vars$var[i],
          var_label = table1_vars$label[i],
          digits = 2
        )
      }
    )
  )
  
  names(out)[names(out) == "value"] <- column_name
  
  out
}

table1_overall <- make_table1_column(df_overall, "Overall")
table1_lower   <- make_table1_column(df_lower, "Lower")
table1_higher  <- make_table1_column(df_higher, "Higher")

table1_final <- table1_overall %>%
  left_join(table1_lower, by = c("variable", "statistic")) %>%
  left_join(table1_higher, by = c("variable", "statistic"))

# =========================
# 11. Sample size check
# =========================

sample_size_check <- data.frame(
  item = c(
    "N in original data",
    "N in Overall column",
    "N in Lower column",
    "N in Higher column",
    "N missing G3_rev_group",
    "Observed median of G3_rev",
    "Number exactly at median of G3_rev"
  ),
  value = c(
    nrow(df),
    nrow(df_overall),
    nrow(df_lower),
    nrow(df_higher),
    sum(is.na(df[[exposure_group]])),
    g3_rev_median,
    sum(df[[exposure_rev]] == g3_rev_median, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

group_counts <- as.data.frame(
  table(df[[exposure_group]], useNA = "ifany")
)

names(group_counts) <- c("G3_rev_group", "n")

g3_distribution <- as.data.frame(
  table(df[[exposure_cont]], useNA = "ifany")
)

names(g3_distribution) <- c("G3", "n")

g3_rev_distribution <- as.data.frame(
  table(df[[exposure_rev]], useNA = "ifany")
)

names(g3_rev_distribution) <- c("G3_rev", "n")

# =========================
# 12. Save to Excel
# =========================

wb <- createWorkbook()

addWorksheet(wb, "Table1")
writeData(wb, "Table1", table1_final)

addWorksheet(wb, "sample_size_check")
writeData(wb, "sample_size_check", sample_size_check)

addWorksheet(wb, "group_counts")
writeData(wb, "group_counts", group_counts)

addWorksheet(wb, "G3_distribution")
writeData(wb, "G3_distribution", g3_distribution)

addWorksheet(wb, "G3_rev_distribution")
writeData(wb, "G3_rev_distribution", g3_rev_distribution)

# 見やすいように列幅を調整
setColWidths(wb, "Table1", cols = 1:5, widths = "auto")
setColWidths(wb, "sample_size_check", cols = 1:2, widths = "auto")
setColWidths(wb, "group_counts", cols = 1:2, widths = "auto")
setColWidths(wb, "G3_distribution", cols = 1:2, widths = "auto")
setColWidths(wb, "G3_rev_distribution", cols = 1:2, widths = "auto")

saveWorkbook(
  wb,
  file = table1_excel_file,
  overwrite = TRUE
)

cat("Table 1 Excel file saved:", table1_excel_file, "\n")

# =========================
# 13. Print check
# =========================

cat("\n")
cat("Sample size check\n")
cat("-----------------\n")
print(sample_size_check)

cat("\n")
cat("Group counts\n")
cat("------------\n")
print(group_counts)

cat("\nCompleted.\n")