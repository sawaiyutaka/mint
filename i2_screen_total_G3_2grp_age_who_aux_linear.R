# ============================================================
# Multiple imputation using random forest
# + linear regression
#
# Outcome:
#   childscreen24M
#   Total 24-month screen time, calculated from AF1-AF6
#   Used as a continuous variable, hours/day
#
# Exposure:
#   G3
#
# Important:
#   G3 is reverse-scored as:
#      G3_rev = 0 - G3
#   Missing G3_rev is imputed by random forest.
#
# Group exposure:
#   G3_rev_group:
#      low  = G3_rev <= observed median
#      high = G3_rev >  observed median
#
# Models:
#   1. Unadjusted:
#      childscreen24M ~ G3_rev_group
#
#   2. Adjusted:
#      childscreen24M ~ G3_rev_group
#                       + age_corrected
#                       + WHO5_all_100
#
# Variables used for imputation only:
#   mother_education_6grp
#   H4_P1
#   EPDS_18m (Excluded from regression adjustment per request)
#
# Variables used both for imputation and regression adjustment:
#   WHO5_all_100
# ============================================================

# install.packages(c("readxl", "mice", "dplyr", "openxlsx", "randomForest"))

library(readxl)
library(mice)
library(dplyr)
library(openxlsx)
library(randomForest)

# =========================
# 1. File settings
# =========================

input_file  <- "D:/mint/data_xlsx/merged_selected_age_corrected.xlsx"
output_file <- "D:/mint/results/linear_regression_MI_random_forest_G3_rev_median_group_childscreen24M_adjusted_age_WHO5.xlsx"

set.seed(12345)

m_imp <- 20
ntree_rf <- 100

# =========================
# 2. Variable settings
# =========================

id_var <- "users_id"

af_vars <- c("AF1", "AF2", "AF3", "AF4", "AF5", "AF6")

outcome <- "childscreen24M"

# Exposure variables
exposure_cont  <- "G3"
exposure_rev   <- "G3_rev"
exposure_group <- "G3_rev_group"

# Covariates used in the adjusted regression model
age_cov  <- "age_corrected"
who5_cov <- "WHO5_all_100"

adjustment_vars <- c(
  age_cov,
  who5_cov
)

# Variables used for imputation only
epds_cov   <- "EPDS_18m" # Moved to imputation-only
education  <- "mother_education_6grp"
income_aux <- "H4_P1"

imputation_only_vars <- c(
  epds_cov,
  education,
  income_aux
)

required_columns <- c(
  id_var,
  af_vars,
  exposure_cont,
  age_cov,
  epds_cov,
  paste0("D", 1:5), # Required for constructing WHO5_all_100
  education,
  income_aux
)

# =========================
# 3. Read data and Construct WHO-5
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

# Construct WHO5_all_100 before imputation
# If any of D1-D5 is NA, the sum (and score) will be NA.
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
# 4. Create continuous outcome variable before imputation
# =========================

to_hours <- function(x) {
  ifelse(is.na(x), NA,
         ifelse(x == 1, 0,
                ifelse(x == 2, 0.5,
                       ifelse(x == 3, 1.5,
                              ifelse(x == 4, 2.5,
                                     ifelse(x == 5, 4,
                                            ifelse(x == 6, 6,
                                                   ifelse(x == 7, 7, NA))))))))
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
# 5. Export childscreen24M with ID for checking
# =========================

outcome_check_file <- "D:/mint/results/childscreen24M_by_ID.xlsx"

outcome_check <- df[, c(
  id_var,
  "AF1", "AF2", "AF3", "AF4", "AF5", "AF6",
  "AF1_h", "AF2_h", "AF3_h", "AF4_h", "AF5_h", "AF6_h",
  outcome
)]

wb_check <- createWorkbook()
addWorksheet(wb_check, "childscreen24M_by_ID")
writeData(wb_check, "childscreen24M_by_ID", outcome_check)
saveWorkbook(wb_check, outcome_check_file, overwrite = TRUE)

cat("Outcome check file saved:", outcome_check_file, "\n")

# =========================
# 6. Descriptive statistics before imputation
# =========================

childscreen24M_mean <- mean(df[[outcome]], na.rm = TRUE)
childscreen24M_sd   <- sd(df[[outcome]], na.rm = TRUE)
childscreen24M_n    <- sum(!is.na(df[[outcome]]))

cat("Mean of", outcome, "before imputation:", round(childscreen24M_mean, 3), "\n")
cat("SD of", outcome, "before imputation:", round(childscreen24M_sd, 3), "\n")
cat("N observed for", outcome, "before imputation:", childscreen24M_n, "\n")

outcome_summary_before <- data.frame(
  outcome = outcome,
  n_observed = childscreen24M_n,
  mean = childscreen24M_mean,
  sd = childscreen24M_sd,
  median = median(df[[outcome]], na.rm = TRUE),
  min = min(df[[outcome]], na.rm = TRUE),
  max = max(df[[outcome]], na.rm = TRUE)
)

outcome_summary_before[, c("mean", "sd", "median", "min", "max")] <-
  round(outcome_summary_before[, c("mean", "sd", "median", "min", "max")], 3)

print(outcome_summary_before)

# =========================
# 7. Reverse G3 before imputation
# =========================

df[[exposure_cont]] <- as.numeric(df[[exposure_cont]])

df[[exposure_rev]] <- 0 - df[[exposure_cont]]

cat(exposure_cont, "was reverse-scored as", exposure_rev, ": 0 -", exposure_cont, "\n")

g3_rev_median <- median(df[[exposure_rev]], na.rm = TRUE)

cat("Median of", exposure_rev, "before imputation:", g3_rev_median, "\n")

df[[exposure_group]] <- NA_character_

idx_obs <- !is.na(df[[exposure_rev]])

df[[exposure_group]][
  idx_obs & df[[exposure_rev]] <= g3_rev_median
] <- "low"

df[[exposure_group]][
  idx_obs & df[[exposure_rev]] > g3_rev_median
] <- "high"

df[[exposure_group]] <- factor(
  df[[exposure_group]],
  levels = c("low", "high")
)

exposure_cont_summary_before <- data.frame(
  exposure = exposure_cont,
  n_observed = sum(!is.na(df[[exposure_cont]])),
  mean = mean(df[[exposure_cont]], na.rm = TRUE),
  sd = sd(df[[exposure_cont]], na.rm = TRUE),
  median = median(df[[exposure_cont]], na.rm = TRUE),
  min = min(df[[exposure_cont]], na.rm = TRUE),
  max = max(df[[exposure_cont]], na.rm = TRUE)
)

exposure_rev_summary_before <- data.frame(
  exposure = exposure_rev,
  n_observed = sum(!is.na(df[[exposure_rev]])),
  mean = mean(df[[exposure_rev]], na.rm = TRUE),
  sd = sd(df[[exposure_rev]], na.rm = TRUE),
  median = median(df[[exposure_rev]], na.rm = TRUE),
  min = min(df[[exposure_rev]], na.rm = TRUE),
  max = max(df[[exposure_rev]], na.rm = TRUE)
)

exposure_group_summary_before <- df %>%
  filter(!is.na(.data[[exposure_group]])) %>%
  group_by(.data[[exposure_group]]) %>%
  summarise(
    n = n(),
    min_G3 = min(.data[[exposure_cont]], na.rm = TRUE),
    max_G3 = max(.data[[exposure_cont]], na.rm = TRUE),
    min_G3_rev = min(.data[[exposure_rev]], na.rm = TRUE),
    max_G3_rev = max(.data[[exposure_rev]], na.rm = TRUE),
    .groups = "drop"
  )

exposure_summary_before <- bind_rows(
  exposure_cont_summary_before,
  exposure_rev_summary_before
)

exposure_summary_before[, c("mean", "sd", "median", "min", "max")] <-
  round(exposure_summary_before[, c("mean", "sd", "median", "min", "max")], 3)

print(exposure_summary_before)
print(exposure_group_summary_before)

# =========================
# 8. Histogram with overflow category
#    30-min bins: overflow at 6 hours or more
# =========================

# 出力ファイル
hist_file_30min <- "D:/mint/results/hist_childscreen24M_overflow6_30min.png"

# =========================
# 8-1. Create histogram variable
# =========================

# 6時間以上を、最後の独立したビン [6, 6.5) に入れる
# 6.25はこのビンの中央付近
df$childscreen24M_hist6 <- ifelse(
  is.na(df[[outcome]]),
  NA,
  ifelse(df[[outcome]] >= 6, 6.25, df[[outcome]])
)

# =========================
# 8-2. Plot histogram
# =========================

bin_width <- 0.5
overflow_x <- 6.5

breaks_seq <- seq(0, overflow_x, by = bin_width)

png(
  filename = hist_file_30min,
  width = 1800,
  height = 1200,
  res = 200
)

par(
  mar = c(5.5, 5.2, 4.2, 2),
  las = 1,
  family = "sans"
)

# ヒストグラム情報を作成
h <- hist(
  df$childscreen24M_hist6,
  breaks = breaks_seq,
  right = FALSE,
  plot = FALSE
)

# ビンの左端を取得
bin_left <- h$breaks[-length(h$breaks)]

# 1時間未満をネイビー、1時間以上をオレンジ
bar_colors <- ifelse(
  bin_left >= 1,
  "orange",
  "navy"
)

plot(
  h,
  main = "Distribution of Average Daily Screen Time at 24 Months\nBin width: 30 minutes; values of 6 hours or more are shown as 6+",
  xlab = "Average daily screen time at 24 months, hours/day",
  ylab = "Number of children",
  xaxt = "n",
  col = bar_colors,
  border = "white"
)

# 1時間の基準線
abline(
  v = 1,
  lty = 2,
  lwd = 1.5,
  col = "gray25"
)

# x軸：0〜5までは通常表示、6の位置を6+と表示
# 6.25にはラベルや目盛り線を入れない
axis(
  side = 1,
  at = 0:6,
  labels = c(as.character(0:5), "6+"),
  tck = -0.015
)

# 補助目盛り：30分刻み
axis(
  side = 1,
  at = breaks_seq,
  labels = FALSE,
  tck = -0.007
)

legend(
  "topright",
  legend = c("<1 hour/day", "\u22651 hour/day"),
  fill = c("navy", "orange"),
  border = "white",
  bty = "n"
)

box(bty = "l")

dev.off()

cat("Histogram saved:", hist_file_30min, "\n")

# =========================
# 9. Keep variables used in imputation and analysis
# =========================

analysis_columns <- c(
  outcome,
  exposure_rev,
  age_cov,
  who5_cov,
  epds_cov,      # Kept in dataset for imputation
  education,
  income_aux
)

dat <- df[, analysis_columns]

dat <- dat[rowSums(!is.na(dat)) > 0, ]

# =========================
# 10. Type conversion
# =========================

dat[[outcome]]      <- as.numeric(dat[[outcome]])
dat[[exposure_rev]] <- as.numeric(dat[[exposure_rev]])
dat[[age_cov]]      <- as.numeric(dat[[age_cov]])
dat[[who5_cov]]      <- as.numeric(dat[[who5_cov]])
dat[[epds_cov]]      <- as.numeric(dat[[epds_cov]])

dat[[education]] <- as.factor(dat[[education]])
dat[[income_aux]] <- as.factor(dat[[income_aux]])

# =========================
# 11. Reverse age
# =========================

dat[[age_cov]] <- 0 - dat[[age_cov]]

cat(age_cov, "was reverse-scored: 0 -", age_cov, "\n")

# =========================
# 12. Reference category for education
# =========================

edu_tab <- table(dat[[education]], useNA = "no")

if (length(edu_tab) > 0) {
  edu_ref <- names(sort(edu_tab, decreasing = TRUE))[1]
  dat[[education]] <- relevel(dat[[education]], ref = edu_ref)
} else {
  edu_ref <- NA
}

cat("Reference category for", education, ":", edu_ref, "\n")

# =========================
# 13. Missing data summary
# =========================

missing_summary <- data.frame(
  variable = names(dat),
  n_missing = sapply(dat, function(x) sum(is.na(x))),
  prop_missing = sapply(dat, function(x) mean(is.na(x)))
)

print(missing_summary)

# =========================
# 14. Multiple imputation using random forest
# =========================

ini <- mice(dat, maxit = 0, printFlag = FALSE)

meth <- ini$method
pred <- ini$predictorMatrix

meth[] <- ""

vars_with_missing <- names(dat)[colSums(is.na(dat)) > 0]

meth[vars_with_missing] <- "rf"

diag(pred) <- 0

cat("Variables imputed by random forest:\n")
print(names(meth)[meth == "rf"])

cat("Variables not imputed:\n")
print(names(meth)[meth == ""])

cat("Variables included in the adjusted regression model:\n")
print(c(age_cov, who5_cov))

cat("Variables used for imputation only:\n")
print(imputation_only_vars)

imp <- mice(
  dat,
  m = m_imp,
  maxit = 20,
  method = meth,
  predictorMatrix = pred,
  ntree = ntree_rf,
  seed = 12345,
  printFlag = TRUE
)

# =========================
# 15. Linear regression models
# =========================

fit_linear_models <- function(imp) {
  
  completed_list <- complete(imp, action = "all")
  
  fit_unadj_list <- vector("list", length(completed_list))
  fit_adj_list   <- vector("list", length(completed_list))
  group_count_list <- vector("list", length(completed_list))
  
  for (i in seq_along(completed_list)) {
    
    d <- completed_list[[i]]
    
    d[[exposure_group]] <- ifelse(
      d[[exposure_rev]] <= g3_rev_median,
      "low",
      "high"
    )
    
    d[[exposure_group]] <- factor(
      d[[exposure_group]],
      levels = c("low", "high")
    )
    
    d[[exposure_group]] <- relevel(d[[exposure_group]], ref = "low")
    
    group_count_list[[i]] <- data.frame(
      imputation = i,
      exposure_group = names(table(d[[exposure_group]])),
      n = as.numeric(table(d[[exposure_group]])),
      stringsAsFactors = FALSE
    )
    
    # Unadjusted model
    fit_unadj_list[[i]] <- lm(
      as.formula(paste0(
        outcome, " ~ ",
        exposure_group
      )),
      data = d
    )
    
    # Adjusted model (Covariates: age_corrected, WHO5_all_100)
    # EPDS_18m is excluded from regression adjustment here
    fit_adj_list[[i]] <- lm(
      as.formula(paste0(
        outcome, " ~ ",
        exposure_group, " + ",
        age_cov, " + ",
        who5_cov
      )),
      data = d
    )
  }
  
  list(
    unadjusted = as.mira(fit_unadj_list),
    adjusted = as.mira(fit_adj_list),
    group_counts = bind_rows(group_count_list)
  )
}

# =========================
# 16. Function to format pooled linear regression results
# =========================

format_linear_results <- function(
    pooled_object,
    model_label,
    outcome_name,
    outcome_label,
    formula_text,
    n_analysis,
    m_imp
) {
  
  s <- summary(pooled_object, conf.int = TRUE, conf.level = 0.95)
  s <- as.data.frame(s)
  
  s <- s[s$term != "(Intercept)", ]
  
  ci_lower_col <- grep("2.5|conf.low|lo 95", names(s), value = TRUE)[1]
  ci_upper_col <- grep("97.5|conf.high|hi 95", names(s), value = TRUE)[1]
  
  out <- data.frame(
    outcome = outcome_name,
    outcome_label = outcome_label,
    model = model_label,
    term = s$term,
    beta = s$estimate,
    std_error = s$std.error,
    statistic = if ("statistic" %in% names(s)) s$statistic else NA,
    df = if ("df" %in% names(s)) s$df else NA,
    ci_lower = s[[ci_lower_col]],
    ci_upper = s[[ci_upper_col]],
    p_value = s$p.value,
    n = n_analysis,
    m = m_imp,
    formula = formula_text,
    stringsAsFactors = FALSE
  )
  
  return(out)
}

# =========================
# 17. Run linear regression
# =========================

outcome_label <- "Average daily screen time at 24 months, hours/day"

cat("Running linear regression for:", outcome_label, "\n")

fits <- fit_linear_models(imp)

pool_unadj <- pool(fits$unadjusted)
pool_adj   <- pool(fits$adjusted)

formula_unadj_text <- paste0(
  outcome,
  " ~ ",
  exposure_group
)

formula_adj_text <- paste0(
  outcome,
  " ~ ",
  exposure_group,
  " + ",
  age_cov,
  " + ",
  who5_cov
)

results_unadj <- format_linear_results(
  pooled_object = pool_unadj,
  model_label = "unadjusted",
  outcome_name = outcome,
  outcome_label = outcome_label,
  formula_text = formula_unadj_text,
  n_analysis = nrow(dat),
  m_imp = m_imp
)

results_adj <- format_linear_results(
  pooled_object = pool_adj,
  model_label = "adjusted_age_WHO5",
  outcome_name = outcome,
  outcome_label = outcome_label,
  formula_text = formula_adj_text,
  n_analysis = nrow(dat),
  m_imp = m_imp
)

all_results <- bind_rows(
  results_unadj,
  results_adj
)

# =========================
# 18. Rounding
# =========================

round_cols <- c(
  "beta",
  "std_error",
  "statistic",
  "df",
  "ci_lower",
  "ci_upper",
  "p_value"
)

all_results[round_cols] <- lapply(
  all_results[round_cols],
  function(x) round(x, 4)
)

missing_summary$prop_missing <- round(missing_summary$prop_missing, 4)

group_counts_by_imputation <- fits$group_counts

group_counts_summary <- group_counts_by_imputation %>%
  group_by(exposure_group) %>%
  summarise(
    mean_n = mean(n),
    min_n = min(n),
    max_n = max(n),
    .groups = "drop"
  )

# =========================
# 19. Save to Excel
# =========================

wb <- createWorkbook()

addWorksheet(wb, "all_results")
writeData(wb, "all_results", all_results)

addWorksheet(wb, "linear_regression")
writeData(wb, "linear_regression", all_results)

addWorksheet(wb, "outcome_summary_before")
writeData(wb, "outcome_summary_before", outcome_summary_before)

addWorksheet(wb, "exposure_summary_before")
writeData(wb, "exposure_summary_before", exposure_summary_before)

addWorksheet(wb, "exposure_group_summary_before")
writeData(wb, "exposure_group_summary_before", exposure_group_summary_before)

addWorksheet(wb, "group_counts_by_imp")
writeData(wb, "group_counts_by_imp", group_counts_by_imputation)

addWorksheet(wb, "group_counts_summary")
writeData(wb, "group_counts_summary", group_counts_summary)

addWorksheet(wb, "missing_summary")
writeData(wb, "missing_summary", missing_summary)

imputation_settings <- data.frame(
  item = c(
    "imputation_method",
    "number_of_imputations",
    "max_iterations",
    "number_of_trees",
    "analysis_model",
    "outcome",
    "outcome_definition",
    "outcome_scale",
    "original_exposure",
    "reverse_scored_exposure",
    "group_exposure_used_in_regression",
    "group_cutoff",
    "group_definition",
    "exposure_imputation",
    "reference_category",
    "effect_measure",
    "age_covariate",
    "age_covariate_use",
    "additional_adjustment_variables",
    "EPDS_18m_use",
    "WHO5_all_100_use",
    "education_variable_for_imputation",
    "education_reference_category",
    "income_variable_for_imputation",
    "variables_used_for_imputation_only",
    "variables_used_for_imputation_and_regression",
    "reverse_scored_variables",
    "variables_imputed_by_random_forest",
    "variables_not_imputed",
    "adjusted_model_formula"
  ),
  value = c(
    "random forest via mice method = 'rf'",
    m_imp,
    20,
    ntree_rf,
    "Linear regression using lm; adjusted model includes age_corrected and WHO5_all_100",
    outcome,
    "((AF1_h + AF3_h + AF5_h) * 5 + (AF2_h + AF4_h + AF6_h) * 2) / 7",
    "Continuous, hours/day",
    exposure_cont,
    exposure_rev,
    exposure_group,
    paste0("Observed median of ", exposure_rev, " before imputation = ", round(g3_rev_median, 4)),
    paste0(
      "low: ", exposure_rev, " <= observed median; ",
      "high: ", exposure_rev, " > observed median"
    ),
    "G3_rev is created before imputation and imputed by random forest if missing; group is created after imputation",
    "low",
    "Beta coefficient: mean difference in childscreen24M hours/day comparing high vs low reverse-scored G3 group",
    age_cov,
    "Used for imputation and adjusted for in the adjusted model",
    paste(adjustment_vars, collapse = ", "),
    "Used for imputation ONLY (Excluded from the regression model per request)",
    "Used for imputation and adjusted for in the adjusted model",
    education,
    edu_ref,
    income_aux,
    paste(imputation_only_vars, collapse = ", "),
    paste(adjustment_vars, collapse = ", "),
    paste(exposure_rev, age_cov, sep = ", "),
    paste(names(meth)[meth == "rf"], collapse = ", "),
    paste(names(meth)[meth == ""], collapse = ", "),
    formula_adj_text
  ),
  stringsAsFactors = FALSE
)

addWorksheet(wb, "imputation_settings")
writeData(wb, "imputation_settings", imputation_settings)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Completed:", output_file, "\n")