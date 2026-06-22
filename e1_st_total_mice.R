# ============================================================
# Multiple imputation using random forest + linear regression
# Outcome: total 24-month screen time
#          calculated from AF1-AF6
# Exposure: perceived fulfilment of financial needs at 18 months
#
# Models:
#   1. Unadjusted: childscreen24M ~ G3_18m
#   2. Adjusted:   childscreen24M ~ G3_18m + age_corrected + mother_education_6grp
#
# Imputation:
#   Missing values are imputed after creating childscreen24M.
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
output_file <- "D:/mint/results/linear_regression_MI_random_forest_G3_18m_childscreen24M.xlsx"

set.seed(12345)

m_imp <- 20  # test:2
ntree_rf <- 100

# =========================
# 2. Variable settings
# =========================

af_vars   <- c("AF1", "AF2", "AF3", "AF4", "AF5", "AF6")

outcome   <- "childscreen24M"
exposure  <- "G3_18m"
age_cov   <- "age_corrected"
education <- "mother_education_6grp"

required_columns <- c(af_vars, exposure, age_cov, education)

# =========================
# 3. Read data
# =========================

df <- read_excel(input_file) %>%
  as.data.frame()

names(df) <- trimws(names(df))

cat("Rows read:", nrow(df), "\n")
cat("Columns read:", ncol(df), "\n")

missing_columns <- setdiff(required_columns, names(df))

if (length(missing_columns) > 0) {
  stop(
    "The following columns are missing from the dataset: ",
    paste(missing_columns, collapse = ", ")
  )
}

# =========================
# 4. Create outcome variable before imputation
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

# Convert AF1-AF6 to numeric
df[af_vars] <- lapply(df[af_vars], function(x) as.numeric(x))

df$AF1_h <- to_hours(df$AF1)  # TV, weekday
df$AF2_h <- to_hours(df$AF2)  # TV, weekend
df$AF3_h <- to_hours(df$AF3)  # smartphone/tablet video, weekday
df$AF4_h <- to_hours(df$AF4)  # smartphone/tablet video, weekend
df$AF5_h <- to_hours(df$AF5)  # games, weekday
df$AF6_h <- to_hours(df$AF6)  # games, weekend

df[[outcome]] <- ifelse(
  is.na(df$AF1) | is.na(df$AF2) | is.na(df$AF3) |
    is.na(df$AF4) | is.na(df$AF5) | is.na(df$AF6),
  NA,
  (
    (df$AF1_h + df$AF3_h + df$AF5_h) * 5 +
      (df$AF2_h + df$AF4_h + df$AF6_h) * 2
  ) / 7
)

cat("Outcome variable created:", outcome, "\n")

# =========================
# Export childscreen24M with ID for checking
# =========================

id_var <- "users_id"

if (!id_var %in% names(df)) {
  stop("ID variable is not found in the dataset: ", id_var)
}

outcome_check_file <- "D:/mint/results/childscreen24M_by_ID.xlsx"

outcome_check <- df[, c(id_var, "AF1", "AF2", "AF3", "AF4", "AF5", "AF6",
                        "AF1_h", "AF2_h", "AF3_h", "AF4_h", "AF5_h", "AF6_h",
                        outcome)]

wb_check <- createWorkbook()

addWorksheet(wb_check, "childscreen24M_by_ID")
writeData(wb_check, "childscreen24M_by_ID", outcome_check)

saveWorkbook(wb_check, outcome_check_file, overwrite = TRUE)

cat("Outcome check file saved:", outcome_check_file, "\n")



# Mean and SD of childscreen24M before imputation
childscreen24M_mean <- mean(df[[outcome]], na.rm = TRUE)
childscreen24M_sd   <- sd(df[[outcome]], na.rm = TRUE)
childscreen24M_n    <- sum(!is.na(df[[outcome]]))

cat("Mean of", outcome, "before imputation:", round(childscreen24M_mean, 3), "\n")
cat("SD of", outcome, "before imputation:", round(childscreen24M_sd, 3), "\n")
cat("N observed for", outcome, "before imputation:", childscreen24M_n, "\n")

# =========================
# Histogram with overflow category: 8 hours or more
# =========================

hist_file <- "D:/mint/results/hist_childscreen24M_overflow8.png"

# For plotting only: values >= 8 are set to 8
df$childscreen24M_hist8 <- ifelse(
  is.na(df[[outcome]]),
  NA,
  pmin(df[[outcome]], 8)
)

png(
  filename = hist_file,
  width = 1600,
  height = 1200,
  res = 200
)

hist(
  df$childscreen24M_hist8,
  breaks = seq(0, 8, by = 1),
  right = FALSE,
  main = "Distribution of childscreen24M",
  xlab = "Average daily screen time at 24 months, hours/day",
  ylab = "Frequency",
  xaxt = "n"
)

axis(
  side = 1,
  at = 0:8,
  labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8+")
)

dev.off()

cat("Histogram with overflow category saved:", hist_file, "\n")

# =========================
# 5. Keep variables used in imputation and analysis
# =========================

analysis_columns <- c(outcome, exposure, age_cov, education)

dat <- df[, analysis_columns]

# =========================
# 6. Type conversion
# =========================

dat[[outcome]]  <- as.numeric(dat[[outcome]])
dat[[exposure]] <- as.numeric(dat[[exposure]])
dat[[age_cov]]  <- as.numeric(dat[[age_cov]])

dat[[education]] <- as.factor(dat[[education]])

# =========================
# 7. Reverse score
# =========================
# G3_18m:
#   Higher value = less perceived fulfilment of financial needs
#
# age_corrected:
#   Higher value = younger maternal age

dat[[exposure]] <- 0 - dat[[exposure]]
dat[[age_cov]]  <- 0 - dat[[age_cov]]

cat(exposure, "was reverse-scored: 0 -", exposure, "\n")
cat(age_cov, "was reverse-scored: 0 -", age_cov, "\n")

# Remove rows in which all analysis variables are missing
dat <- dat[rowSums(!is.na(dat)) > 0, ]

# =========================
# 8. Reference category for education
# =========================
# Use the most frequent observed category as the reference category.

edu_tab <- table(dat[[education]], useNA = "no")

if (length(edu_tab) > 0) {
  edu_ref <- names(sort(edu_tab, decreasing = TRUE))[1]
  dat[[education]] <- relevel(dat[[education]], ref = edu_ref)
} else {
  edu_ref <- NA
}

cat("Reference category for", education, ":", edu_ref, "\n")

# =========================
# 9. Missing data summary
# =========================

missing_summary <- data.frame(
  variable = names(dat),
  n_missing = sapply(dat, function(x) sum(is.na(x))),
  prop_missing = sapply(dat, function(x) mean(is.na(x)))
)

print(missing_summary)

# =========================
# 10. Multiple imputation using random forest
# =========================

ini <- mice(dat, maxit = 0, printFlag = FALSE)

meth <- ini$method
pred <- ini$predictorMatrix

meth[] <- ""

vars_with_missing <- names(dat)[colSums(is.na(dat)) > 0]

meth[vars_with_missing] <- "rf"

cat("Variables imputed by random forest:\n")
print(vars_with_missing)

diag(pred) <- 0

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
# 11. Regression models
# =========================

formula_unadj_text <- "childscreen24M ~ G3_18m"
formula_adj_text   <- "childscreen24M ~ G3_18m + age_corrected + mother_education_6grp"

cat("Unadjusted model:\n")
cat(formula_unadj_text, "\n")

cat("Adjusted model:\n")
cat(formula_adj_text, "\n")

fit_unadj <- with(
  imp,
  lm(childscreen24M ~ G3_18m)
)

fit_adj <- with(
  imp,
  lm(childscreen24M ~ G3_18m + age_corrected + mother_education_6grp)
)

# =========================
# 12. Pool estimates using Rubin's rule
# =========================

pool_unadj <- pool(fit_unadj)
pool_adj   <- pool(fit_adj)

summary(pool_unadj, conf.int = TRUE, conf.level = 0.95)
summary(pool_adj, conf.int = TRUE, conf.level = 0.95)

# =========================
# 13. Function to format pooled results
# =========================

format_pooled_results <- function(pool_object, model_label, formula_text, n_analysis, m_imp) {
  
  s <- summary(pool_object, conf.int = TRUE, conf.level = 0.95)
  s <- as.data.frame(s)
  
  s <- s[s$term != "(Intercept)", ]
  
  ci_lower_col <- grep("2.5|conf.low|lo 95", names(s), value = TRUE)[1]
  ci_upper_col <- grep("97.5|conf.high|hi 95", names(s), value = TRUE)[1]
  
  out <- data.frame(
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

results_unadj <- format_pooled_results(
  pool_object = pool_unadj,
  model_label = "unadjusted",
  formula_text = formula_unadj_text,
  n_analysis = nrow(dat),
  m_imp = m_imp
)

results_adj <- format_pooled_results(
  pool_object = pool_adj,
  model_label = "adjusted",
  formula_text = formula_adj_text,
  n_analysis = nrow(dat),
  m_imp = m_imp
)

all_results <- bind_rows(results_unadj, results_adj)

# =========================
# 14. Rounding
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

all_results[round_cols] <- lapply(all_results[round_cols], function(x) round(x, 4))
results_unadj[round_cols] <- lapply(results_unadj[round_cols], function(x) round(x, 4))
results_adj[round_cols] <- lapply(results_adj[round_cols], function(x) round(x, 4))

missing_summary$prop_missing <- round(missing_summary$prop_missing, 4)

# =========================
# 15. Save to Excel
# =========================

wb <- createWorkbook()

addWorksheet(wb, "all_results")
writeData(wb, "all_results", all_results)

addWorksheet(wb, "unadjusted")
writeData(wb, "unadjusted", results_unadj)

addWorksheet(wb, "adjusted")
writeData(wb, "adjusted", results_adj)

addWorksheet(wb, "missing_summary")
writeData(wb, "missing_summary", missing_summary)

imputation_settings <- data.frame(
  item = c(
    "imputation_method",
    "number_of_imputations",
    "max_iterations",
    "number_of_trees",
    "outcome",
    "outcome_definition",
    "exposure",
    "age_covariate",
    "education_covariate",
    "education_reference_category",
    "reverse_scored_variables",
    "variables_imputed_by_random_forest"
  ),
  value = c(
    "random forest via mice method = 'rf'",
    m_imp,
    20,
    ntree_rf,
    outcome,
    "((AF1_h + AF3_h + AF5_h) * 5 + (AF2_h + AF4_h + AF6_h) * 2) / 7",
    exposure,
    age_cov,
    education,
    edu_ref,
    paste(exposure, age_cov, sep = ", "),
    paste(vars_with_missing, collapse = ", ")
  )
)

addWorksheet(wb, "imputation_settings")
writeData(wb, "imputation_settings", imputation_settings)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Completed:", output_file, "\n")