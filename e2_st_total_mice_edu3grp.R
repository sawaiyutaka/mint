# ============================================================
# Multiple imputation using random forest + linear regression
# Outcome: total 24-month screen time
#          calculated from AF1-AF6
#
# Exposure: maternal education, dummy-coded
#
# mother_education_6grp:
#   Already coded as 0, 1, 2
#
# Dummy variables:
#   education_group1 = 1 if mother_education_6grp == 1, otherwise 0
#   education_group2 = 1 if mother_education_6grp == 2, otherwise 0
#
# Reference:
#   mother_education_6grp == 0
#
# Models:
#   1. Unadjusted:
#      childscreen24M ~ education_group1 + education_group2
#
#   2. Adjusted:
#      childscreen24M ~ education_group1 + education_group2 + age_corrected
#
# Imputation:
#   Missing values are imputed after creating childscreen24M.
#   Education dummy variables are created after imputation.
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
output_file <- "D:/mint/results/linear_regression_MI_random_forest_mother_education_dummy_childscreen24M.xlsx"

set.seed(12345)

m_imp <- 20
ntree_rf <- 100

# =========================
# 2. Variable settings
# =========================

af_vars <- c("AF1", "AF2", "AF3", "AF4", "AF5", "AF6")

outcome <- "childscreen24M"

education <- "mother_education_6grp"

education_group1_dummy <- "education_group1"
education_group2_dummy <- "education_group2"

age_cov <- "age_corrected"

required_columns <- c(
  af_vars,
  education,
  age_cov
)

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
# 4. Check maternal education variable
# =========================

df[[education]] <- as.numeric(df[[education]])

education_summary_before <- data.frame(
  mother_education_6grp = c(0, 1, 2, NA),
  label = c(
    "Education group 0, reference",
    "Education group 1",
    "Education group 2",
    "Missing"
  ),
  n = c(
    sum(df[[education]] == 0, na.rm = TRUE),
    sum(df[[education]] == 1, na.rm = TRUE),
    sum(df[[education]] == 2, na.rm = TRUE),
    sum(is.na(df[[education]]))
  )
)

print(education_summary_before)

# =========================
# 5. Create outcome variable before imputation
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

cat("Outcome variable created:", outcome, "\n")

# =========================
# 6. Export check file
# =========================

id_var <- "users_id"

if (!id_var %in% names(df)) {
  stop("ID variable is not found in the dataset: ", id_var)
}

check_file <- "D:/mint/results/childscreen24M_mother_education_dummy_check_by_ID.xlsx"

check_data <- df[, c(
  id_var,
  education,
  "AF1", "AF2", "AF3", "AF4", "AF5", "AF6",
  "AF1_h", "AF2_h", "AF3_h", "AF4_h", "AF5_h", "AF6_h",
  outcome
)]

wb_check <- createWorkbook()
addWorksheet(wb_check, "check_by_ID")
writeData(wb_check, "check_by_ID", check_data)
saveWorkbook(wb_check, check_file, overwrite = TRUE)

cat("Check file saved:", check_file, "\n")

# =========================
# 7. Mean and SD before imputation
# =========================

childscreen24M_mean <- mean(df[[outcome]], na.rm = TRUE)
childscreen24M_sd   <- sd(df[[outcome]], na.rm = TRUE)
childscreen24M_n    <- sum(!is.na(df[[outcome]]))

cat("Mean of", outcome, "before imputation:", round(childscreen24M_mean, 3), "\n")
cat("SD of", outcome, "before imputation:", round(childscreen24M_sd, 3), "\n")
cat("N observed for", outcome, "before imputation:", childscreen24M_n, "\n")

# =========================
# 8. Keep variables used in imputation and analysis
# =========================

analysis_columns <- c(
  outcome,
  education,
  age_cov
)

dat <- df[, analysis_columns]

# =========================
# 9. Type conversion
# =========================

dat[[outcome]] <- as.numeric(dat[[outcome]])

dat[[education]] <- factor(
  dat[[education]],
  levels = c(0, 1, 2),
  labels = c("group0", "group1", "group2")
)

dat[[age_cov]] <- as.numeric(dat[[age_cov]])

# =========================
# 10. Reverse score for age
# =========================
# age_corrected:
#   Higher value = younger maternal age

dat[[age_cov]] <- 0 - dat[[age_cov]]

cat(age_cov, "was reverse-scored: 0 -", age_cov, "\n")

# Remove rows in which all analysis variables are missing
dat <- dat[rowSums(!is.na(dat)) > 0, ]

# =========================
# 11. Missing data summary
# =========================

missing_summary <- data.frame(
  variable = names(dat),
  n_missing = sapply(dat, function(x) sum(is.na(x))),
  prop_missing = sapply(dat, function(x) mean(is.na(x)))
)

print(missing_summary)

# =========================
# 12. Multiple imputation using random forest
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
# 13. Linear regression models
# =========================

fit_linear_education_dummy <- function(imp) {
  
  completed_list <- complete(imp, action = "all")
  
  fit_unadj_list <- vector("list", length(completed_list))
  fit_adj_list   <- vector("list", length(completed_list))
  
  for (i in seq_along(completed_list)) {
    
    d <- completed_list[[i]]
    
    d[[education]] <- factor(
      d[[education]],
      levels = c("group0", "group1", "group2")
    )
    
    d[[education_group1_dummy]] <- ifelse(d[[education]] == "group1", 1, 0)
    d[[education_group2_dummy]] <- ifelse(d[[education]] == "group2", 1, 0)
    
    fit_unadj_list[[i]] <- lm(
      childscreen24M ~ education_group1 + education_group2,
      data = d
    )
    
    fit_adj_list[[i]] <- lm(
      childscreen24M ~ education_group1 + education_group2 + age_corrected,
      data = d
    )
  }
  
  list(
    unadjusted = as.mira(fit_unadj_list),
    adjusted = as.mira(fit_adj_list)
  )
}

fits <- fit_linear_education_dummy(imp)

pool_unadj <- pool(fits$unadjusted)
pool_adj   <- pool(fits$adjusted)

formula_unadj_text <- "childscreen24M ~ education_group1 + education_group2"
formula_adj_text   <- "childscreen24M ~ education_group1 + education_group2 + age_corrected"

summary(pool_unadj, conf.int = TRUE, conf.level = 0.95)
summary(pool_adj, conf.int = TRUE, conf.level = 0.95)

# =========================
# 14. Function to format pooled results
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
# 15. Rounding
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
# 16. Save to Excel
# =========================

wb <- createWorkbook()

addWorksheet(wb, "all_results")
writeData(wb, "all_results", all_results)

addWorksheet(wb, "unadjusted")
writeData(wb, "unadjusted", results_unadj)

addWorksheet(wb, "adjusted")
writeData(wb, "adjusted", results_adj)

addWorksheet(wb, "education_summary_before")
writeData(wb, "education_summary_before", education_summary_before)

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
    "exposure_definition",
    "reference_category",
    "dummy_variables",
    "age_covariate",
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
    education,
    "mother_education_6grp already coded as 0, 1, 2",
    "mother_education_6grp == 0",
    "education_group1, education_group2",
    age_cov,
    age_cov,
    paste(vars_with_missing, collapse = ", ")
  )
)

addWorksheet(wb, "imputation_settings")
writeData(wb, "imputation_settings", imputation_settings)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Completed:", output_file, "\n")