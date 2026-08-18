# ============================================================
# Screen time associated with:
#   1) 2gobun (binary outcome)
#   2) ASQ-3 Communication score (continuous outcome)
#
# Outcomes:
#
#   1. 2gobun
#      C4-4 = 1 -> 0
#      C4-4 = 2 -> 1
#      C4-4 = NA -> NA
#
#      Analysis:
#      Poisson regression with robust SE
#      Effect measure: Risk Ratio (RR)
#
#   2. ASQ3_communication
#      Continuous score
#
#      Analysis:
#      Linear regression
#      Effect measure: regression coefficient (beta)
#
# Exposures:
#
#   1. childscreen24M
#      Continuous, hours/day
#
#   2. childscreen24M_binary
#      <1 hour/day  -> 0 (reference)
#      >=1 hour/day -> 1
#
# All models are unadjusted.
#
# Multiple imputation:
#   mice
#   m = 20
#   random forest
#   ntree = 100
#
# Outcomes are not imputed.
# childscreen24M is imputed when missing.
# childscreen24M_binary is recreated from childscreen24M
# after multiple imputation.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

library(readxl)
library(dplyr)
library(mice)
library(openxlsx)
library(randomForest)
library(geepack)


# ============================================================
# 1. Settings
# ============================================================

input_file <-
  "D:/mint/data_xlsx/merged_selected_age_corrected_24_language.xlsx"

output_file <-
  "D:/mint/results/screen_time_2gobun_ASQ3_MI_analysis.xlsx"

m_imp <- 20
ntree_rf <- 100

set.seed(12345)


# ============================================================
# 2. Read data
# ============================================================

df <- read_excel(input_file)

names(df) <- trimws(names(df))


# ============================================================
# 3. Variable settings
# ============================================================

screen_vars <- paste0("AF", 1:6)

who5_vars <- paste0("D", 1:5)

asq_vars <- paste0("R", 1:6)


# ============================================================
# 4. Check required variables
# ============================================================

required_columns <- c(
  screen_vars,
  who5_vars,
  asq_vars,
  "C4-4",
  "age_corrected",
  "H4_P1",
  "EPDS_18m",
  "mother_education_6grp"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  
  stop(
    "Missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}


# ============================================================
# 5. Convert variables to numeric
# ============================================================

df[required_columns] <- lapply(
  df[required_columns],
  function(x) suppressWarnings(as.numeric(x))
)


# ============================================================
# 6. Create childscreen24M
# ============================================================

# AF coding:
#
# 1 -> 0 hours
# 2 -> 0.5 hours
# 3 -> 1.5 hours
# 4 -> 2.5 hours
# 5 -> 4 hours
# 6 -> 6 hours
# 7 -> 7 hours

to_hours <- function(x) {
  
  case_when(
    x == 1 ~ 0,
    x == 2 ~ 0.5,
    x == 3 ~ 1.5,
    x == 4 ~ 2.5,
    x == 5 ~ 4,
    x == 6 ~ 6,
    x == 7 ~ 7,
    TRUE ~ NA_real_
  )
}


for (v in screen_vars) {
  
  df[[paste0(v, "_h")]] <-
    to_hours(df[[v]])
}


# Weighted daily average
#
# AF1, AF3, AF5 = weekdays
# AF2, AF4, AF6 = weekends
#
# If any of AF1-AF6 is missing:
#   childscreen24M = NA

df <- df %>%
  mutate(
    
    childscreen24M = if_else(
      
      if_any(
        all_of(screen_vars),
        is.na
      ),
      
      NA_real_,
      
      (
        (AF1_h + AF3_h + AF5_h) * 5 +
          (AF2_h + AF4_h + AF6_h) * 2
      ) / 7
    )
  )


# ============================================================
# 7. Create binary screen-time variable
# ============================================================

# <1 hour/day  -> 0
# >=1 hour/day -> 1
# Missing      -> NA

df$childscreen24M_binary <- case_when(
  
  is.na(df$childscreen24M) ~ NA_real_,
  
  df$childscreen24M < 1 ~ 0,
  
  df$childscreen24M >= 1 ~ 1,
  
  TRUE ~ NA_real_
)


# ============================================================
# 8. Create WHO5_all_100
#
# Used only as an auxiliary variable for multiple imputation.
# It is not included in the regression models.
# ============================================================

df <- df %>%
  mutate(
    
    WHO5_raw = if_else(
      
      if_any(
        all_of(who5_vars),
        is.na
      ),
      
      NA_real_,
      
      (6 - D1) +
        (6 - D2) +
        (6 - D3) +
        (6 - D4) +
        (6 - D5)
    ),
    
    WHO5_all_100 = WHO5_raw * 4
  )


# ============================================================
# 9. Create 2gobun
# ============================================================

# C4-4 = 1 -> 2gobun = 0
# C4-4 = 2 -> 2gobun = 1
# C4-4 = NA or another value -> NA

df[["2gobun"]] <- case_when(
  
  df[["C4-4"]] == 1 ~ 0,
  
  df[["C4-4"]] == 2 ~ 1,
  
  TRUE ~ NA_real_
)


# ============================================================
# 10. Create ASQ-3 Communication score
#
# Original coding:
#   1 = Yes
#   2 = Sometimes
#   3 = Not Yet
#
# ASQ-3 scoring:
#   Yes       = 10
#   Sometimes = 5
#   Not Yet   = 0
#
# Missing-item rule:
#   0 missing   -> sum of 6 items
#   1-2 missing -> mean of answered items x 6
#   >=3 missing -> ASQ3_communication = NA
# ============================================================

for (v in asq_vars) {
  
  df[[paste0(v, "_score")]] <- case_when(
    
    df[[v]] == 1 ~ 10,
    
    df[[v]] == 2 ~ 5,
    
    df[[v]] == 3 ~ 0,
    
    TRUE ~ NA_real_
  )
}


asq_score_vars <- paste0(
  asq_vars,
  "_score"
)


# Number of missing ASQ-3 items

df$ASQ3_communication_n_missing <-
  rowSums(
    is.na(
      df[asq_score_vars]
    )
  )


# Adjusted ASQ-3 Communication score

df$ASQ3_communication <- ifelse(
  
  df$ASQ3_communication_n_missing <= 2,
  
  rowMeans(
    df[asq_score_vars],
    na.rm = TRUE
  ) * 6,
  
  NA_real_
)


# ============================================================
# 11. Distribution of 2gobun
# ============================================================

N_total <- nrow(df)

n_2gobun_0 <- sum(
  df[["2gobun"]] == 0,
  na.rm = TRUE
)

n_2gobun_1 <- sum(
  df[["2gobun"]] == 1,
  na.rm = TRUE
)

n_2gobun_missing <- sum(
  is.na(df[["2gobun"]])
)


gobun_distribution <- data.frame(
  
  Outcome = "2gobun",
  
  Category = c(
    "0: C4-4 = 1",
    "1: C4-4 = 2",
    "Missing"
  ),
  
  n = c(
    n_2gobun_0,
    n_2gobun_1,
    n_2gobun_missing
  ),
  
  Percent = round(
    100 *
      c(
        n_2gobun_0,
        n_2gobun_1,
        n_2gobun_missing
      ) /
      N_total,
    1
  )
)


cat("\n=====================================\n")
cat("2GOBUN DISTRIBUTION\n")
cat("=====================================\n")

print(gobun_distribution)


# ============================================================
# 12. Summary of ASQ3_communication
# ============================================================

asq_observed <- df$ASQ3_communication[
  !is.na(df$ASQ3_communication)
]


asq3_summary <- data.frame(
  
  Outcome = "ASQ3_communication",
  
  N_total = N_total,
  
  N_observed = length(asq_observed),
  
  N_missing = sum(
    is.na(df$ASQ3_communication)
  ),
  
  Missing_percent = round(
    mean(
      is.na(df$ASQ3_communication)
    ) * 100,
    1
  ),
  
  Mean = mean(asq_observed),
  
  SD = sd(asq_observed),
  
  Median = median(asq_observed),
  
  Minimum = min(asq_observed),
  
  Maximum = max(asq_observed)
) %>%
  
  mutate(
    across(
      c(
        Mean,
        SD,
        Median,
        Minimum,
        Maximum
      ),
      ~ round(.x, 2)
    )
  )


cat("\n=====================================\n")
cat("ASQ-3 COMMUNICATION SUMMARY\n")
cat("=====================================\n")

print(asq3_summary)


# ============================================================
# 13. Distribution of binary screen time
#
# This table is based on observed data before imputation.
# ============================================================

n_screen_0 <- sum(
  df$childscreen24M_binary == 0,
  na.rm = TRUE
)

n_screen_1 <- sum(
  df$childscreen24M_binary == 1,
  na.rm = TRUE
)

n_screen_missing <- sum(
  is.na(df$childscreen24M_binary)
)


screen_binary_distribution <- data.frame(
  
  Exposure = "childscreen24M_binary",
  
  Category = c(
    "0: <1 hour/day",
    "1: >=1 hour/day",
    "Missing"
  ),
  
  n = c(
    n_screen_0,
    n_screen_1,
    n_screen_missing
  ),
  
  Percent = round(
    100 *
      c(
        n_screen_0,
        n_screen_1,
        n_screen_missing
      ) /
      N_total,
    1
  )
)


cat("\n=====================================\n")
cat("BINARY SCREEN-TIME DISTRIBUTION\n")
cat("=====================================\n")

print(screen_binary_distribution)


# ============================================================
# 14. Summary of continuous screen time
#
# Based on observed data before imputation.
# ============================================================

screen_observed <- df$childscreen24M[
  !is.na(df$childscreen24M)
]


screen_continuous_summary <- data.frame(
  
  Exposure = "childscreen24M",
  
  N_total = N_total,
  
  N_observed = length(screen_observed),
  
  N_missing = sum(
    is.na(df$childscreen24M)
  ),
  
  Missing_percent = round(
    mean(
      is.na(df$childscreen24M)
    ) * 100,
    1
  ),
  
  Mean = mean(screen_observed),
  
  SD = sd(screen_observed),
  
  Median = median(screen_observed),
  
  Minimum = min(screen_observed),
  
  Maximum = max(screen_observed)
) %>%
  
  mutate(
    across(
      c(
        Mean,
        SD,
        Median,
        Minimum,
        Maximum
      ),
      ~ round(.x, 2)
    )
  )


cat("\n=====================================\n")
cat("CONTINUOUS SCREEN-TIME SUMMARY\n")
cat("=====================================\n")

print(screen_continuous_summary)


# ============================================================
# 15. Dataset for multiple imputation
#
# Outcomes:
#   2gobun and ASQ3_communication are not imputed.
#
# Exposure:
#   childscreen24M is imputed when missing.
#
# Binary screen time is not separately imputed.
# It is recreated from childscreen24M after imputation.
#
# Other variables are auxiliary variables used only for
# multiple imputation.
# ============================================================

dat_imp <- df %>%
  select(
    `2gobun`,
    ASQ3_communication,
    childscreen24M,
    H4_P1,
    age_corrected,
    WHO5_all_100,
    EPDS_18m,
    mother_education_6grp
  )


# ============================================================
# 16. Missing-value summary before MI
# ============================================================

missing_summary <- data.frame(
  
  variable = names(dat_imp),
  
  missing_n = colSums(
    is.na(dat_imp)
  ),
  
  missing_percent = round(
    colMeans(
      is.na(dat_imp)
    ) * 100,
    1
  )
)


cat("\n=====================================\n")
cat("MISSING VALUES BEFORE MI\n")
cat("=====================================\n")

print(missing_summary)


# ============================================================
# 17. MICE settings
# ============================================================

ini <- mice(
  dat_imp,
  maxit = 0,
  printFlag = FALSE
)

meth <- ini$method

pred <- ini$predictorMatrix


# Start with no imputation

meth[] <- ""


# ------------------------------------------------------------
# Outcomes are not imputed
# ------------------------------------------------------------

outcome_vars <- c(
  "2gobun",
  "ASQ3_communication"
)

meth[outcome_vars] <- ""


# Following the original code, outcomes are not used as
# predictors in the imputation models.

pred[, outcome_vars] <- 0

pred[outcome_vars, ] <- 0


# ------------------------------------------------------------
# Exposure and auxiliary variables:
# impute missing values using random forest
# ------------------------------------------------------------

variables_to_impute <- setdiff(
  names(dat_imp),
  outcome_vars
)


for (v in variables_to_impute) {
  
  if (any(is.na(dat_imp[[v]]))) {
    
    meth[v] <- "rf"
    
  } else {
    
    meth[v] <- ""
  }
}


diag(pred) <- 0


cat("\n=====================================\n")
cat("VARIABLES IMPUTED BY RANDOM FOREST\n")
cat("=====================================\n")

print(
  names(meth)[meth == "rf"]
)


# ============================================================
# 18. Multiple imputation
# ============================================================

set.seed(12345)

imp <- mice(
  
  dat_imp,
  
  m = m_imp,
  
  maxit = 20,
  
  method = meth,
  
  predictorMatrix = pred,
  
  ntree = ntree_rf,
  
  seed = 12345,
  
  printFlag = TRUE
)


# ============================================================
# 19. Completed datasets
# ============================================================

completed_list <- complete(
  imp,
  action = "all"
)


# ============================================================
# 20. Regression models
#
# Model 1:
#   Outcome  = 2gobun
#   Exposure = childscreen24M, continuous
#   Poisson regression with robust SE
#
# Model 2:
#   Outcome  = 2gobun
#   Exposure = childscreen24M_binary
#   Poisson regression with robust SE
#
# Model 3:
#   Outcome  = ASQ3_communication
#   Exposure = childscreen24M, continuous
#   Linear regression
#
# Model 4:
#   Outcome  = ASQ3_communication
#   Exposure = childscreen24M_binary
#   Linear regression
# ============================================================

poisson_continuous_list <- list()

poisson_binary_list <- list()

linear_continuous_list <- list()

linear_binary_list <- list()


for (i in seq_along(completed_list)) {
  
  d <- completed_list[[i]]
  
  
  # ----------------------------------------------------------
  # Recreate binary screen time from imputed continuous
  # screen time.
  #
  # This guarantees consistency between the continuous and
  # binary screen-time variables.
  # ----------------------------------------------------------
  
  d$childscreen24M_binary <- ifelse(
    d$childscreen24M >= 1,
    1,
    0
  )
  
  
  # ----------------------------------------------------------
  # Unique ID for GEE
  # One row = one participant
  # ----------------------------------------------------------
  
  d$gee_id <- seq_len(
    nrow(d)
  )
  
  
  # ==========================================================
  # Model 1:
  # 2gobun outcome, continuous screen time
  # ==========================================================
  
  poisson_continuous_list[[i]] <- geeglm(
    
    `2gobun` ~ childscreen24M,
    
    id = gee_id,
    
    data = d,
    
    family = poisson(
      link = "log"
    ),
    
    corstr = "independence",
    
    std.err = "san.se"
  )
  
  
  # ==========================================================
  # Model 2:
  # 2gobun outcome, binary screen time
  # ==========================================================
  
  poisson_binary_list[[i]] <- geeglm(
    
    `2gobun` ~ childscreen24M_binary,
    
    id = gee_id,
    
    data = d,
    
    family = poisson(
      link = "log"
    ),
    
    corstr = "independence",
    
    std.err = "san.se"
  )
  
  
  # ==========================================================
  # Model 3:
  # ASQ-3 outcome, continuous screen time
  # ==========================================================
  
  linear_continuous_list[[i]] <- lm(
    
    ASQ3_communication ~ childscreen24M,
    
    data = d
  )
  
  
  # ==========================================================
  # Model 4:
  # ASQ-3 outcome, binary screen time
  # ==========================================================
  
  linear_binary_list[[i]] <- lm(
    
    ASQ3_communication ~ childscreen24M_binary,
    
    data = d
  )
}


# ============================================================
# 21. Pool estimates using Rubin's rules
# ============================================================

poisson_continuous_pool <- pool(
  as.mira(
    poisson_continuous_list
  )
)


poisson_binary_pool <- pool(
  as.mira(
    poisson_binary_list
  )
)


linear_continuous_pool <- pool(
  as.mira(
    linear_continuous_list
  )
)


linear_binary_pool <- pool(
  as.mira(
    linear_binary_list
  )
)


# ============================================================
# 22. Extract Poisson regression results
# ============================================================

poisson_continuous_result <- summary(
  
  poisson_continuous_pool,
  
  conf.int = TRUE,
  
  exponentiate = TRUE
  
) %>%
  
  as.data.frame() %>%
  
  filter(
    term == "childscreen24M"
  ) %>%
  
  transmute(
    
    outcome = "2gobun (1 = event)",
    
    regression =
      "Poisson regression with robust SE",
    
    exposure =
      "childscreen24M (continuous)",
    
    comparison =
      "Per 1-hour/day increase",
    
    estimate_type = "Risk Ratio",
    
    estimate = estimate,
    
    CI_lower = `2.5 %`,
    
    CI_upper = `97.5 %`,
    
    p_value = p.value,
    
    adjusted_for = "None"
  )


poisson_binary_result <- summary(
  
  poisson_binary_pool,
  
  conf.int = TRUE,
  
  exponentiate = TRUE
  
) %>%
  
  as.data.frame() %>%
  
  filter(
    term == "childscreen24M_binary"
  ) %>%
  
  transmute(
    
    outcome = "2gobun (1 = event)",
    
    regression =
      "Poisson regression with robust SE",
    
    exposure =
      "childscreen24M_binary",
    
    comparison =
      ">=1 hour/day vs <1 hour/day",
    
    estimate_type = "Risk Ratio",
    
    estimate = estimate,
    
    CI_lower = `2.5 %`,
    
    CI_upper = `97.5 %`,
    
    p_value = p.value,
    
    adjusted_for = "None"
  )


# ============================================================
# 23. Extract linear regression results
# ============================================================

linear_continuous_result <- summary(
  
  linear_continuous_pool,
  
  conf.int = TRUE
  
) %>%
  
  as.data.frame() %>%
  
  filter(
    term == "childscreen24M"
  ) %>%
  
  transmute(
    
    outcome = "ASQ3_communication",
    
    regression = "Linear regression",
    
    exposure =
      "childscreen24M (continuous)",
    
    comparison =
      "Per 1-hour/day increase",
    
    estimate_type =
      "Regression coefficient (beta)",
    
    estimate = estimate,
    
    CI_lower = `2.5 %`,
    
    CI_upper = `97.5 %`,
    
    p_value = p.value,
    
    adjusted_for = "None"
  )


linear_binary_result <- summary(
  
  linear_binary_pool,
  
  conf.int = TRUE
  
) %>%
  
  as.data.frame() %>%
  
  filter(
    term == "childscreen24M_binary"
  ) %>%
  
  transmute(
    
    outcome = "ASQ3_communication",
    
    regression = "Linear regression",
    
    exposure =
      "childscreen24M_binary",
    
    comparison =
      ">=1 hour/day vs <1 hour/day",
    
    estimate_type =
      "Regression coefficient (beta)",
    
    estimate = estimate,
    
    CI_lower = `2.5 %`,
    
    CI_upper = `97.5 %`,
    
    p_value = p.value,
    
    adjusted_for = "None"
  )


# ============================================================
# 24. Combine all results
# ============================================================

results <- bind_rows(
  
  poisson_continuous_result,
  
  poisson_binary_result,
  
  linear_continuous_result,
  
  linear_binary_result
  
) %>%
  
  mutate(
    across(
      c(
        estimate,
        CI_lower,
        CI_upper,
        p_value
      ),
      ~ round(.x, 4)
    )
  )


if (nrow(results) != 4) {
  
  warning(
    "Four exposure estimates were expected, but ",
    nrow(results),
    " estimates were extracted. Check the model term names."
  )
}


cat("\n=====================================\n")
cat("REGRESSION RESULTS\n")
cat("=====================================\n")

print(results)


# ============================================================
# 25. Analysis sample information
#
# Outcomes are not imputed.
# Therefore, participants with missing outcome values are
# excluded from the corresponding regression analysis.
# ============================================================

analysis_sample <- data.frame(
  
  Outcome = c(
    "2gobun",
    "ASQ3_communication"
  ),
  
  N_total = c(
    nrow(dat_imp),
    nrow(dat_imp)
  ),
  
  N_observed_outcome = c(
    sum(
      !is.na(dat_imp[["2gobun"]])
    ),
    sum(
      !is.na(dat_imp$ASQ3_communication)
    )
  ),
  
  N_missing_outcome = c(
    sum(
      is.na(dat_imp[["2gobun"]])
    ),
    sum(
      is.na(dat_imp$ASQ3_communication)
    )
  )
)


# ============================================================
# 26. Model information
# ============================================================

model_information <- data.frame(
  
  Item = c(
    "Binary outcome",
    "Binary outcome event",
    "Continuous outcome",
    "Continuous exposure",
    "Continuous exposure unit",
    "Binary exposure",
    "Binary exposure reference",
    "Binary exposure comparison",
    "Model for 2gobun",
    "Effect measure for 2gobun",
    "Model for ASQ-3",
    "Effect measure for ASQ-3",
    "Adjusted covariates",
    "Outcomes imputed",
    "Exposure imputation",
    "Binary exposure derivation",
    "Auxiliary variables",
    "Number of imputations",
    "Random forest ntree",
    "Robust SE"
  ),
  
  Value = c(
    "2gobun",
    "2gobun = 1",
    "ASQ3_communication",
    "childscreen24M",
    "1 hour/day increase",
    "childscreen24M_binary",
    "<1 hour/day",
    ">=1 hour/day vs <1 hour/day",
    "Poisson regression with robust SE",
    "Risk Ratio (RR)",
    "Linear regression",
    "Regression coefficient (beta)",
    "None",
    "No",
    "Random forest via mice",
    "Recreated from imputed childscreen24M",
    paste(
      c(
        "H4_P1",
        "age_corrected",
        "WHO5_all_100",
        "EPDS_18m",
        "mother_education_6grp"
      ),
      collapse = ", "
    ),
    as.character(m_imp),
    as.character(ntree_rf),
    "GEE sandwich SE (geeglm, std.err = san.se)"
  )
)


# ============================================================
# 27. Save results to Excel
# ============================================================

wb <- createWorkbook()


# ------------------------------------------------------------
# Regression results
# ------------------------------------------------------------

addWorksheet(
  wb,
  "regression_results"
)

writeData(
  wb,
  "regression_results",
  results
)


# ------------------------------------------------------------
# 2gobun distribution
# ------------------------------------------------------------

addWorksheet(
  wb,
  "2gobun_distribution"
)

writeData(
  wb,
  "2gobun_distribution",
  gobun_distribution
)


# ------------------------------------------------------------
# ASQ-3 summary
# ------------------------------------------------------------

addWorksheet(
  wb,
  "ASQ3_summary"
)

writeData(
  wb,
  "ASQ3_summary",
  asq3_summary
)


# ------------------------------------------------------------
# Binary screen-time distribution
# ------------------------------------------------------------

addWorksheet(
  wb,
  "screen_binary"
)

writeData(
  wb,
  "screen_binary",
  screen_binary_distribution
)


# ------------------------------------------------------------
# Continuous screen-time summary
# ------------------------------------------------------------

addWorksheet(
  wb,
  "screen_continuous"
)

writeData(
  wb,
  "screen_continuous",
  screen_continuous_summary
)


# ------------------------------------------------------------
# Missing values before MI
# ------------------------------------------------------------

addWorksheet(
  wb,
  "missing_before_MI"
)

writeData(
  wb,
  "missing_before_MI",
  missing_summary
)


# ------------------------------------------------------------
# Analysis sample
# ------------------------------------------------------------

addWorksheet(
  wb,
  "analysis_sample"
)

writeData(
  wb,
  "analysis_sample",
  analysis_sample
)


# ------------------------------------------------------------
# Model information
# ------------------------------------------------------------

addWorksheet(
  wb,
  "model_information"
)

writeData(
  wb,
  "model_information",
  model_information
)


# ------------------------------------------------------------
# Save Excel file
# ------------------------------------------------------------

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


cat("\n=====================================\n")
cat("ANALYSIS COMPLETED\n")
cat("=====================================\n")

cat("Output file:\n")
cat(output_file, "\n")