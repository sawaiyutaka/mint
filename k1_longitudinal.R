# ============================================================
# Screen time at 24 months and maternal/family factors
# Multiple imputation + unadjusted regression analyses
#
# Outcomes
#   1) childscreen24M: continuous hours/day
#      -> Linear regression; effect measure = Beta
#   2) screen_time_1h: 0 = <1 hour/day, 1 = >=1 hour/day
#      -> Poisson regression with robust SE; effect measure = RR
#
# Exposures (each entered separately; unadjusted models only)
#   1) age_corrected:       0 = >=25, 1 = <25 years
#   2) H4_P1:               0 = >=5, 1 = 1-4 (low income)
#   3) EPDS at 1/6/12/18m: 0 = <=8, 1 = >=9
#   4) E1 at 1/6/12/18m:   0 = 3-6, 1 = 1-2 (isolated)
#
# Multiple imputation
#   mice, random forest, m = 20, ntree = 100
#   Raw variables are imputed first; binary exposures are then derived.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

library(readxl)
library(mice)
library(dplyr)
library(openxlsx)
library(randomForest)
library(geepack)


# ============================================================
# 1. File settings
# ============================================================

input_file <- "D:/mint/data_xlsx/merged_selected_age_corrected_24_language.xlsx"

output_file <- paste0(
  "D:/mint/results/",
  "screen_time_maternal_factors_MI_unadjusted.xlsx"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

set.seed(12345)

m_imp <- 20
ntree_rf <- 100


# ============================================================
# 2. Variable settings
# ============================================================

id_var <- "users_id"
af_vars <- paste0("AF", 1:6)
outcome_cont <- "childscreen24M"

age_raw <- "age_corrected"
income_raw <- "H4_P1"

epds_vars <- c(
  "EPDS_1m",
  "EPDS_6m",
  "EPDS_12m",
  "EPDS_18m"
)

support_vars <- c(
  "E1_1m",
  "E1_6m",
  "E1_12m",
  "E1_18m"
)

education <- "mother_education_6grp"

required_columns <- c(
  id_var,
  af_vars,
  age_raw,
  income_raw,
  epds_vars,
  support_vars,
  education
)


# ============================================================
# 3. Read data and check columns
# ============================================================

df_raw <- read_excel(input_file)

names(df_raw) <- trimws(names(df_raw))

missing_columns <- setdiff(
  required_columns,
  names(df_raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (anyDuplicated(df_raw[[id_var]]) > 0) {
  stop(
    "users_id contains duplicates. ",
    "Check whether one row equals one participant."
  )
}

if (anyNA(df_raw[[id_var]])) {
  stop("users_id contains missing values.")
}

cat(
  "Number of rows:",
  nrow(df_raw),
  "\n"
)

cat(
  "Number of columns:",
  ncol(df_raw),
  "\n"
)


# ============================================================
# 4. Construct continuous screen time
# ============================================================

to_hours <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
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

df <- as.data.frame(df_raw)

df[af_vars] <- lapply(
  df[af_vars],
  as.numeric
)

for (v in af_vars) {
  df[[paste0(v, "_h")]] <- to_hours(
    df[[v]]
  )
}


# AF1, AF3, AF5 = weekdays
# AF2, AF4, AF6 = weekends
#
# If any of AF1-AF6 is missing,
# childscreen24M is set to missing.

df[[outcome_cont]] <- ifelse(
  rowSums(is.na(df[af_vars])) > 0,
  NA_real_,
  (
    (df$AF1_h + df$AF3_h + df$AF5_h) * 5 +
      (df$AF2_h + df$AF4_h + df$AF6_h) * 2
  ) / 7
)


# ============================================================
# 5. Screen-time summary before imputation
# ============================================================

screen_summary <- data.frame(
  n_total = nrow(df),
  
  n_observed = sum(
    !is.na(df[[outcome_cont]])
  ),
  
  n_missing = sum(
    is.na(df[[outcome_cont]])
  ),
  
  mean = mean(
    df[[outcome_cont]],
    na.rm = TRUE
  ),
  
  sd = sd(
    df[[outcome_cont]],
    na.rm = TRUE
  ),
  
  median = median(
    df[[outcome_cont]],
    na.rm = TRUE
  ),
  
  min = min(
    df[[outcome_cont]],
    na.rm = TRUE
  ),
  
  max = max(
    df[[outcome_cont]],
    na.rm = TRUE
  ),
  
  n_less_1h = sum(
    df[[outcome_cont]] < 1,
    na.rm = TRUE
  ),
  
  n_1h_or_more = sum(
    df[[outcome_cont]] >= 1,
    na.rm = TRUE
  )
)

print(screen_summary)


# ============================================================
# 6. Dataset for multiple imputation
# ============================================================

# All requested raw exposure variables are included jointly
# in the imputation model.
#
# Education is retained as an imputation-only variable,
# following the original code.

analysis_columns <- c(
  id_var,
  outcome_cont,
  age_raw,
  income_raw,
  epds_vars,
  support_vars,
  education
)

dat <- df[
  ,
  analysis_columns,
  drop = FALSE
]


# Exclude rows in which every variable other than ID is missing

non_id_vars <- setdiff(
  names(dat),
  id_var
)

dat <- dat[
  rowSums(
    !is.na(
      dat[, non_id_vars, drop = FALSE]
    )
  ) > 0,
  ,
  drop = FALSE
]

cat(
  "Number of participants used for MI:",
  nrow(dat),
  "\n"
)


# ============================================================
# 7. Variable types for imputation
# ============================================================

dat[[outcome_cont]] <- as.numeric(
  dat[[outcome_cont]]
)

dat[[age_raw]] <- as.numeric(
  dat[[age_raw]]
)

for (v in epds_vars) {
  dat[[v]] <- as.numeric(
    dat[[v]]
  )
}


# Treat ordinal/categorical response variables as factors
# in random-forest imputation

dat[[income_raw]] <- factor(
  dat[[income_raw]]
)

for (v in support_vars) {
  dat[[v]] <- factor(
    dat[[v]],
    levels = 1:6
  )
}

dat[[education]] <- factor(
  dat[[education]]
)


# ============================================================
# 8. Missing-value summary before imputation
# ============================================================

missing_summary <- data.frame(
  variable = names(dat),
  
  n_missing = sapply(
    dat,
    function(x) sum(is.na(x))
  ),
  
  prop_missing = sapply(
    dat,
    function(x) mean(is.na(x))
  )
)

print(missing_summary)


# ============================================================
# 9. Multiple imputation using random forest
# ============================================================

ini <- mice(
  dat,
  maxit = 0,
  printFlag = FALSE
)

meth <- ini$method
pred <- ini$predictorMatrix


# Impute every incomplete non-ID variable by random forest

meth[] <- ""

vars_with_missing <- names(dat)[
  colSums(is.na(dat)) > 0
]

meth[vars_with_missing] <- "rf"


# Do not impute ID and do not use ID as a predictor

meth[id_var] <- ""

pred[, id_var] <- 0
pred[id_var, ] <- 0

diag(pred) <- 0


cat(
  "\nVariables imputed by random forest:\n"
)

print(
  names(meth)[meth == "rf"]
)


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


completed_list <- complete(
  imp,
  action = "all"
)


# ============================================================
# 10. Functions to derive binary exposures after imputation
# ============================================================

factor_to_numeric <- function(x) {
  if (is.factor(x)) {
    return(
      as.numeric(
        as.character(x)
      )
    )
  }
  
  as.numeric(x)
}


derive_analysis_variables <- function(d) {
  
  # ----------------------------------------------------------
  # Binary screen-time outcome
  #
  # 0 = <1 hour/day
  # 1 = >=1 hour/day
  # ----------------------------------------------------------
  
  d$screen_time_1h <- ifelse(
    d[[outcome_cont]] >= 1,
    1,
    0
  )
  
  
  # ----------------------------------------------------------
  # Maternal age
  #
  # 0 = >=25 years: reference
  # 1 = <25 years: exposed
  # ----------------------------------------------------------
  
  age_num <- factor_to_numeric(
    d[[age_raw]]
  )
  
  d$age_under25 <- dplyr::case_when(
    is.na(age_num) ~ NA_real_,
    age_num >= 25 ~ 0,
    age_num < 25 ~ 1,
    TRUE ~ NA_real_
  )
  
  
  # ----------------------------------------------------------
  # Household income
  #
  # 0 = H4_P1 >=5: reference
  # 1 = H4_P1 1-4: low-income exposure
  # ----------------------------------------------------------
  
  h4_num <- factor_to_numeric(
    d[[income_raw]]
  )
  
  d$low_income <- dplyr::case_when(
    is.na(h4_num) ~ NA_real_,
    h4_num >= 1 & h4_num <= 2 ~ 1,
    h4_num >= 3 ~ 0,
    TRUE ~ NA_real_
  )
  
  
  # ----------------------------------------------------------
  # EPDS
  #
  # 0 = EPDS <=8: reference
  # 1 = EPDS >=9: exposed
  # ----------------------------------------------------------
  
  for (v in epds_vars) {
    
    x <- factor_to_numeric(
      d[[v]]
    )
    
    new_name <- paste0(
      v,
      "_9plus"
    )
    
    d[[new_name]] <- dplyr::case_when(
      is.na(x) ~ NA_real_,
      x >= 0 & x <= 8 ~ 0,
      x >= 9 ~ 1,
      TRUE ~ NA_real_
    )
  }
  
  
  # ----------------------------------------------------------
  # Social support
  #
  # 0 = E1 3-6: reference
  # 1 = E1 1-2: isolated/exposed
  # ----------------------------------------------------------
  
  for (v in support_vars) {
    
    x <- factor_to_numeric(
      d[[v]]
    )
    
    new_name <- paste0(
      v,
      "_isolated"
    )
    
    d[[new_name]] <- dplyr::case_when(
      is.na(x) ~ NA_real_,
      x >= 1 & x <= 2 ~ 1,
      x >= 3 & x <= 6 ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  d
}


# ============================================================
# 11. Exposure definitions
# ============================================================

exposure_info <- data.frame(
  
  exposure = c(
    "age_under25",
    "low_income",
    paste0(epds_vars, "_9plus"),
    paste0(support_vars, "_isolated")
  ),
  
  exposure_label = c(
    "Maternal age <25 years",
    "Low household income at P1 (H4_P1 = 1-4)",
    "EPDS >=9 at 1 month postpartum",
    "EPDS >=9 at 6 months postpartum",
    "EPDS >=9 at 12 months postpartum",
    "EPDS >=9 at 18 months postpartum",
    "Social isolation at 1 month postpartum (E1 = 1-2)",
    "Social isolation at 6 months postpartum (E1 = 1-2)",
    "Social isolation at 12 months postpartum (E1 = 1-2)",
    "Social isolation at 18 months postpartum (E1 = 1-2)"
  ),
  
  reference_group = c(
    "age_corrected >=25 years",
    "H4_P1 >=5",
    rep("EPDS <=8", 4),
    rep("E1 = 3-6", 4)
  ),
  
  exposed_group = c(
    "age_corrected <25 years",
    "H4_P1 = 1-4",
    rep("EPDS >=9", 4),
    rep("E1 = 1-2", 4)
  ),
  
  stringsAsFactors = FALSE
)

exposure_vars <- exposure_info$exposure


# ============================================================
# 12. Fit all unadjusted models in each imputed dataset
# ============================================================

linear_model_lists <- setNames(
  vector(
    "list",
    length(exposure_vars)
  ),
  exposure_vars
)

poisson_model_lists <- setNames(
  vector(
    "list",
    length(exposure_vars)
  ),
  exposure_vars
)

exposure_count_list <- vector(
  "list",
  length(completed_list)
)

outcome_count_list <- vector(
  "list",
  length(completed_list)
)


for (i in seq_along(completed_list)) {
  
  d <- derive_analysis_variables(
    completed_list[[i]]
  )
  
  
  # One participant per cluster, matching the robust Poisson
  # approach used in the original code
  
  d$gee_id <- match(
    d[[id_var]],
    unique(d[[id_var]])
  )
  
  stopifnot(
    length(unique(d$gee_id)) == nrow(d),
    !anyNA(d$gee_id)
  )
  
  
  # Fit each exposure separately
  
  for (exposure in exposure_vars) {
    
    linear_formula <- reformulate(
      exposure,
      response = outcome_cont
    )
    
    poisson_formula <- reformulate(
      exposure,
      response = "screen_time_1h"
    )
    
    
    # --------------------------------------------------------
    # Unadjusted linear regression
    # --------------------------------------------------------
    
    linear_model_lists[[exposure]][[i]] <- lm(
      formula = linear_formula,
      data = d
    )
    
    
    # --------------------------------------------------------
    # Unadjusted Poisson regression with robust SE
    # --------------------------------------------------------
    
    poisson_model_lists[[exposure]][[i]] <- geeglm(
      formula = poisson_formula,
      id = gee_id,
      data = d,
      family = poisson(
        link = "log"
      ),
      corstr = "independence",
      std.err = "san.se"
    )
  }
  
  
  # ----------------------------------------------------------
  # Exposure counts in each imputed dataset
  # ----------------------------------------------------------
  
  exposure_count_list[[i]] <- bind_rows(
    lapply(
      exposure_vars,
      function(v) {
        data.frame(
          imputation = i,
          
          exposure = v,
          
          group = c(
            "reference_0",
            "exposed_1"
          ),
          
          n = c(
            sum(d[[v]] == 0),
            sum(d[[v]] == 1)
          )
        )
      }
    )
  )
  
  
  # ----------------------------------------------------------
  # Outcome counts in each imputed dataset
  # ----------------------------------------------------------
  
  outcome_count_list[[i]] <- data.frame(
    imputation = i,
    
    group = c(
      "less_than_1h",
      "1h_or_more"
    ),
    
    n = c(
      sum(d$screen_time_1h == 0),
      sum(d$screen_time_1h == 1)
    )
  )
}


# ============================================================
# 13. Pool linear regression results
# ============================================================

linear_results <- bind_rows(
  lapply(
    exposure_vars,
    function(exposure) {
      
      pooled <- pool(
        as.mira(
          linear_model_lists[[exposure]]
        )
      )
      
      result <- summary(
        pooled,
        conf.int = TRUE
      ) %>%
        as.data.frame() %>%
        filter(
          term == exposure
        )
      
      info <- exposure_info[
        exposure_info$exposure == exposure,
      ]
      
      result %>%
        mutate(
          exposure = exposure,
          exposure_label = info$exposure_label,
          reference_group = info$reference_group,
          exposed_group = info$exposed_group,
          outcome = "childscreen24M (hours/day)",
          model = "Unadjusted linear regression",
          effect_measure = "Beta"
        )
    }
  )
) %>%
  select(
    exposure,
    exposure_label,
    reference_group,
    exposed_group,
    outcome,
    model,
    effect_measure,
    everything()
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )


# ============================================================
# 14. Pool robust Poisson regression results
# ============================================================

poisson_results <- bind_rows(
  lapply(
    exposure_vars,
    function(exposure) {
      
      pooled <- pool(
        as.mira(
          poisson_model_lists[[exposure]]
        )
      )
      
      result <- summary(
        pooled,
        conf.int = TRUE,
        exponentiate = TRUE
      ) %>%
        as.data.frame() %>%
        filter(
          term == exposure
        )
      
      info <- exposure_info[
        exposure_info$exposure == exposure,
      ]
      
      result %>%
        mutate(
          exposure = exposure,
          exposure_label = info$exposure_label,
          reference_group = info$reference_group,
          exposed_group = info$exposed_group,
          outcome = "screen_time_1h (1 = >=1 hour/day)",
          model = paste0(
            "Unadjusted Poisson regression ",
            "with robust SE"
          ),
          effect_measure = "Risk Ratio (RR)"
        )
    }
  )
) %>%
  select(
    exposure,
    exposure_label,
    reference_group,
    exposed_group,
    outcome,
    model,
    effect_measure,
    everything()
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )


# ============================================================
# 15. Summarize counts across imputations
# ============================================================

exposure_counts_MI <- bind_rows(
  exposure_count_list
) %>%
  group_by(
    exposure,
    group
  ) %>%
  summarise(
    mean_n = mean(n),
    min_n = min(n),
    max_n = max(n),
    .groups = "drop"
  ) %>%
  left_join(
    exposure_info,
    by = "exposure"
  ) %>%
  select(
    exposure,
    exposure_label,
    reference_group,
    exposed_group,
    group,
    mean_n,
    min_n,
    max_n
  )


outcome_counts_MI <- bind_rows(
  outcome_count_list
) %>%
  group_by(
    group
  ) %>%
  summarise(
    mean_n = mean(n),
    min_n = min(n),
    max_n = max(n),
    .groups = "drop"
  )


# ============================================================
# 16. Model formulas and analysis information
# ============================================================

model_formulas <- bind_rows(
  lapply(
    seq_len(nrow(exposure_info)),
    function(i) {
      
      ex <- exposure_info$exposure[i]
      
      data.frame(
        exposure = ex,
        
        exposure_label = exposure_info$exposure_label[i],
        
        linear_formula = paste(
          outcome_cont,
          "~",
          ex
        ),
        
        poisson_formula = paste(
          "screen_time_1h ~",
          ex
        ),
        
        adjusted_for = "None"
      )
    }
  )
)


model_information <- data.frame(
  
  item = c(
    "Continuous outcome",
    "Continuous-outcome model",
    "Continuous effect measure",
    "Binary outcome",
    "Binary event",
    "Binary-outcome model",
    "Binary effect measure",
    "Adjustment",
    "Imputation method",
    "Number of imputations",
    "Random forest trees",
    "MICE iterations",
    "Imputation-only variable",
    "Poisson robust SE method"
  ),
  
  value = c(
    "childscreen24M, hours/day",
    "Linear regression",
    "Beta coefficient",
    "screen_time_1h",
    "1 = >=1 hour/day; 0 = <1 hour/day",
    "Poisson regression with robust SE",
    "Risk Ratio (RR)",
    "None; each exposure was analyzed separately",
    "Random forest via mice",
    as.character(m_imp),
    as.character(ntree_rf),
    "20",
    "mother_education_6grp",
    "GEE sandwich SE (geeglm, std.err = san.se)"
  )
)


# ============================================================
# 17. Print main results
# ============================================================

cat(
  "\n=====================================\n"
)

cat(
  "UNADJUSTED LINEAR REGRESSION\n"
)

cat(
  "=====================================\n"
)

print(
  linear_results
)


cat(
  "\n=====================================\n"
)

cat(
  "UNADJUSTED POISSON REGRESSION WITH ROBUST SE\n"
)

cat(
  "=====================================\n"
)

print(
  poisson_results
)


# ============================================================
# 18. Save results to Excel
# ============================================================

wb <- createWorkbook()


# Linear regression

addWorksheet(
  wb,
  "linear_unadjusted"
)

writeData(
  wb,
  "linear_unadjusted",
  linear_results
)


# Poisson regression with robust SE

addWorksheet(
  wb,
  "poisson_robust_unadj"
)

writeData(
  wb,
  "poisson_robust_unadj",
  poisson_results
)


# Exposure definitions

addWorksheet(
  wb,
  "exposure_definitions"
)

writeData(
  wb,
  "exposure_definitions",
  exposure_info
)


# Model formulas

addWorksheet(
  wb,
  "model_formulas"
)

writeData(
  wb,
  "model_formulas",
  model_formulas
)


# Screen-time summary

addWorksheet(
  wb,
  "screen_summary"
)

writeData(
  wb,
  "screen_summary",
  screen_summary
)


# Exposure counts after MI

addWorksheet(
  wb,
  "exposure_counts_MI"
)

writeData(
  wb,
  "exposure_counts_MI",
  exposure_counts_MI
)


# Outcome counts after MI

addWorksheet(
  wb,
  "outcome_counts_MI"
)

writeData(
  wb,
  "outcome_counts_MI",
  outcome_counts_MI
)


# Missing-value summary

addWorksheet(
  wb,
  "missing_summary"
)

writeData(
  wb,
  "missing_summary",
  missing_summary
)


# Model information

addWorksheet(
  wb,
  "model_information"
)

writeData(
  wb,
  "model_information",
  model_information
)


# Save workbook

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)


cat(
  "\n=====================================\n"
)

cat(
  "Analysis completed successfully.\n"
)

cat(
  "Output file:\n",
  output_file,
  "\n"
)

cat(
  "=====================================\n"
)