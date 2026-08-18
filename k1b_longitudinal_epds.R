# ============================================================
# Continuous EPDS score and screen time at 24 months
# Multiple imputation + unadjusted robust Poisson regression
#
# Outcome
#   screen_time_1h:
#     0 = <1 hour/day
#     1 = >=1 hour/day
#
# Exposures
#   EPDS_1m, EPDS_6m, EPDS_12m, EPDS_18m
#   Each EPDS score is used as a continuous variable.
#   The four time points are analyzed in separate models.
#
# Effect measure
#   Risk Ratio (RR) per 1-point increase in EPDS score
#
# Adjustment
#   Unadjusted models only
#
# Multiple imputation
#   mice, random forest, m = 20, ntree = 100
#   Continuous EPDS scores are imputed without dichotomization.
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
library(broom)


# ============================================================
# 1. File settings
# ============================================================

input_file <- paste0(
  "D:/mint/data_xlsx/",
  "merged_selected_age_corrected_24_language.xlsx"
)

output_file <- paste0(
  "D:/mint/results/",
  "EPDS_continuous_RR_per_1point_MI_unadjusted.xlsx"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

set.seed(12345)

m_imp <- 20
ntree_rf <- 100
mice_maxit <- 20


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

# age, income, support, and education are not included in the
# regression models. They are retained as auxiliary variables
# in the imputation model, following the attached original code.

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

cat("Number of rows:", nrow(df_raw), "\n")
cat("Number of columns:", ncol(df_raw), "\n")


# ============================================================
# 4. Construct continuous and binary screen-time outcomes
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
  df[[paste0(v, "_h")]] <- to_hours(df[[v]])
}

# AF1, AF3, AF5 = weekdays
# AF2, AF4, AF6 = weekends
# If any of AF1-AF6 is missing, childscreen24M is missing.

df[[outcome_cont]] <- ifelse(
  rowSums(is.na(df[af_vars])) > 0,
  NA_real_,
  (
    (df$AF1_h + df$AF3_h + df$AF5_h) * 5 +
      (df$AF2_h + df$AF4_h + df$AF6_h) * 2
  ) / 7
)

# screen_time_1h will be derived after imputation.
# 0 = <1 hour/day; 1 = >=1 hour/day


# ============================================================
# 5. Observed-data summaries before imputation
# ============================================================

screen_summary_observed <- data.frame(
  n_total = nrow(df),
  n_observed = sum(!is.na(df[[outcome_cont]])),
  n_missing = sum(is.na(df[[outcome_cont]])),
  mean_hours = mean(df[[outcome_cont]], na.rm = TRUE),
  sd_hours = sd(df[[outcome_cont]], na.rm = TRUE),
  median_hours = median(df[[outcome_cont]], na.rm = TRUE),
  min_hours = min(df[[outcome_cont]], na.rm = TRUE),
  max_hours = max(df[[outcome_cont]], na.rm = TRUE),
  n_less_1h = sum(df[[outcome_cont]] < 1, na.rm = TRUE),
  n_1h_or_more = sum(df[[outcome_cont]] >= 1, na.rm = TRUE)
)

epds_summary_observed <- bind_rows(
  lapply(
    epds_vars,
    function(v) {
      x <- suppressWarnings(as.numeric(df[[v]]))
      
      data.frame(
        variable = v,
        n_total = length(x),
        n_observed = sum(!is.na(x)),
        n_missing = sum(is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    }
  )
)

print(screen_summary_observed)
print(epds_summary_observed)


# ============================================================
# 6. Dataset for multiple imputation
# ============================================================

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

# Exclude rows in which every variable other than ID is missing.

non_id_vars <- setdiff(names(dat), id_var)

dat <- dat[
  rowSums(
    !is.na(dat[, non_id_vars, drop = FALSE])
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

dat[[outcome_cont]] <- as.numeric(dat[[outcome_cont]])
dat[[age_raw]] <- as.numeric(dat[[age_raw]])

# EPDS remains continuous.

for (v in epds_vars) {
  dat[[v]] <- as.numeric(dat[[v]])
}

# Ordinal/categorical auxiliary variables are treated as factors.

dat[[income_raw]] <- factor(dat[[income_raw]])

for (v in support_vars) {
  dat[[v]] <- factor(
    dat[[v]],
    levels = 1:6
  )
}

dat[[education]] <- factor(dat[[education]])


# ============================================================
# 8. Missing-value summary before imputation
# ============================================================

missing_summary <- data.frame(
  variable = names(dat),
  n_missing = sapply(dat, function(x) sum(is.na(x))),
  prop_missing = sapply(dat, function(x) mean(is.na(x)))
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

# Impute every incomplete non-ID variable by random forest.

meth[] <- ""

vars_with_missing <- names(dat)[
  colSums(is.na(dat)) > 0
]

meth[vars_with_missing] <- "rf"

# Do not impute ID and do not use ID as a predictor.

meth[id_var] <- ""
pred[, id_var] <- 0
pred[id_var, ] <- 0
diag(pred) <- 0

cat("\nVariables imputed by random forest:\n")
print(names(meth)[meth == "rf"])

imp <- mice(
  dat,
  m = m_imp,
  maxit = mice_maxit,
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
# 10. Exposure definitions
# ============================================================

exposure_info <- data.frame(
  exposure = epds_vars,
  
  exposure_label = c(
    "EPDS score at 1 month postpartum",
    "EPDS score at 6 months postpartum",
    "EPDS score at 12 months postpartum",
    "EPDS score at 18 months postpartum"
  ),
  
  exposure_unit = rep(
    "Per 1-point increase in EPDS score",
    length(epds_vars)
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# 11. Fit robust Poisson models in each imputed dataset
# ============================================================

poisson_model_lists <- setNames(
  vector("list", length(epds_vars)),
  epds_vars
)

epds_summary_MI_list <- vector(
  "list",
  length(completed_list)
)

outcome_count_list <- vector(
  "list",
  length(completed_list)
)

for (i in seq_along(completed_list)) {
  
  d <- completed_list[[i]]
  
  # Binary outcome derived from imputed continuous screen time.
  
  d$screen_time_1h <- ifelse(
    d[[outcome_cont]] >= 1,
    1,
    0
  )
  
  # One participant per cluster, matching the robust Poisson
  # approach in the attached original code.
  
  d$gee_id <- match(
    d[[id_var]],
    unique(d[[id_var]])
  )
  
  stopifnot(
    length(unique(d$gee_id)) == nrow(d),
    !anyNA(d$gee_id)
  )
  
  # Each EPDS time point is analyzed in a separate model.
  
  for (exposure in epds_vars) {
    
    poisson_formula <- reformulate(
      exposure,
      response = "screen_time_1h"
    )
    
    poisson_model_lists[[exposure]][[i]] <- geeglm(
      formula = poisson_formula,
      id = gee_id,
      data = d,
      family = poisson(link = "log"),
      corstr = "independence",
      std.err = "san.se"
    )
  }
  
  # Descriptive summaries after imputation.
  
  epds_summary_MI_list[[i]] <- bind_rows(
    lapply(
      epds_vars,
      function(v) {
        data.frame(
          imputation = i,
          variable = v,
          mean = mean(d[[v]]),
          sd = sd(d[[v]]),
          median = median(d[[v]]),
          min = min(d[[v]]),
          max = max(d[[v]])
        )
      }
    )
  )
  
  outcome_count_list[[i]] <- data.frame(
    imputation = i,
    group = c("less_than_1h", "1h_or_more"),
    n = c(
      sum(d$screen_time_1h == 0),
      sum(d$screen_time_1h == 1)
    )
  )
}


# ============================================================
# 12. Pool robust Poisson regression results
# ============================================================

poisson_results <- bind_rows(
  lapply(
    epds_vars,
    function(exposure) {
      
      pooled <- pool(
        as.mira(poisson_model_lists[[exposure]])
      )
      
      result <- summary(
        pooled,
        conf.int = TRUE,
        exponentiate = TRUE
      ) %>%
        as.data.frame() %>%
        filter(term == exposure)
      
      info <- exposure_info[
        exposure_info$exposure == exposure,
        ,
        drop = FALSE
      ]
      
      result %>%
        mutate(
          exposure = exposure,
          exposure_label = info$exposure_label,
          exposure_unit = info$exposure_unit,
          outcome = "screen_time_1h (1 = >=1 hour/day)",
          model = paste0(
            "Unadjusted Poisson regression ",
            "with robust SE"
          ),
          effect_measure = paste0(
            "Risk Ratio per 1-point increase in EPDS"
          )
        )
    }
  )
) %>%
  select(
    exposure,
    exposure_label,
    exposure_unit,
    outcome,
    model,
    effect_measure,
    everything()
  ) %>%
  rename(
    RR = estimate,
    CI_lower_95 = `2.5 %`,
    CI_upper_95 = `97.5 %`
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )


# ============================================================
# 13. Summaries across imputations
# ============================================================

epds_summary_MI <- bind_rows(
  epds_summary_MI_list
) %>%
  group_by(variable) %>%
  summarise(
    mean_of_means = mean(mean),
    min_mean = min(mean),
    max_mean = max(mean),
    mean_of_sds = mean(sd),
    min_observed_value = min(min),
    max_observed_value = max(max),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )

outcome_counts_MI <- bind_rows(
  outcome_count_list
) %>%
  group_by(group) %>%
  summarise(
    mean_n = mean(n),
    min_n = min(n),
    max_n = max(n),
    .groups = "drop"
  )


# ============================================================
# 14. Model formulas and analysis information
# ============================================================

model_formulas <- data.frame(
  exposure = epds_vars,
  formula = paste("screen_time_1h ~", epds_vars),
  adjusted_for = "None",
  stringsAsFactors = FALSE
)

model_information <- data.frame(
  item = c(
    "Outcome",
    "Event",
    "Exposure",
    "Exposure unit",
    "Models",
    "Regression model",
    "Effect measure",
    "Adjustment",
    "Imputation method",
    "Number of imputations",
    "Random forest trees",
    "MICE iterations",
    "Imputation auxiliary variables",
    "Poisson robust SE method"
  ),
  value = c(
    "screen_time_1h",
    "1 = >=1 hour/day; 0 = <1 hour/day",
    "EPDS_1m, EPDS_6m, EPDS_12m, EPDS_18m",
    "1 point; EPDS was not dichotomized",
    "Each EPDS time point was analyzed separately",
    "Poisson regression with robust SE",
    "Risk Ratio (RR) per 1-point increase in EPDS",
    "None",
    "Random forest via mice",
    as.character(m_imp),
    as.character(ntree_rf),
    as.character(mice_maxit),
    paste(
      c(age_raw, income_raw, support_vars, education),
      collapse = ", "
    ),
    "GEE sandwich SE (geeglm, std.err = san.se)"
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 15. Print main results
# ============================================================

cat("\n=====================================\n")
cat("EPDS AS A CONTINUOUS EXPOSURE\n")
cat("RR PER 1-POINT INCREASE IN EPDS\n")
cat("=====================================\n")

print(poisson_results)


# ============================================================
# 16. Save results to Excel
# ============================================================

wb <- createWorkbook()

addWorksheet(wb, "RR_per_1point_EPDS")
writeData(wb, "RR_per_1point_EPDS", poisson_results)

addWorksheet(wb, "exposure_definitions")
writeData(wb, "exposure_definitions", exposure_info)

addWorksheet(wb, "model_formulas")
writeData(wb, "model_formulas", model_formulas)

addWorksheet(wb, "screen_summary_observed")
writeData(wb, "screen_summary_observed", screen_summary_observed)

addWorksheet(wb, "EPDS_summary_observed")
writeData(wb, "EPDS_summary_observed", epds_summary_observed)

addWorksheet(wb, "EPDS_summary_MI")
writeData(wb, "EPDS_summary_MI", epds_summary_MI)

addWorksheet(wb, "outcome_counts_MI")
writeData(wb, "outcome_counts_MI", outcome_counts_MI)

addWorksheet(wb, "missing_summary")
writeData(wb, "missing_summary", missing_summary)

addWorksheet(wb, "model_information")
writeData(wb, "model_information", model_information)

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)

cat("\n=====================================\n")
cat("Analysis completed successfully.\n")
cat("Output file:\n", output_file, "\n")
cat("=====================================\n")
