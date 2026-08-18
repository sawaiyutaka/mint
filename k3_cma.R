# ============================================================
# Causal mediation analysis:
# Low household income -> low G3 group -> screen time at 24 months
#
# Exposure
#   low_income:
#     0 = H4_P1 >= 5 (reference)
#     1 = H4_P1 1-4 (low income)
#
# Mediator
#   G3_below_median:
#     0 = G3 >= the observed-data median (reference)
#     1 = G3 <  the observed-data median
#
# Outcomes
#   1) childscreen24M: continuous hours/day
#   2) screen_time_1h:
#        0 = <1 hour/day
#        1 = >=1 hour/day
#
# Missing data
#   mice random forest, m = 20, ntree = 100
#   H4_P1 and G3 are imputed in their original forms first.
#   Binary exposure, mediator, and outcome are derived afterward.
#
# Important
#   The attached source analysis was unadjusted. Therefore, basec_vars is
#   empty below. For causal interpretation, specify an appropriate set of
#   PRE-EXPOSURE confounders based on a DAG.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

library(readxl)
library(mice)
library(dplyr)
library(openxlsx)
library(randomForest)
library(CMAverse)


# ============================================================
# 1. File and analysis settings
# ============================================================

input_file <- "D:/mint/data_xlsx/merged_selected_age_corrected_24_language.xlsx"

output_file <- paste0(
  "D:/mint/results/",
  "CMA_low_income_G3_screen_time_MI.xlsx"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

seed_main <- 12345
set.seed(seed_main)

m_imp <- 20
ntree_rf <- 100
mice_maxit <- 20

# Number of bootstrap samples used for the binary-outcome CMA.
# Increase to 1000 or 2000 for the final analysis if computation permits.
nboot_cma <- 500

# TRUE includes the exposure-mediator interaction in the outcome model and
# provides VanderWeele's four-way decomposition.
include_em_interaction <- TRUE


# ============================================================
# 2. Variable settings
# ============================================================

id_var <- "users_id"
af_vars <- paste0("AF", 1:6)

outcome_cont <- "childscreen24M"
outcome_binary <- "screen_time_1h"

income_raw <- "H4_P1"
g3_raw <- "G3"

exposure_binary <- "low_income"
mediator_binary <- "G3_below_median"

age_raw <- "age_corrected"

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

# The source code fitted unadjusted models, so this is empty by default.
# Add only appropriate pre-exposure confounders after confirming the DAG.
# Example (only if substantively justified):
# basec_vars <- c("age_corrected", "mother_education_6grp")
basec_vars <- character(0)

required_columns <- unique(c(
  id_var,
  af_vars,
  income_raw,
  g3_raw,
  age_raw,
  epds_vars,
  support_vars,
  education,
  basec_vars
))


# ============================================================
# 3. Utility functions
# ============================================================

factor_to_numeric <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(as.character(x)))
  }
  
  as.numeric(x)
}


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


# Rubin pooling for scalar estimates.
# For ratio effects, estimates are pooled on the log scale.
pool_scalar_rubin <- function(q, u, transform = c("identity", "log")) {
  transform <- match.arg(transform)
  
  keep <- is.finite(q) & is.finite(u) & u >= 0
  q <- q[keep]
  u <- u[keep]
  
  m <- length(q)
  
  if (m < 2) {
    if (transform == "log") {
      return(data.frame(
        estimate = NA_real_,
        std_error_log = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        df = NA_real_,
        m_used = m
      ))
    }
    
    return(data.frame(
      estimate = NA_real_,
      std_error = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      p_value = NA_real_,
      df = NA_real_,
      m_used = m
    ))
  }
  
  if (transform == "log") {
    if (any(q <= 0)) {
      stop("A ratio estimate was non-positive and could not be log-pooled.")
    }
    
    # Delta-method conversion from SE on the ratio scale to SE on log scale.
    u <- u / (q^2)
    q <- log(q)
  }
  
  q_bar <- mean(q)
  u_bar <- mean(u)
  b <- stats::var(q)
  total_var <- u_bar + (1 + 1 / m) * b
  se_total <- sqrt(total_var)
  
  if (!is.finite(b) || b <= .Machine$double.eps) {
    df <- Inf
    critical <- stats::qnorm(0.975)
    p_value <- 2 * stats::pnorm(-abs(q_bar / se_total))
  } else {
    df <- (m - 1) * (
      1 + u_bar / ((1 + 1 / m) * b)
    )^2
    
    critical <- stats::qt(0.975, df = df)
    p_value <- 2 * stats::pt(
      -abs(q_bar / se_total),
      df = df
    )
  }
  
  conf_low <- q_bar - critical * se_total
  conf_high <- q_bar + critical * se_total
  
  if (transform == "log") {
    return(data.frame(
      estimate = exp(q_bar),
      std_error_log = se_total,
      conf_low = exp(conf_low),
      conf_high = exp(conf_high),
      p_value = p_value,
      df = df,
      m_used = m
    ))
  }
  
  data.frame(
    estimate = q_bar,
    std_error = se_total,
    conf_low = conf_low,
    conf_high = conf_high,
    p_value = p_value,
    df = df,
    m_used = m
  )
}


extract_cmest_effects <- function(fit, imputation, outcome_label) {
  data.frame(
    imputation = imputation,
    outcome = outcome_label,
    effect = names(fit$effect.pe),
    estimate = as.numeric(fit$effect.pe),
    std_error = as.numeric(fit$effect.se),
    stringsAsFactors = FALSE
  )
}


pool_cmest_effects <- function(effect_data, outcome_type) {
  effect_names <- unique(effect_data$effect)
  
  bind_rows(lapply(effect_names, function(effect_name) {
    x <- effect_data[
      effect_data$effect == effect_name,
      ,
      drop = FALSE
    ]
    
    # In CMAverse, the first six binary/categorical-outcome effects are
    # ratios: Rcde, Rpnde, Rtnde, Rpnie, Rtnie, and Rte.
    is_ratio <- outcome_type == "binary" &&
      effect_name %in% c(
        "Rcde", "Rpnde", "Rtnde",
        "Rpnie", "Rtnie", "Rte"
      )
    
    pooled <- pool_scalar_rubin(
      q = x$estimate,
      u = x$std_error^2,
      transform = ifelse(is_ratio, "log", "identity")
    )
    
    pooled$outcome <- unique(x$outcome)
    pooled$effect <- effect_name
    pooled$scale <- ifelse(
      is_ratio,
      "ratio (pooled on log scale)",
      "additive/proportion scale"
    )
    
    pooled
  })) %>%
    select(
      outcome,
      effect,
      scale,
      everything()
    )
}


# ============================================================
# 4. Read data and check columns
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

df <- as.data.frame(df_raw)

cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")


# ============================================================
# 5. Construct continuous screen time
# ============================================================

df[af_vars] <- lapply(df[af_vars], as.numeric)

for (v in af_vars) {
  df[[paste0(v, "_h")]] <- to_hours(df[[v]])
}

# AF1, AF3, AF5 = weekdays
# AF2, AF4, AF6 = weekends
# If any of AF1-AF6 is missing, childscreen24M is missing before MI.

df[[outcome_cont]] <- ifelse(
  rowSums(is.na(df[af_vars])) > 0,
  NA_real_,
  (
    (df$AF1_h + df$AF3_h + df$AF5_h) * 5 +
      (df$AF2_h + df$AF4_h + df$AF6_h) * 2
  ) / 7
)


# ============================================================
# 6. Fix the G3 median cutoff before multiple imputation
# ============================================================

g3_observed_numeric <- factor_to_numeric(df[[g3_raw]])

if (all(is.na(g3_observed_numeric))) {
  stop("All observed G3 values are missing.")
}

g3_cutoff <- median(
  g3_observed_numeric,
  na.rm = TRUE
)

cat("Observed-data median of G3:", g3_cutoff, "\n")


# ============================================================
# 7. Dataset for multiple imputation
# ============================================================

# Variables from the attached code are retained as auxiliary variables.
# Any variables placed in basec_vars are also added automatically.

analysis_columns <- unique(c(
  id_var,
  outcome_cont,
  income_raw,
  g3_raw,
  age_raw,
  epds_vars,
  support_vars,
  education,
  basec_vars
))

dat <- df[, analysis_columns, drop = FALSE]

non_id_vars <- setdiff(names(dat), id_var)

dat <- dat[
  rowSums(!is.na(dat[, non_id_vars, drop = FALSE])) > 0,
  ,
  drop = FALSE
]

cat("Number of participants used for MI:", nrow(dat), "\n")


# ============================================================
# 8. Variable types for imputation
# ============================================================

dat[[outcome_cont]] <- as.numeric(dat[[outcome_cont]])
dat[[g3_raw]] <- factor_to_numeric(dat[[g3_raw]])
dat[[age_raw]] <- factor_to_numeric(dat[[age_raw]])

for (v in epds_vars) {
  dat[[v]] <- factor_to_numeric(dat[[v]])
}

# H4_P1 is imputed as its original categorical variable.
dat[[income_raw]] <- factor(dat[[income_raw]])

for (v in support_vars) {
  dat[[v]] <- factor(dat[[v]], levels = 1:6)
}

dat[[education]] <- factor(dat[[education]])


# ============================================================
# 9. Missing-value summary before imputation
# ============================================================

missing_summary <- data.frame(
  variable = names(dat),
  n_missing = sapply(dat, function(x) sum(is.na(x))),
  prop_missing = sapply(dat, function(x) mean(is.na(x)))
)

print(missing_summary)


# ============================================================
# 10. Multiple imputation using random forest
# ============================================================

ini <- mice(
  dat,
  maxit = 0,
  printFlag = FALSE
)

meth <- ini$method
pred <- ini$predictorMatrix

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
  seed = seed_main,
  printFlag = TRUE
)

completed_list <- complete(
  imp,
  action = "all"
)


# ============================================================
# 11. Derive exposure, mediator, and binary outcome after MI
# ============================================================

derive_cma_variables <- function(d, g3_cutoff) {
  h4_num <- factor_to_numeric(d[[income_raw]])
  g3_num <- factor_to_numeric(d[[g3_raw]])
  
  # Exposure:
  # 0 = H4_P1 >=5
  # 1 = H4_P1 1-4
  d[[exposure_binary]] <- dplyr::case_when(
    is.na(h4_num) ~ NA_real_,
    h4_num >= 1 & h4_num <= 4 ~ 1,
    h4_num >= 5 ~ 0,
    TRUE ~ NA_real_
  )
  
  # Mediator:
  # 0 = G3 >= median
  # 1 = G3 < median
  d[[mediator_binary]] <- dplyr::case_when(
    is.na(g3_num) ~ NA_real_,
    g3_num >= g3_cutoff ~ 0,
    g3_num < g3_cutoff ~ 1,
    TRUE ~ NA_real_
  )
  
  # Binary outcome:
  # 0 = <1 hour/day
  # 1 = >=1 hour/day
  d[[outcome_binary]] <- dplyr::case_when(
    is.na(d[[outcome_cont]]) ~ NA_real_,
    d[[outcome_cont]] < 1 ~ 0,
    d[[outcome_cont]] >= 1 ~ 1,
    TRUE ~ NA_real_
  )
  
  d
}


analysis_list <- lapply(
  completed_list,
  derive_cma_variables,
  g3_cutoff = g3_cutoff
)


# Validate coding and positivity in every imputed dataset.
for (i in seq_along(analysis_list)) {
  d <- analysis_list[[i]]
  
  vars_to_check <- c(
    exposure_binary,
    mediator_binary,
    outcome_cont,
    outcome_binary,
    basec_vars
  )
  
  if (anyNA(d[, vars_to_check, drop = FALSE])) {
    stop(
      "Missing values remained in CMA variables in imputation ",
      i,
      ". Check invalid source codes or the imputation model."
    )
  }
  
  if (!all(sort(unique(d[[exposure_binary]])) == c(0, 1))) {
    stop("Both exposure levels were not present in imputation ", i, ".")
  }
  
  if (!all(sort(unique(d[[mediator_binary]])) == c(0, 1))) {
    stop("Both mediator levels were not present in imputation ", i, ".")
  }
  
  if (!all(sort(unique(d[[outcome_binary]])) == c(0, 1))) {
    stop("Both binary outcome levels were not present in imputation ", i, ".")
  }
}


# ============================================================
# 12. Descriptive counts across imputations
# ============================================================

count_data <- bind_rows(lapply(seq_along(analysis_list), function(i) {
  d <- analysis_list[[i]]
  
  data.frame(
    imputation = i,
    low_income_0 = sum(d[[exposure_binary]] == 0),
    low_income_1 = sum(d[[exposure_binary]] == 1),
    G3_median_or_above_0 = sum(d[[mediator_binary]] == 0),
    G3_below_median_1 = sum(d[[mediator_binary]] == 1),
    screen_less_1h_0 = sum(d[[outcome_binary]] == 0),
    screen_1h_or_more_1 = sum(d[[outcome_binary]] == 1)
  )
}))

count_summary <- count_data %>%
  summarise(
    across(
      -imputation,
      list(mean = mean, min = min, max = max)
    )
  )


# ============================================================
# 13. Fit CMA in every imputed dataset
# ============================================================

basec_arg <- if (length(basec_vars) == 0) NULL else basec_vars

continuous_fits <- vector("list", length(analysis_list))
binary_fits <- vector("list", length(analysis_list))

continuous_effects_list <- vector("list", length(analysis_list))
binary_effects_list <- vector("list", length(analysis_list))

for (i in seq_along(analysis_list)) {
  d <- analysis_list[[i]]
  
  cat(
    "\nFitting CMA for imputation",
    i,
    "of",
    length(analysis_list),
    "...\n"
  )
  
  # ----------------------------------------------------------
  # Model 1: continuous screen-time outcome
  # Effect scale: difference in hours/day
  # ----------------------------------------------------------
  
  continuous_fits[[i]] <- cmest(
    data = d,
    model = "rb",
    outcome = outcome_cont,
    exposure = exposure_binary,
    mediator = mediator_binary,
    basec = basec_arg,
    EMint = include_em_interaction,
    mreg = list("logistic"),
    yreg = "linear",
    astar = 0,
    a = 1,
    mval = list(0),
    estimation = "paramfunc",
    inference = "delta"
  )
  
  continuous_effects_list[[i]] <- extract_cmest_effects(
    continuous_fits[[i]],
    imputation = i,
    outcome_label = "childscreen24M (continuous hours/day)"
  )
  
  # ----------------------------------------------------------
  # Model 2: binary screen-time outcome
  # Effect scale: ratio for natural/total effects
  #
  # Counterfactual imputation is used rather than logistic
  # paramfunc because >=1 hour/day may not be a rare outcome.
  # ----------------------------------------------------------
  
  set.seed(seed_main + i)
  
  binary_fits[[i]] <- cmest(
    data = d,
    model = "rb",
    outcome = outcome_binary,
    exposure = exposure_binary,
    mediator = mediator_binary,
    basec = basec_arg,
    EMint = include_em_interaction,
    mreg = list("logistic"),
    yreg = "logistic",
    astar = 0,
    a = 1,
    mval = list(0),
    yval = 1,
    estimation = "imputation",
    inference = "bootstrap",
    nboot = nboot_cma,
    boot.ci.type = "per"
  )
  
  binary_effects_list[[i]] <- extract_cmest_effects(
    binary_fits[[i]],
    imputation = i,
    outcome_label = "screen_time_1h (1 = >=1 hour/day)"
  )
}


# ============================================================
# 14. Pool CMA effects across the 20 imputations
# ============================================================

continuous_effects_by_imputation <- bind_rows(
  continuous_effects_list
)

binary_effects_by_imputation <- bind_rows(
  binary_effects_list
)

continuous_results_pooled <- pool_cmest_effects(
  continuous_effects_by_imputation,
  outcome_type = "continuous"
) %>%
  mutate(
    exposure_comparison = "low income (1) vs H4_P1 >=5 (0)",
    mediator_reference = paste0(
      "G3 >= observed median (G3 >= ",
      g3_cutoff,
      ")"
    ),
    model = "Regression-based CMA; linear outcome; logistic mediator",
    EM_interaction = include_em_interaction,
    adjustment = ifelse(
      length(basec_vars) == 0,
      "None",
      paste(basec_vars, collapse = ", ")
    )
  ) %>%
  select(
    outcome,
    exposure_comparison,
    mediator_reference,
    model,
    EM_interaction,
    adjustment,
    effect,
    scale,
    everything()
  )


binary_results_pooled <- pool_cmest_effects(
  binary_effects_by_imputation,
  outcome_type = "binary"
) %>%
  mutate(
    exposure_comparison = "low income (1) vs H4_P1 >=5 (0)",
    mediator_reference = paste0(
      "G3 >= observed median (G3 >= ",
      g3_cutoff,
      ")"
    ),
    model = paste0(
      "Regression-based CMA; logistic outcome and mediator; ",
      "counterfactual imputation"
    ),
    EM_interaction = include_em_interaction,
    adjustment = ifelse(
      length(basec_vars) == 0,
      "None",
      paste(basec_vars, collapse = ", ")
    )
  ) %>%
  select(
    outcome,
    exposure_comparison,
    mediator_reference,
    model,
    EM_interaction,
    adjustment,
    effect,
    scale,
    everything()
  )


# ============================================================
# 15. Analysis information
# ============================================================

analysis_information <- data.frame(
  item = c(
    "Exposure",
    "Exposure reference",
    "Exposure active",
    "Mediator",
    "Mediator reference for CDE",
    "Observed-data G3 median",
    "Continuous outcome",
    "Binary outcome",
    "Exposure-mediator interaction",
    "Adjustment variables",
    "MI method",
    "Number of imputations",
    "MICE iterations",
    "Random forest trees",
    "Binary CMA estimation",
    "Binary CMA bootstrap samples"
  ),
  value = c(
    "low_income",
    "0 = H4_P1 >=5",
    "1 = H4_P1 1-4",
    "G3_below_median",
    "0 = G3 >= observed-data median",
    as.character(g3_cutoff),
    "childscreen24M, hours/day",
    "screen_time_1h: 0 = <1 hour/day; 1 = >=1 hour/day",
    as.character(include_em_interaction),
    ifelse(
      length(basec_vars) == 0,
      "None (unadjusted; causal interpretation not warranted)",
      paste(basec_vars, collapse = ", ")
    ),
    "Random forest via mice",
    as.character(m_imp),
    as.character(mice_maxit),
    as.character(ntree_rf),
    "Direct counterfactual imputation + bootstrap within each MI dataset",
    as.character(nboot_cma)
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 16. Print main results
# ============================================================

cat("\n=====================================\n")
cat("POOLED CMA: CONTINUOUS OUTCOME\n")
cat("=====================================\n")
print(continuous_results_pooled)

cat("\n=====================================\n")
cat("POOLED CMA: BINARY OUTCOME\n")
cat("=====================================\n")
print(binary_results_pooled)


# ============================================================
# 17. Save results to Excel
# ============================================================

wb <- createWorkbook()

addWorksheet(wb, "CMA_continuous_pooled")
writeData(wb, "CMA_continuous_pooled", continuous_results_pooled)

addWorksheet(wb, "CMA_binary_pooled")
writeData(wb, "CMA_binary_pooled", binary_results_pooled)

addWorksheet(wb, "continuous_each_MI")
writeData(
  wb,
  "continuous_each_MI",
  continuous_effects_by_imputation
)

addWorksheet(wb, "binary_each_MI")
writeData(
  wb,
  "binary_each_MI",
  binary_effects_by_imputation
)

addWorksheet(wb, "counts_each_MI")
writeData(wb, "counts_each_MI", count_data)

addWorksheet(wb, "counts_summary")
writeData(wb, "counts_summary", count_summary)

addWorksheet(wb, "missing_summary")
writeData(wb, "missing_summary", missing_summary)

addWorksheet(wb, "analysis_information")
writeData(wb, "analysis_information", analysis_information)

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)

cat("\n=====================================\n")
cat("Analysis completed successfully.\n")
cat("Output file:\n", output_file, "\n")
cat("=====================================\n")