# ============================================================
# Maternal/family factors and WAST-Short-J at three time points
#
# Descriptive statistics before multiple imputation
# Multiple imputation + unadjusted logistic regression
#
# Outcomes
#   WAST-Short-J positive screen at:
#     1) Pregnancy:               PC1 + PC2
#     2) Immediately postpartum: PC3 + PC4
#     3) 24 months:              PC5 + PC6
#
# WAST-Short-J scoring
#   Original responses are retained:
#     1 = a lot of tension / great difficulty
#     2 = some tension / some difficulty
#     3 = no tension / no difficulty
#
#   No reverse scoring is performed.
#
#   Total score:
#     Minimum = 2
#     Maximum = 6
#
#   Lower scores indicate greater IPV risk.
#
#   Positive screen:
#     1 = total score <= 3
#     0 = total score >= 4
#
# Important:
#   PC1-PC6 are NOT imputed separately.
#
#   The two items at each time point are first summed:
#     PC1 + PC2 -> WAST_pregnancy
#     PC3 + PC4 -> WAST_postpartum
#     PC5 + PC6 -> WAST_24m
#
#   The resulting three WAST total scores are then imputed.
#
# Exposures (each entered separately; unadjusted models only)
#
#   1) Maternal age:
#        <25 years vs >=25 years
#
#   2) Household income:
#        H4_P1 1-4 vs >=5
#
#   3) Maternal education:
#        mother_education_6grp
#        0-2; category 0 is reference
#
#   4) Paternal education:
#        father_education_6grp
#        0-2; category 0 is reference
#
#   5) Social support at 1/6/12/18/24 months:
#        E1_1m, E1_6m, E1_12m, E1_18m, E1
#        1-2 = isolated/exposed
#        3-6 = reference
#
#   6) EPDS at 1/6/12/18 months:
#        >=9 = exposed
#        <=8 = reference
#
# Multiple imputation:
#   mice
#   random forest
#   m = 20
#   ntree = 100
#
# Regression:
#   Unadjusted logistic regression
#   Effect measure = Odds Ratio (OR)
#
# Descriptive cross-tabulation:
#   BEFORE multiple imputation, for each exposure category
#   and each WAST time point, report:
#
#     - negative n (%)
#     - positive n (%)
#     - missing n (%)
#
#   Percentages use the number of participants in each
#   exposure category as the denominator.
#
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

library(readxl)
library(mice)
library(dplyr)
library(openxlsx)
library(randomForest)


# ============================================================
# 1. File and analysis settings
# ============================================================

input_file <- "D:/mint/data_xlsx/merged_ipv.xlsx"

output_file <- paste0(
  "D:/mint/results/",
  "WAST_associations_MI_logistic_unadjusted.xlsx"
)

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

set.seed(12345)

m_imp <- 20
ntree_rf <- 100
mice_iterations <- 20


# ============================================================
# 2. Variable settings
# ============================================================

id_var <- "users_id"

age_raw <- "age_corrected"
income_raw <- "H4_P1"
mother_education_raw <- "mother_education_6grp"
father_education_raw <- "father_education_6grp"


support_vars <- c(
  "E1_1m",
  "E1_6m",
  "E1_12m",
  "E1_18m",
  "E1"
)


epds_vars <- c(
  "EPDS_1m",
  "EPDS_6m",
  "EPDS_12m",
  "EPDS_18m"
)


wast_vars <- paste0(
  "PC",
  1:6
)


wast_timepoints <- data.frame(
  
  timepoint = c(
    "pregnancy",
    "postpartum",
    "24m"
  ),
  
  item1 = c(
    "PC1",
    "PC3",
    "PC5"
  ),
  
  item2 = c(
    "PC2",
    "PC4",
    "PC6"
  ),
  
  score_var = c(
    "WAST_pregnancy",
    "WAST_postpartum",
    "WAST_24m"
  ),
  
  outcome = c(
    "WAST_pregnancy_positive",
    "WAST_postpartum_positive",
    "WAST_24m_positive"
  ),
  
  outcome_label = c(
    "WAST-Short-J positive during pregnancy",
    "WAST-Short-J positive immediately postpartum",
    "WAST-Short-J positive at 24 months"
  ),
  
  stringsAsFactors = FALSE
)


required_columns <- c(
  id_var,
  age_raw,
  income_raw,
  mother_education_raw,
  father_education_raw,
  support_vars,
  epds_vars,
  wast_vars
)


# ============================================================
# 3. Helper functions
# ============================================================


# ------------------------------------------------------------
# Convert factor/character to numeric
# ------------------------------------------------------------

factor_to_numeric <- function(x) {
  
  if (is.factor(x)) {
    
    return(
      suppressWarnings(
        as.numeric(
          as.character(x)
        )
      )
    )
  }
  
  suppressWarnings(
    as.numeric(x)
  )
}


# ------------------------------------------------------------
# Create a factor using only levels actually present
# ------------------------------------------------------------

make_observed_factor <- function(
    x,
    preferred_reference = "0"
) {
  
  y <- droplevels(
    factor(
      factor_to_numeric(x)
    )
  )
  
  if (
    preferred_reference %in% levels(y)
  ) {
    
    y <- relevel(
      y,
      ref = preferred_reference
    )
  }
  
  y
}


# ============================================================
# WAST total-score construction
# ============================================================

construct_wast_totals <- function(d) {
  
  
  # ----------------------------------------------------------
  # Validate PC1-PC6
  #
  # Valid:
  #   1, 2, 3
  #
  # No reverse scoring
  # ----------------------------------------------------------
  
  for (v in wast_vars) {
    
    x <- factor_to_numeric(
      d[[v]]
    )
    
    x[
      !is.na(x) &
        !(x %in% 1:3)
    ] <- NA_real_
    
    d[[v]] <- x
  }
  
  
  # ----------------------------------------------------------
  # Calculate total score for each time point
  #
  # If either item is missing, the total is missing.
  # ----------------------------------------------------------
  
  for (
    i in seq_len(
      nrow(wast_timepoints)
    )
  ) {
    
    item1 <- wast_timepoints$item1[i]
    
    item2 <- wast_timepoints$item2[i]
    
    score_var <- wast_timepoints$score_var[i]
    
    
    d[[score_var]] <- ifelse(
      
      is.na(
        d[[item1]]
      ) |
        is.na(
          d[[item2]]
        ),
      
      NA_real_,
      
      d[[item1]] +
        d[[item2]]
    )
  }
  
  d
}


# ============================================================
# Derive variables used in analyses
# ============================================================

derive_analysis_variables <- function(d) {
  
  
  # ==========================================================
  # Maternal age
  #
  # 0 = >=25 years
  # 1 = <25 years
  # ==========================================================
  
  age_num <- factor_to_numeric(
    d[[age_raw]]
  )
  
  age_num[
    !is.na(age_num) &
      age_num < 0
  ] <- NA_real_
  
  d[[age_raw]] <- age_num
  
  
  d$age_under25 <- dplyr::case_when(
    
    is.na(age_num) ~ NA_real_,
    
    age_num < 25 ~ 1,
    
    age_num >= 25 ~ 0,
    
    TRUE ~ NA_real_
  )
  
  
  # ==========================================================
  # Household income
  #
  # 0 = H4_P1 >=5
  # 1 = H4_P1 1-4
  # ==========================================================
  
  income_num <- factor_to_numeric(
    d[[income_raw]]
  )
  
  income_num[
    !is.na(income_num) &
      income_num < 1
  ] <- NA_real_
  
  d[[income_raw]] <- income_num
  
  
  d$low_income <- dplyr::case_when(
    
    is.na(income_num) ~ NA_real_,
    
    income_num >= 1 &
      income_num <= 4 ~ 1,
    
    income_num >= 5 ~ 0,
    
    TRUE ~ NA_real_
  )
  
  
  # ==========================================================
  # Maternal education
  #
  # Categories:
  #   0
  #   1
  #   2
  #
  # Reference:
  #   0
  # ==========================================================
  
  mother_edu_num <- factor_to_numeric(
    d[[mother_education_raw]]
  )
  
  mother_edu_num[
    !is.na(mother_edu_num) &
      !(mother_edu_num %in% 0:2)
  ] <- NA_real_
  
  d[[mother_education_raw]] <-
    mother_edu_num
  
  
  d$mother_education_3cat <-
    make_observed_factor(
      
      mother_edu_num,
      
      preferred_reference = "0"
    )
  
  
  # ==========================================================
  # Paternal education
  #
  # Categories:
  #   0
  #   1
  #   2
  #
  # Reference:
  #   0
  # ==========================================================
  
  father_edu_num <- factor_to_numeric(
    d[[father_education_raw]]
  )
  
  father_edu_num[
    !is.na(father_edu_num) &
      !(father_edu_num %in% 0:2)
  ] <- NA_real_
  
  d[[father_education_raw]] <-
    father_edu_num
  
  
  d$father_education_3cat <-
    make_observed_factor(
      
      father_edu_num,
      
      preferred_reference = "0"
    )
  
  
  # ==========================================================
  # Social support
  #
  # 0 = E1 3-6
  # 1 = E1 1-2
  #
  # E1 without suffix = 24 months
  # ==========================================================
  
  for (v in support_vars) {
    
    x <- factor_to_numeric(
      d[[v]]
    )
    
    x[
      !is.na(x) &
        !(x %in% 1:6)
    ] <- NA_real_
    
    d[[v]] <- x
    
    
    new_name <- paste0(
      v,
      "_isolated"
    )
    
    
    d[[new_name]] <-
      dplyr::case_when(
        
        is.na(x) ~ NA_real_,
        
        x >= 1 &
          x <= 2 ~ 1,
        
        x >= 3 &
          x <= 6 ~ 0,
        
        TRUE ~ NA_real_
      )
  }
  
  
  # ==========================================================
  # EPDS
  #
  # 0 = <=8
  # 1 = >=9
  # ==========================================================
  
  for (v in epds_vars) {
    
    x <- factor_to_numeric(
      d[[v]]
    )
    
    x[
      !is.na(x) &
        x < 0
    ] <- NA_real_
    
    d[[v]] <- x
    
    
    new_name <- paste0(
      v,
      "_9plus"
    )
    
    
    d[[new_name]] <-
      dplyr::case_when(
        
        is.na(x) ~ NA_real_,
        
        x >= 0 &
          x <= 8 ~ 0,
        
        x >= 9 ~ 1,
        
        TRUE ~ NA_real_
      )
  }
  
  
  # ==========================================================
  # WAST-Short-J outcomes
  #
  # Original item scores:
  #   1, 2, 3
  #
  # No reverse scoring
  #
  # Total:
  #   2-6
  #
  # Positive:
  #   <=3 -> 1
  #
  # Negative:
  #   >=4 -> 0
  # ==========================================================
  
  for (
    i in seq_len(
      nrow(wast_timepoints)
    )
  ) {
    
    score_var <-
      wast_timepoints$score_var[i]
    
    outcome_var <-
      wast_timepoints$outcome[i]
    
    
    score_num <- factor_to_numeric(
      d[[score_var]]
    )
    
    
    score_num[
      !is.na(score_num) &
        !(score_num %in% 2:6)
    ] <- NA_real_
    
    
    d[[score_var]] <-
      score_num
    
    
    d[[outcome_var]] <-
      ifelse(
        
        is.na(score_num),
        
        NA_integer_,
        
        ifelse(
          score_num <= 3,
          1L,
          0L
        )
      )
  }
  
  
  d
}


# ============================================================
# Continuous descriptive statistics
# ============================================================

summarize_continuous <- function(
    d,
    variables
) {
  
  bind_rows(
    
    lapply(
      
      variables,
      
      function(v) {
        
        x <- factor_to_numeric(
          d[[v]]
        )
        
        
        n_observed <- sum(
          !is.na(x)
        )
        
        
        data.frame(
          
          variable = v,
          
          n_total = length(x),
          
          n_observed = n_observed,
          
          n_missing = sum(
            is.na(x)
          ),
          
          
          mean = ifelse(
            n_observed > 0,
            mean(
              x,
              na.rm = TRUE
            ),
            NA_real_
          ),
          
          
          sd = ifelse(
            n_observed > 1,
            sd(
              x,
              na.rm = TRUE
            ),
            NA_real_
          ),
          
          
          median = ifelse(
            n_observed > 0,
            median(
              x,
              na.rm = TRUE
            ),
            NA_real_
          ),
          
          
          q1 = ifelse(
            n_observed > 0,
            
            as.numeric(
              quantile(
                x,
                0.25,
                na.rm = TRUE
              )
            ),
            
            NA_real_
          ),
          
          
          q3 = ifelse(
            n_observed > 0,
            
            as.numeric(
              quantile(
                x,
                0.75,
                na.rm = TRUE
              )
            ),
            
            NA_real_
          ),
          
          
          min = ifelse(
            n_observed > 0,
            min(
              x,
              na.rm = TRUE
            ),
            NA_real_
          ),
          
          
          max = ifelse(
            n_observed > 0,
            max(
              x,
              na.rm = TRUE
            ),
            NA_real_
          ),
          
          
          stringsAsFactors = FALSE
        )
      }
    )
    
  ) %>%
    
    mutate(
      
      across(
        
        c(
          mean,
          sd,
          median,
          q1,
          q3,
          min,
          max
        ),
        
        ~ round(
          .x,
          2
        )
      )
    )
}


# ============================================================
# Categorical descriptive statistics
# ============================================================

summarize_categorical <- function(
    d,
    variables
) {
  
  bind_rows(
    
    lapply(
      
      variables,
      
      function(v) {
        
        x <- d[[v]]
        
        x_character <- as.character(
          x
        )
        
        
        observed_categories <- sort(
          
          unique(
            
            x_character[
              !is.na(x_character)
            ]
          )
        )
        
        
        categories <- c(
          observed_categories,
          "Missing"
        )
        
        
        n_observed <- sum(
          !is.na(x)
        )
        
        
        bind_rows(
          
          lapply(
            
            categories,
            
            function(category) {
              
              
              is_missing_category <-
                category == "Missing"
              
              
              n_category <-
                
                if (is_missing_category) {
                  
                  sum(
                    is.na(x)
                  )
                  
                } else {
                  
                  sum(
                    x_character == category,
                    na.rm = TRUE
                  )
                }
              
              
              data.frame(
                
                variable = v,
                
                category = category,
                
                n = n_category,
                
                
                percent_of_total = round(
                  
                  n_category /
                    length(x) *
                    100,
                  
                  1
                ),
                
                
                percent_of_observed = ifelse(
                  
                  is_missing_category |
                    n_observed == 0,
                  
                  NA_real_,
                  
                  round(
                    
                    n_category /
                      n_observed *
                      100,
                    
                    1
                  )
                ),
                
                
                stringsAsFactors = FALSE
              )
            }
          )
        )
      }
    )
  )
}


# ============================================================
# Outcome counts by exposure category BEFORE MI
#
# For every exposure and every WAST time point:
#
#   Exposure category
#   Total n
#   WAST negative n (%)
#   WAST positive n (%)
#   WAST missing n (%)
#
# Percentages use exposure-category n as denominator.
#
# Exposure missing is also shown as a separate category.
# ============================================================

summarize_outcome_by_exposure <- function(
    d,
    exposure_info,
    outcome_info
) {
  
  result_list <- list()
  
  counter <- 1
  
  
  for (
    exposure in exposure_info$exposure
  ) {
    
    
    exposure_label <-
      exposure_info$exposure_label[
        exposure_info$exposure == exposure
      ][1]
    
    
    x <- d[[exposure]]
    
    
    x_char <- ifelse(
      is.na(x),
      "Missing",
      as.character(x)
    )
    
    
    exposure_categories <- sort(
      unique(
        x_char[
          x_char != "Missing"
        ]
      )
    )
    
    
    if (
      any(
        x_char == "Missing"
      )
    ) {
      
      exposure_categories <- c(
        exposure_categories,
        "Missing"
      )
    }
    
    
    for (
      outcome in outcome_info$outcome
    ) {
      
      
      outcome_row <-
        outcome_info[
          outcome_info$outcome == outcome,
          ,
          drop = FALSE
        ]
      
      
      y <- d[[outcome]]
      
      
      for (
        category in exposure_categories
      ) {
        
        
        index <-
          x_char == category
        
        
        total_n <- sum(
          index
        )
        
        
        negative_n <- sum(
          index &
            !is.na(y) &
            y == 0
        )
        
        
        positive_n <- sum(
          index &
            !is.na(y) &
            y == 1
        )
        
        
        missing_n <- sum(
          index &
            is.na(y)
        )
        
        
        negative_percent <-
          ifelse(
            total_n > 0,
            round(
              negative_n /
                total_n *
                100,
              1
            ),
            NA_real_
          )
        
        
        positive_percent <-
          ifelse(
            total_n > 0,
            round(
              positive_n /
                total_n *
                100,
              1
            ),
            NA_real_
          )
        
        
        missing_percent <-
          ifelse(
            total_n > 0,
            round(
              missing_n /
                total_n *
                100,
              1
            ),
            NA_real_
          )
        
        
        result_list[[counter]] <-
          data.frame(
            
            exposure = exposure,
            
            exposure_label =
              exposure_label,
            
            exposure_category =
              category,
            
            outcome = outcome,
            
            outcome_label =
              outcome_row$outcome_label,
            
            timepoint =
              outcome_row$timepoint,
            
            total_n =
              total_n,
            
            negative_n =
              negative_n,
            
            negative_percent =
              negative_percent,
            
            positive_n =
              positive_n,
            
            positive_percent =
              positive_percent,
            
            missing_n =
              missing_n,
            
            missing_percent =
              missing_percent,
            
            negative_n_percent =
              paste0(
                negative_n,
                " (",
                sprintf(
                  "%.1f",
                  negative_percent
                ),
                "%)"
              ),
            
            positive_n_percent =
              paste0(
                positive_n,
                " (",
                sprintf(
                  "%.1f",
                  positive_percent
                ),
                "%)"
              ),
            
            missing_n_percent =
              paste0(
                missing_n,
                " (",
                sprintf(
                  "%.1f",
                  missing_percent
                ),
                "%)"
              ),
            
            stringsAsFactors = FALSE
          )
        
        
        counter <- counter + 1
      }
    }
  }
  
  
  bind_rows(
    result_list
  )
}


# ============================================================
# 4. Read data and check columns
# ============================================================

df_raw <- read_excel(
  input_file
)


names(df_raw) <- trimws(
  names(df_raw)
)


missing_columns <- setdiff(
  required_columns,
  names(df_raw)
)


if (
  length(
    missing_columns
  ) > 0
) {
  
  stop(
    "Missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}


if (
  anyDuplicated(
    df_raw[[id_var]]
  ) > 0
) {
  
  stop(
    "users_id contains duplicates. ",
    "Check whether one row equals one participant."
  )
}


if (
  anyNA(
    df_raw[[id_var]]
  )
) {
  
  stop(
    "users_id contains missing values."
  )
}


cat(
  "Number of participants:",
  nrow(df_raw),
  "\n"
)


# ============================================================
# 5. Descriptive statistics BEFORE multiple imputation
# ============================================================

df_observed <- as.data.frame(
  df_raw
)


# ------------------------------------------------------------
# Calculate WAST total scores
#
# No reverse scoring
# ------------------------------------------------------------

df_observed <-
  construct_wast_totals(
    df_observed
  )


# ------------------------------------------------------------
# Derive exposure and outcome variables
# ------------------------------------------------------------

df_observed <-
  derive_analysis_variables(
    df_observed
  )


# ============================================================
# Exposure definitions
#
# Define these here because they are also used for
# cross-tabulation before MI.
# ============================================================

exposure_info <- data.frame(
  
  exposure = c(
    
    "age_under25",
    
    "low_income",
    
    "mother_education_3cat",
    
    "father_education_3cat",
    
    paste0(
      support_vars,
      "_isolated"
    ),
    
    paste0(
      epds_vars,
      "_9plus"
    )
  ),
  
  
  exposure_label = c(
    
    "Maternal age <25 years",
    
    "Low household income at P1 (H4_P1 = 1-4)",
    
    "Maternal education (3 categories: 0-2)",
    
    "Paternal education (3 categories: 0-2)",
    
    "Social isolation at 1 month (E1_1m = 1-2)",
    
    "Social isolation at 6 months (E1_6m = 1-2)",
    
    "Social isolation at 12 months (E1_12m = 1-2)",
    
    "Social isolation at 18 months (E1_18m = 1-2)",
    
    "Social isolation at 24 months (E1 = 1-2)",
    
    "EPDS >=9 at 1 month",
    
    "EPDS >=9 at 6 months",
    
    "EPDS >=9 at 12 months",
    
    "EPDS >=9 at 18 months"
  ),
  
  
  exposure_type = c(
    
    "binary",
    
    "binary",
    
    "categorical_3_levels",
    
    "categorical_3_levels",
    
    rep(
      "binary",
      9
    )
  ),
  
  
  reference_group = c(
    
    "age_corrected >=25 years",
    
    "H4_P1 >=5",
    
    "mother_education_6grp = 0",
    
    "father_education_6grp = 0",
    
    rep(
      "E1 = 3-6",
      5
    ),
    
    rep(
      "EPDS <=8",
      4
    )
  ),
  
  
  exposed_or_comparison_group = c(
    
    "age_corrected <25 years",
    
    "H4_P1 = 1-4",
    
    "Categories 1 and 2, each compared with category 0",
    
    "Categories 1 and 2, each compared with category 0",
    
    rep(
      "E1 = 1-2",
      5
    ),
    
    rep(
      "EPDS >=9",
      4
    )
  ),
  
  
  stringsAsFactors = FALSE
)


# ============================================================
# Outcome definitions
# ============================================================

outcome_info <-
  wast_timepoints %>%
  
  transmute(
    
    timepoint = timepoint,
    
    outcome = outcome,
    
    outcome_label = outcome_label,
    
    negative_group =
      "WAST-Short-J total score >=4",
    
    positive_group =
      "WAST-Short-J total score <=3"
  )


exposure_vars <-
  exposure_info$exposure


outcome_vars <-
  outcome_info$outcome


# ============================================================
# Variables for descriptive statistics
# ============================================================

raw_descriptive_vars <- c(
  
  age_raw,
  
  income_raw,
  
  mother_education_raw,
  
  father_education_raw,
  
  support_vars,
  
  epds_vars,
  
  wast_vars
)


descriptive_vars <- c(
  
  raw_descriptive_vars,
  
  wast_timepoints$score_var
)


# ============================================================
# Missing values BEFORE MI
# ============================================================

missing_before_MI <- data.frame(
  
  variable = descriptive_vars,
  
  
  n_missing = sapply(
    
    descriptive_vars,
    
    function(v) {
      
      sum(
        is.na(
          df_observed[[v]]
        )
      )
    }
  ),
  
  
  percent_missing = round(
    
    sapply(
      
      descriptive_vars,
      
      function(v) {
        
        mean(
          is.na(
            df_observed[[v]]
          )
        ) * 100
      }
    ),
    
    1
  ),
  
  
  stringsAsFactors = FALSE
)


# ============================================================
# Continuous descriptive statistics BEFORE MI
# ============================================================

continuous_before_MI <-
  summarize_continuous(
    
    df_observed,
    
    c(
      age_raw,
      epds_vars,
      wast_timepoints$score_var
    )
  )


# ============================================================
# Derived binary variables
# ============================================================

derived_binary_vars <- c(
  
  "age_under25",
  
  "low_income",
  
  paste0(
    support_vars,
    "_isolated"
  ),
  
  paste0(
    epds_vars,
    "_9plus"
  ),
  
  wast_timepoints$outcome
)


# ============================================================
# Categorical descriptive statistics BEFORE MI
# ============================================================

categorical_before_MI <-
  summarize_categorical(
    
    df_observed,
    
    c(
      
      income_raw,
      
      mother_education_raw,
      
      father_education_raw,
      
      support_vars,
      
      wast_vars,
      
      derived_binary_vars
    )
  )


# ============================================================
# Outcome status by each exposure BEFORE MI
#
# This is the requested table showing:
#
#   negative
#   positive
#   missing
#
# for each exposure category.
# ============================================================

outcome_by_exposure_before_MI <-
  summarize_outcome_by_exposure(
    
    df_observed,
    
    exposure_info,
    
    outcome_info
  )


cat(
  "\nContinuous descriptive statistics before MI:\n"
)

print(
  continuous_before_MI
)


cat(
  "\nCategorical descriptive statistics before MI:\n"
)

print(
  categorical_before_MI
)


cat(
  "\nMissing-value summary before MI:\n"
)

print(
  missing_before_MI
)


cat(
  "\nOutcome status by exposure before MI:\n"
)

print(
  outcome_by_exposure_before_MI
)


# ============================================================
# 6. Dataset for multiple imputation
# ============================================================

# IMPORTANT:
#
# PC1-PC6 are NOT included separately.
#
# Instead:
#
#   PC1 + PC2 -> WAST_pregnancy
#   PC3 + PC4 -> WAST_postpartum
#   PC5 + PC6 -> WAST_24m
#
# These three total scores are entered into MICE.
#
# Binary WAST outcomes are derived after MI.
# ============================================================

imputation_vars <- c(
  
  age_raw,
  
  income_raw,
  
  mother_education_raw,
  
  father_education_raw,
  
  support_vars,
  
  epds_vars,
  
  wast_timepoints$score_var
)


analysis_columns <- c(
  id_var,
  imputation_vars
)


dat <- df_observed[
  ,
  analysis_columns,
  drop = FALSE
]


non_id_vars <- setdiff(
  names(dat),
  id_var
)


# ------------------------------------------------------------
# Remove rows in which all analysis variables are missing
# ------------------------------------------------------------

dat <- dat[
  
  rowSums(
    
    !is.na(
      
      dat[
        ,
        non_id_vars,
        drop = FALSE
      ]
    )
    
  ) > 0,
  
  ,
  
  drop = FALSE
]


cat(
  "Number of participants included in MI:",
  nrow(dat),
  "\n"
)


# ============================================================
# 7. Variable types for multiple imputation
# ============================================================


# ------------------------------------------------------------
# Maternal age: numeric
# ------------------------------------------------------------

dat[[age_raw]] <-
  factor_to_numeric(
    dat[[age_raw]]
  )


# ------------------------------------------------------------
# EPDS: numeric
# ------------------------------------------------------------

for (v in epds_vars) {
  
  dat[[v]] <-
    factor_to_numeric(
      dat[[v]]
    )
}


# ------------------------------------------------------------
# Household income: factor
# ------------------------------------------------------------

dat[[income_raw]] <-
  factor(
    dat[[income_raw]]
  )


# ------------------------------------------------------------
# Maternal education: factor
# ------------------------------------------------------------

dat[[mother_education_raw]] <-
  make_observed_factor(
    
    dat[[mother_education_raw]],
    
    preferred_reference = "0"
  )


# ------------------------------------------------------------
# Paternal education: factor
# ------------------------------------------------------------

dat[[father_education_raw]] <-
  make_observed_factor(
    
    dat[[father_education_raw]],
    
    preferred_reference = "0"
  )


# ------------------------------------------------------------
# Social support: factor
# ------------------------------------------------------------

for (v in support_vars) {
  
  dat[[v]] <-
    make_observed_factor(
      
      dat[[v]],
      
      preferred_reference = "1"
    )
}


# ------------------------------------------------------------
# WAST total scores
#
# Possible:
#   2
#   3
#   4
#   5
#   6
#
# Treat as categorical variables in random-forest MI.
# ------------------------------------------------------------

for (
  v in wast_timepoints$score_var
) {
  
  dat[[v]] <-
    make_observed_factor(
      
      dat[[v]],
      
      preferred_reference = "2"
    )
}


# ============================================================
# 8. Multiple imputation using random forest
# ============================================================


# ------------------------------------------------------------
# Check completely missing variables
# ------------------------------------------------------------

all_missing_vars <-
  names(dat)[
    
    sapply(
      
      dat,
      
      function(x) {
        
        all(
          is.na(x)
        )
      }
    )
  ]


all_missing_vars <- setdiff(
  all_missing_vars,
  id_var
)


if (
  length(
    all_missing_vars
  ) > 0
) {
  
  stop(
    
    "The following variables are completely missing ",
    "and cannot be imputed: ",
    
    paste(
      all_missing_vars,
      collapse = ", "
    )
  )
}


# ------------------------------------------------------------
# Initialize MICE
# ------------------------------------------------------------

ini <- mice(
  
  dat,
  
  maxit = 0,
  
  printFlag = FALSE
)


meth <- ini$method

pred <- ini$predictorMatrix


# ------------------------------------------------------------
# Use random forest for variables with missing values
# ------------------------------------------------------------

meth[] <- ""


vars_with_missing <-
  names(dat)[
    
    colSums(
      is.na(dat)
    ) > 0
  ]


vars_with_missing <-
  setdiff(
    vars_with_missing,
    id_var
  )


meth[
  vars_with_missing
] <- "rf"


# ------------------------------------------------------------
# ID is not imputed and not used as a predictor
# ------------------------------------------------------------

meth[id_var] <- ""


pred[, id_var] <- 0

pred[id_var, ] <- 0

diag(pred) <- 0


cat(
  "\nVariables imputed by random forest:\n"
)


print(
  names(meth)[
    meth == "rf"
  ]
)


# ------------------------------------------------------------
# Run MICE
# ------------------------------------------------------------

imp <- mice(
  
  dat,
  
  m = m_imp,
  
  maxit = mice_iterations,
  
  method = meth,
  
  predictorMatrix = pred,
  
  ntree = ntree_rf,
  
  seed = 12345,
  
  printFlag = TRUE
)


# ------------------------------------------------------------
# Extract completed datasets
# ------------------------------------------------------------

completed_list <- complete(
  imp,
  action = "all"
)


# ============================================================
# 9. Model combinations
# ============================================================

model_keys <- expand.grid(
  
  exposure = exposure_vars,
  
  outcome = outcome_vars,
  
  stringsAsFactors = FALSE
)


model_lists <- setNames(
  
  vector(
    "list",
    nrow(model_keys)
  ),
  
  paste(
    model_keys$outcome,
    model_keys$exposure,
    sep = "__"
  )
)


exposure_count_list <- vector(
  "list",
  length(completed_list)
)


outcome_count_list <- vector(
  "list",
  length(completed_list)
)


# ============================================================
# 10. Fit unadjusted logistic regression
#     in each imputed dataset
# ============================================================

for (
  i in seq_along(
    completed_list
  )
) {
  
  
  # ----------------------------------------------------------
  # Derive variables after MI
  # ----------------------------------------------------------
  
  d <-
    derive_analysis_variables(
      completed_list[[i]]
    )
  
  
  # ----------------------------------------------------------
  # Remove unused education levels
  # ----------------------------------------------------------
  
  d$mother_education_3cat <-
    make_observed_factor(
      
      d$mother_education_3cat,
      
      preferred_reference = "0"
    )
  
  
  d$father_education_3cat <-
    make_observed_factor(
      
      d$father_education_3cat,
      
      preferred_reference = "0"
    )
  
  
  # ==========================================================
  # Run every exposure-outcome model
  # ==========================================================
  
  for (
    j in seq_len(
      nrow(model_keys)
    )
  ) {
    
    
    exposure <-
      model_keys$exposure[j]
    
    
    outcome <-
      model_keys$outcome[j]
    
    
    key <- paste(
      outcome,
      exposure,
      sep = "__"
    )
    
    
    # --------------------------------------------------------
    # Check exposure variation
    # --------------------------------------------------------
    
    if (
      
      length(
        
        unique(
          
          d[[exposure]][
            
            !is.na(
              d[[exposure]]
            )
          ]
        )
        
      ) < 2
    ) {
      
      stop(
        
        "Exposure has fewer than two observed levels ",
        "in imputation ",
        i,
        ": ",
        exposure
      )
    }
    
    
    # --------------------------------------------------------
    # Check outcome variation
    # --------------------------------------------------------
    
    if (
      
      length(
        
        unique(
          
          d[[outcome]][
            
            !is.na(
              d[[outcome]]
            )
          ]
        )
        
      ) < 2
    ) {
      
      stop(
        
        "Outcome has fewer than two observed levels ",
        "in imputation ",
        i,
        ": ",
        outcome
      )
    }
    
    
    # --------------------------------------------------------
    # Model formula
    # --------------------------------------------------------
    
    model_formula <-
      reformulate(
        
        exposure,
        
        response = outcome
      )
    
    
    # --------------------------------------------------------
    # Unadjusted logistic regression
    #
    # Outcome:
    #   0 = WAST negative
    #   1 = WAST positive
    #
    # Therefore:
    #   OR > 1 indicates higher odds of WAST positivity.
    # --------------------------------------------------------
    
    model_lists[[key]][[i]] <-
      glm(
        
        formula = model_formula,
        
        data = d,
        
        family = binomial(
          link = "logit"
        )
      )
  }
  
  
  # ==========================================================
  # Exposure counts after MI
  # ==========================================================
  
  exposure_count_list[[i]] <-
    
    bind_rows(
      
      lapply(
        
        exposure_vars,
        
        function(v) {
          
          
          x <- d[[v]]
          
          
          categories <- sort(
            
            unique(
              
              as.character(
                
                x[
                  !is.na(x)
                ]
              )
            )
          )
          
          
          data.frame(
            
            imputation = i,
            
            exposure = v,
            
            category = categories,
            
            n = sapply(
              
              categories,
              
              function(category) {
                
                sum(
                  as.character(x) ==
                    category,
                  na.rm = TRUE
                )
              }
            ),
            
            stringsAsFactors = FALSE
          )
        }
      )
    )
  
  
  # ==========================================================
  # Outcome counts after MI
  # ==========================================================
  
  outcome_count_list[[i]] <-
    
    bind_rows(
      
      lapply(
        
        outcome_vars,
        
        function(v) {
          
          data.frame(
            
            imputation = i,
            
            outcome = v,
            
            group = c(
              "negative_0",
              "positive_1"
            ),
            
            n = c(
              
              sum(
                d[[v]] == 0,
                na.rm = TRUE
              ),
              
              sum(
                d[[v]] == 1,
                na.rm = TRUE
              )
            ),
            
            stringsAsFactors = FALSE
          )
        }
      )
    )
}


# ============================================================
# 11. Pool logistic regression results
#
# Rubin's rules are used across the 20 imputed datasets.
#
# Exponentiated coefficients = Odds Ratios
# ============================================================

logistic_results <-
  
  bind_rows(
    
    lapply(
      
      seq_len(
        nrow(model_keys)
      ),
      
      function(j) {
        
        
        exposure <-
          model_keys$exposure[j]
        
        
        outcome <-
          model_keys$outcome[j]
        
        
        key <- paste(
          outcome,
          exposure,
          sep = "__"
        )
        
        
        # ----------------------------------------------------
        # Pool estimates
        # ----------------------------------------------------
        
        pooled <-
          pool(
            
            as.mira(
              model_lists[[key]]
            )
          )
        
        
        # ----------------------------------------------------
        # Exponentiate logistic coefficients
        #
        # Gives Odds Ratios and 95% CIs.
        # ----------------------------------------------------
        
        result <-
          summary(
            
            pooled,
            
            conf.int = TRUE,
            
            exponentiate = TRUE
            
          ) %>%
          
          as.data.frame() %>%
          
          filter(
            term != "(Intercept)"
          )
        
        
        exposure_row <-
          exposure_info[
            
            exposure_info$exposure ==
              exposure,
            
            ,
            
            drop = FALSE
          ]
        
        
        outcome_row <-
          outcome_info[
            
            outcome_info$outcome ==
              outcome,
            
            ,
            
            drop = FALSE
          ]
        
        
        result %>%
          
          mutate(
            
            outcome = outcome,
            
            outcome_label =
              outcome_row$outcome_label,
            
            timepoint =
              outcome_row$timepoint,
            
            exposure = exposure,
            
            exposure_label =
              exposure_row$exposure_label,
            
            exposure_type =
              exposure_row$exposure_type,
            
            reference_group =
              exposure_row$reference_group,
            
            comparison_group =
              exposure_row$
              exposed_or_comparison_group,
            
            model =
              "Unadjusted logistic regression",
            
            effect_measure =
              "Odds Ratio (OR)"
          )
      }
    )
  ) %>%
  
  
  select(
    
    outcome,
    
    outcome_label,
    
    timepoint,
    
    exposure,
    
    exposure_label,
    
    exposure_type,
    
    reference_group,
    
    comparison_group,
    
    model,
    
    effect_measure,
    
    term,
    
    everything()
  ) %>%
  
  
  mutate(
    
    across(
      
      where(
        is.numeric
      ),
      
      ~ round(
        .x,
        4
      )
    )
  )


# ============================================================
# 12. Summarize counts across imputations
# ============================================================


# ------------------------------------------------------------
# Exposure counts
# ------------------------------------------------------------

exposure_counts_MI <-
  
  bind_rows(
    exposure_count_list
  ) %>%
  
  group_by(
    exposure,
    category
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
  )


# ------------------------------------------------------------
# Outcome counts
# ------------------------------------------------------------

outcome_counts_MI <-
  
  bind_rows(
    outcome_count_list
  ) %>%
  
  group_by(
    outcome,
    group
  ) %>%
  
  summarise(
    
    mean_n = mean(n),
    
    min_n = min(n),
    
    max_n = max(n),
    
    .groups = "drop"
  ) %>%
  
  left_join(
    outcome_info,
    by = "outcome"
  )


# ============================================================
# 13. Model formulas
# ============================================================

model_formulas <-
  
  model_keys %>%
  
  mutate(
    
    formula = paste(
      outcome,
      "~",
      exposure
    ),
    
    adjusted_for = "None"
  ) %>%
  
  left_join(
    
    exposure_info %>%
      
      select(
        exposure,
        exposure_label
      ),
    
    by = "exposure"
  ) %>%
  
  left_join(
    
    outcome_info %>%
      
      select(
        outcome,
        outcome_label,
        timepoint
      ),
    
    by = "outcome"
  ) %>%
  
  select(
    
    outcome,
    
    outcome_label,
    
    timepoint,
    
    exposure,
    
    exposure_label,
    
    formula,
    
    adjusted_for
  )


# ============================================================
# 14. Analysis information
# ============================================================

model_information <- data.frame(
  
  item = c(
    
    "Outcome",
    
    "Outcome coding",
    
    "WAST-Short-J time points",
    
    "WAST-Short-J item scoring",
    
    "WAST-Short-J reverse scoring",
    
    "WAST-Short-J total-score range",
    
    "WAST-Short-J positive cut-off",
    
    "Direction of WAST score",
    
    "Model",
    
    "Effect measure",
    
    "Interpretation of OR >1",
    
    "Adjustment",
    
    "Number of exposure-outcome models",
    
    "Imputation method",
    
    "Number of imputations",
    
    "Random forest trees",
    
    "MICE iterations",
    
    "Variables imputed",
    
    "WAST variables entered into MI",
    
    "WAST item-level MI",
    
    "Derivation after imputation",
    
    "Descriptive-statistics dataset",
    
    "Outcome-by-exposure counts",
    
    "Denominator for outcome percentages"
  ),
  
  
  value = c(
    
    "WAST-Short-J positive screen",
    
    "0 = negative; 1 = positive",
    
    "Pregnancy, immediately postpartum, and 24 months",
    
    "Original scores 1, 2, and 3 are retained",
    
    "No reverse scoring",
    
    "2-6",
    
    "Total score <=3",
    
    "Lower scores indicate greater IPV risk",
    
    "Unadjusted logistic regression",
    
    "Odds Ratio (OR)",
    
    "Higher odds of WAST-Short-J positive screening",
    
    "None; each exposure was analyzed separately",
    
    as.character(
      nrow(model_keys)
    ),
    
    "Random forest via mice",
    
    as.character(
      m_imp
    ),
    
    as.character(
      ntree_rf
    ),
    
    as.character(
      mice_iterations
    ),
    
    paste0(
      "Raw exposure variables and three ",
      "pre-calculated WAST total scores"
    ),
    
    paste(
      wast_timepoints$score_var,
      collapse = ", "
    ),
    
    "PC1-PC6 were not imputed separately",
    
    paste0(
      "Binary exposures and WAST-positive outcomes ",
      "were derived separately in each imputed dataset"
    ),
    
    "Observed data before multiple imputation",
    
    paste0(
      "For each exposure category and WAST time point, ",
      "negative, positive, and missing outcome counts ",
      "were calculated before MI"
    ),
    
    "Total number of participants in each exposure category"
  ),
  
  
  stringsAsFactors = FALSE
)


# ============================================================
# 15. Print main results
# ============================================================

cat(
  "\n=====================================\n"
)

cat(
  "UNADJUSTED LOGISTIC REGRESSION\n"
)

cat(
  "=====================================\n"
)


print(
  logistic_results
)


cat(
  "\n=====================================\n"
)

cat(
  "OUTCOME COUNTS BY EXPOSURE BEFORE MI\n"
)

cat(
  "=====================================\n"
)


print(
  outcome_by_exposure_before_MI
)


# ============================================================
# 16. Save results to Excel
# ============================================================

wb <- createWorkbook()


# ------------------------------------------------------------
# Function for writing Excel sheets
# ------------------------------------------------------------

write_result_sheet <- function(
    wb,
    sheet_name,
    object
) {
  
  addWorksheet(
    wb,
    sheet_name
  )
  
  
  writeData(
    wb,
    sheet_name,
    object,
    withFilter = TRUE
  )
  
  
  freezePane(
    wb,
    sheet_name,
    firstRow = TRUE
  )
  
  
  setColWidths(
    wb,
    sheet_name,
    cols = seq_len(
      ncol(object)
    ),
    widths = "auto"
  )
}


# ============================================================
# Regression results
# ============================================================

write_result_sheet(
  wb,
  "OR_WAST_positive",
  logistic_results
)


# ============================================================
# Requested descriptive cross-tabulation
#
# For every exposure category:
#
#   negative n (%)
#   positive n (%)
#   missing n (%)
#
# before MI
# ============================================================

write_result_sheet(
  wb,
  "outcome_by_exp_before_MI",
  outcome_by_exposure_before_MI
)


# ============================================================
# Other descriptive statistics
# ============================================================

write_result_sheet(
  wb,
  "continuous_before_MI",
  continuous_before_MI
)


write_result_sheet(
  wb,
  "categorical_before_MI",
  categorical_before_MI
)


write_result_sheet(
  wb,
  "missing_before_MI",
  missing_before_MI
)


# ============================================================
# Variable definitions
# ============================================================

write_result_sheet(
  wb,
  "exposure_definitions",
  exposure_info
)


write_result_sheet(
  wb,
  "outcome_definitions",
  outcome_info
)


# ============================================================
# Counts after MI
# ============================================================

write_result_sheet(
  wb,
  "exposure_counts_MI",
  exposure_counts_MI
)


write_result_sheet(
  wb,
  "outcome_counts_MI",
  outcome_counts_MI
)


# ============================================================
# Model information
# ============================================================

write_result_sheet(
  wb,
  "model_formulas",
  model_formulas
)


write_result_sheet(
  wb,
  "model_information",
  model_information
)


# ============================================================
# Save workbook
# ============================================================

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