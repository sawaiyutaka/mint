library(geepack)
library(VGAM)
library(CMAverse)
library(dplyr)
options(digits=22)
options(scipen=100)
df <- read.csv("D:/mint/data_xlsx/merged_selected.csv", fileEncoding = "UTF-8")

# =========================
# 2. Create low_income_baseline
# =========================
# Same coding as the Python code:
# H4_P1 1-4   -> 2
# H4_P1 5-8   -> 1
# H4_P1 9-15  -> 0
#
# Interpretation:
# 2 = lower baseline household income
# 1 = middle baseline household income
# 0 = higher baseline household income

if (!"H4_P1" %in% names(df)) {
  stop("H4_P1 is not found in the dataset.")
}

df <- df %>%
  mutate(
    H4_P1 = as.numeric(H4_P1),
    low_income_baseline = case_when(
      H4_P1 >= 1  & H4_P1 <= 4  ~ 2,
      H4_P1 >= 5  & H4_P1 <= 8  ~ 1,
      H4_P1 >= 9  & H4_P1 <= 15 ~ 0,
      TRUE ~ NA_real_
    ),
    low_income_baseline = factor(low_income_baseline),
    mother_education_6grp = factor(mother_education_6grp)
  )

cat("Distribution of low_income_baseline:\n")
print(table(df$low_income_baseline, useNA = "ifany"))

cat("Distribution of mother_education_6grp:\n")
print(table(df$mother_education_6grp, useNA = "ifany"))

# =========================
# 3. Original variable names
# =========================

original_A_var <- "A13_P1"  # original maternal age during pregnancy
original_M_var <- "G3_18m"  # original financial ease at 18 months
original_Y_var <- "AF3"     # original screen time at 24 months
original_C_fin <- "G3_P1"   # original financial ease during pregnancy

required_vars <- c(
  original_A_var,
  original_M_var,
  original_Y_var,
  original_C_fin,
  "mother_education_6grp",
  "low_income_baseline"
)

missing_vars <- setdiff(required_vars, names(df))

if (length(missing_vars) > 0) {
  stop(paste("These variables are missing:", paste(missing_vars, collapse = ", ")))
}

# =========================
# 4. Prepare analytic dataset
# =========================
# New analysis variables:
# younger_maternal_age:
#   larger value = younger maternal age
#
# financial_difficulty_18m:
#   larger value = less financial ease / greater financial difficulty at 18 months
#
# financial_difficulty_pregnancy:
#   larger value = less financial ease / greater financial difficulty during pregnancy
#
# screen_time_24m:
#   larger value = longer screen time

dat <- df %>%
  select(all_of(required_vars)) %>%
  mutate(
    A13_P1 = as.numeric(A13_P1),
    G3_18m = as.numeric(G3_18m),
    AF3 = as.numeric(AF3),
    G3_P1 = as.numeric(G3_P1),
    mother_education_6grp = factor(mother_education_6grp),
    low_income_baseline = factor(
      low_income_baseline,
      levels = c(0, 1, 2)
    )
  ) %>%
  na.omit() %>%
  mutate(
    younger_maternal_age = 0 - A13_P1,
    financial_difficulty_18m = 0 - G3_18m,
    financial_difficulty_pregnancy = 0 - G3_P1,
    screen_time_24m = AF3
  )

cat("Complete case N:", nrow(dat), "\n")


# ============================================================
# Analysis 1: younger maternal age
# ============================================================

age_base <- as.numeric(median(dat$younger_maternal_age, na.rm = TRUE))
age_change <- age_base + 1
medianm <- as.numeric(median(dat$financial_difficulty_18m, na.rm = TRUE))

set.seed(11111)

re_age <- cmest(
  data = dat,
  model = "rb",
  outcome = "screen_time_24m", 
  exposure = "younger_maternal_age",
  mediator = c("financial_difficulty_18m"), 
  basec = c(
    "financial_difficulty_pregnancy",
    "low_income_baseline"
  ),
  EMint = TRUE,
  mreg = list("linear"), 
  yreg = "linear",
  astar = age_base,
  a = age_change,
  mval = list(medianm), 
  estimation = "para",
  inference = "delta"
)

summary(re_age)

# ============================================================
# Analysis 2: maternal education
# ============================================================

levels(dat$mother_education_6grp)
table(dat$mother_education_6grp, useNA = "ifany")

base <- "0"      # ここは実際のカテゴリに合わせて変更
change <- "2"    # ここは実際のカテゴリに合わせて変更
medianm <- median(dat$financial_difficulty_18m, na.rm = TRUE)

set.seed(11111)

re_edu <- cmest(
  data = dat,
  model = "rb",
  outcome = "screen_time_24m", 
  exposure = "mother_education_6grp",
  mediator = c("financial_difficulty_18m"), 
  basec = c(
    "financial_difficulty_pregnancy",
    "younger_maternal_age"
  ),
  EMint = TRUE,
  mreg = list("linear"), 
  yreg = "linear",
  astar = base,
  a = change,
  mval = list(medianm), 
  estimation = "para",
  inference = "delta"
)

summary(re_edu)

# ============================================================
# Analysis 3: low income baseline
# ============================================================

levels(dat$low_income_baseline)
table(dat$low_income_baseline, useNA = "ifany")

base <- "0"      # higher income
change <- "2"    # lower income
medianm <- median(dat$financial_difficulty_18m, na.rm = TRUE)

set.seed(11111)

re_income <- cmest(
  data = dat,
  model = "rb",
  outcome = "screen_time_24m", 
  exposure = "low_income_baseline",
  mediator = c("financial_difficulty_18m"), 
  basec = c(
    "financial_difficulty_pregnancy",
    "younger_maternal_age"
  ),
  EMint = TRUE,
  mreg = list("linear"), 
  yreg = "linear",
  astar = base,
  a = change,
  mval = list(medianm), 
  estimation = "para",
  inference = "delta"
)

summary(re_income)

# ============================================================
# 6. Export cmest results to CSV
# ============================================================

extract_cmest_results <- function(cmest_object, analysis_name, exposure_label, astar_label, a_label) {
  
  s <- summary(cmest_object)
  
  out <- as.data.frame(s$summarydf)
  
  out$effect <- rownames(out)
  rownames(out) <- NULL
  
  out <- out %>%
    mutate(
      analysis = analysis_name,
      exposure = exposure_label,
      astar = astar_label,
      a = a_label
    ) %>%
    select(
      analysis,
      exposure,
      astar,
      a,
      effect,
      Estimate,
      Std.error,
      `95% CIL`,
      `95% CIU`,
      P.val
    )
  
  return(out)
}

# Analysis 1: maternal age
res_age <- extract_cmest_results(
  cmest_object = re_age,
  analysis_name = "Analysis 1: Maternal age",
  exposure_label = "younger_maternal_age",
  astar_label = "median maternal age",
  a_label = "1 year younger than median maternal age"
)

# Analysis 2: maternal education
res_edu <- extract_cmest_results(
  cmest_object = re_edu,
  analysis_name = "Analysis 2: Maternal education",
  exposure_label = "mother_education_6grp",
  astar_label = "0",
  a_label = "2"
)

# Analysis 3: baseline household income
res_income <- extract_cmest_results(
  cmest_object = re_income,
  analysis_name = "Analysis 3: Baseline household income",
  exposure_label = "low_income_baseline",
  astar_label = "0, higher income",
  a_label = "2, lower income"
)

# Combine all results
cmest_results_all <- bind_rows(
  res_age,
  res_edu,
  res_income
)

# Show results in R console
print(cmest_results_all)

# Export to CSV
write.csv(
  cmest_results_all,
  file = "D:/mint/results/cmest_results_summary.csv",
  row.names = FALSE,
  fileEncoding = "CP932"
)
