# ============================================================
# Correlation between ASQ3_communication and 2gobun
#
# ASQ3_communication:
#   Continuous score (0-60; higher = better communication)
#
# 2gobun:
#   C4-4 = 1 -> 0
#   C4-4 = 2 -> 1
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Check and convert C4-4 to numeric
# ------------------------------------------------------------

if (!"C4-4" %in% names(df)) {
  stop("C4-4 is not found in the dataset.")
}

df[["C4-4"]] <-
  suppressWarnings(
    as.numeric(df[["C4-4"]])
  )


# ------------------------------------------------------------
# 2. Create 2gobun
# ------------------------------------------------------------

df[["2gobun"]] <- case_when(
  df[["C4-4"]] == 1 ~ 0,
  df[["C4-4"]] == 2 ~ 1,
  TRUE ~ NA_real_
)


# ------------------------------------------------------------
# 3. Prepare complete-case data
# ------------------------------------------------------------

correlation_data <- df %>%
  select(
    ASQ3_communication,
    `2gobun`
  ) %>%
  filter(
    complete.cases(.)
  )


cat("\n=====================================\n")
cat("CORRELATION ANALYSIS\n")
cat("=====================================\n")

cat(
  "Number of complete cases:",
  nrow(correlation_data),
  "\n"
)


# ------------------------------------------------------------
# 4. Point-biserial correlation
#
# Pearson correlation between a continuous variable and
# a binary variable is equivalent to point-biserial correlation.
# ------------------------------------------------------------

correlation_test <- cor.test(
  correlation_data$ASQ3_communication,
  correlation_data[["2gobun"]],
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

print(correlation_test)


# ------------------------------------------------------------
# 5. Create results table
# ------------------------------------------------------------

correlation_result <- data.frame(
  Variable_1 = "ASQ3_communication",
  Variable_2 = "2gobun",
  N = nrow(correlation_data),
  Correlation = unname(correlation_test$estimate),
  CI_lower = correlation_test$conf.int[1],
  CI_upper = correlation_test$conf.int[2],
  P_value = correlation_test$p.value
)

correlation_result <- correlation_result %>%
  mutate(
    across(
      c(Correlation, CI_lower, CI_upper),
      ~ round(.x, 3)
    ),
    P_value = signif(P_value, 3)
  )

print(correlation_result)