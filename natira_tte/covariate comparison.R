library(dplyr)
library(tableone)
library(survey)

data <- read.csv("imputed_data.csv", stringsAsFactors = FALSE)
data$group_num <- as.numeric(as.character(data$group))
if (!all(na.omit(unique(data$group_num)) %in% c(0, 1))) stop("group must be coded 0/1.")
data$group <- factor(data$group_num, levels = c(0, 1), labels = c("No support", "Prenatal home visiting support"))
factor_vars <- c("education", "work4day", "income", "nomarry", "alone", "satogaeri")
data[factor_vars] <- lapply(data[factor_vars], factor)
baseline_vars <- c("age", "week", "education", "work4day", "income", "nomarry", "alone", "satogaeri", "support", "yutoriT1")
continuous_vars <- c("age", "week", "support", "yutoriT1")

group_N <- table(data$group)
print(group_N)
N_control <- as.integer(group_N["No support"])
N_support <- as.integer(group_N["Prenatal home visiting support"])
cat("\nActual group N:\n", "No support =", N_control, "\n", "Support    =", N_support, "\n")

iptw_num_model <- glm(group_num ~ 1, family = binomial(link = "logit"), data = data)
iptw_den_model <- glm(group_num ~ age + week + education + work4day + income + nomarry + alone + satogaeri + support + yutoriT1, family = binomial(link = "logit"), data = data)
data$pA_num <- predict(iptw_num_model, newdata = data, type = "response")
data$pA_den <- predict(iptw_den_model, newdata = data, type = "response")
data$sw_iptw <- ifelse(data$group_num == 1, data$pA_num / data$pA_den, (1 - data$pA_num) / (1 - data$pA_den))
cat("\nIPTW summary\n")
print(summary(data$sw_iptw))
cat("\nSum of IPTW by group:\n")
print(tapply(data$sw_iptw, data$group, sum))

design_iptw <- svydesign(ids = ~1, weights = ~sw_iptw, data = data)
table_before <- CreateTableOne(vars = baseline_vars, strata = "group", data = data, factorVars = factor_vars, test = FALSE, smd = TRUE)
table_after <- svyCreateTableOne(vars = baseline_vars, strata = "group", data = design_iptw, factorVars = factor_vars, test = FALSE, smd = TRUE)
smd_before <- ExtractSmd(table_before)
smd_after <- ExtractSmd(table_after)

integer_counts_from_prop <- function(prop, N) {
  prop[prop < 0] <- 0
  if (sum(prop) == 0) return(rep(0L, length(prop)))
  prop <- prop / sum(prop)
  raw_count <- prop * N
  base_count <- floor(raw_count)
  remainder <- N - sum(base_count)
  if (remainder > 0) {
    fractional_part <- raw_count - base_count
    add_to <- order(fractional_part, decreasing = TRUE)[seq_len(remainder)]
    base_count[add_to] <- base_count[add_to] + 1
  }
  as.integer(base_count)
}

format_n_pct <- function(n, pct) sprintf("%d (%.1f%%)", as.integer(n), pct)

continuous_before <- function(var, group_value) {
  x <- data[data$group == group_value, var]
  sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

continuous_after <- function(var, group_value) {
  dsub <- subset(design_iptw, group == group_value)
  f <- as.formula(paste0("~", var))
  m <- coef(svymean(f, dsub, na.rm = TRUE))[1]
  v <- coef(svyvar(f, dsub, na.rm = TRUE))[1]
  sprintf("%.2f (%.2f)", m, sqrt(v))
}

get_smd <- function(smd_object, var) {
  idx <- which(rownames(smd_object) == var)
  if (length(idx) == 0) return(NA_real_)
  as.numeric(smd_object[idx[1], 1])
}

continuous_rows <- list()
for (v in continuous_vars) {
  continuous_rows[[v]] <- data.frame(
    Characteristic = v, Level = "",
    `Before: No support` = continuous_before(v, "No support"),
    `Before: Support` = continuous_before(v, "Prenatal home visiting support"),
    `Before: SMD` = get_smd(smd_before, v),
    `After IPTW: No support` = continuous_after(v, "No support"),
    `After IPTW: Support` = continuous_after(v, "Prenatal home visiting support"),
    `After IPTW: SMD` = get_smd(smd_after, v),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

categorical_rows <- list()
for (v in factor_vars) {
  levs <- levels(data[[v]])
  tab_control <- table(factor(data[data$group == "No support", v], levels = levs))
  tab_support <- table(factor(data[data$group == "Prenatal home visiting support", v], levels = levs))
  before_n_control <- as.integer(tab_control)
  before_n_support <- as.integer(tab_support)
  before_pct_control <- 100 * before_n_control / N_control
  before_pct_support <- 100 * before_n_support / N_support
  d_control <- data[data$group == "No support", ]
  d_support <- data[data$group == "Prenatal home visiting support", ]
  
  weighted_prop_control <- sapply(levs, function(lv) {
    idx <- d_control[[v]] == lv
    sum(d_control$sw_iptw[idx], na.rm = TRUE) / sum(d_control$sw_iptw, na.rm = TRUE)
  })
  weighted_prop_support <- sapply(levs, function(lv) {
    idx <- d_support[[v]] == lv
    sum(d_support$sw_iptw[idx], na.rm = TRUE) / sum(d_support$sw_iptw, na.rm = TRUE)
  })
  
  after_n_control <- integer_counts_from_prop(weighted_prop_control, N_control)
  after_n_support <- integer_counts_from_prop(weighted_prop_support, N_support)
  after_pct_control <- weighted_prop_control * 100
  after_pct_support <- weighted_prop_support * 100
  this_smd_before <- get_smd(smd_before, v)
  this_smd_after <- get_smd(smd_after, v)
  
  temp <- data.frame(
    Characteristic = c(v, rep("", length(levs) - 1)), Level = levs,
    `Before: No support` = format_n_pct(before_n_control, before_pct_control),
    `Before: Support` = format_n_pct(before_n_support, before_pct_support),
    `Before: SMD` = c(this_smd_before, rep(NA_real_, length(levs) - 1)),
    `After IPTW: No support` = format_n_pct(after_n_control, after_pct_control),
    `After IPTW: Support` = format_n_pct(after_n_support, after_pct_support),
    `After IPTW: SMD` = c(this_smd_after, rep(NA_real_, length(levs) - 1)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  categorical_rows[[v]] <- temp
  
  if (sum(after_n_control) != N_control) stop(paste0("After-IPTW integer counts for ", v, " do not sum to No-support N."))
  if (sum(after_n_support) != N_support) stop(paste0("After-IPTW integer counts for ", v, " do not sum to Support N."))
}

row_list <- list(continuous_rows[["age"]], continuous_rows[["week"]], categorical_rows[["education"]], categorical_rows[["work4day"]], categorical_rows[["income"]], categorical_rows[["nomarry"]], categorical_rows[["alone"]], categorical_rows[["satogaeri"]], continuous_rows[["support"]], continuous_rows[["yutoriT1"]])
baseline_table <- bind_rows(row_list)
baseline_table[["Before: SMD"]] <- ifelse(is.na(baseline_table[["Before: SMD"]]), "", sprintf("%.2f", baseline_table[["Before: SMD"]]))
baseline_table[["After IPTW: SMD"]] <- ifelse(is.na(baseline_table[["After IPTW: SMD"]]), "", sprintf("%.2f", baseline_table[["After IPTW: SMD"]]))

label_map <- c(
  age = "Maternal age, mean (SD)", week = "Gestational week at baseline, mean (SD)",
  education = "Education", work4day = "Employment", income = "Household income",
  nomarry = "Marital status", alone = "Living alone", satogaeri = "Satogaeri",
  support = "Social support, mean (SD)", yutoriT1 = "Baseline yutori score, mean (SD)"
)
baseline_table$Characteristic <- ifelse(baseline_table$Characteristic %in% names(label_map), label_map[baseline_table$Characteristic], baseline_table$Characteristic)

cat("\nFinal sample size check\n")
cat("No support N =", N_control, "\n")
cat("Support N =", N_support, "\n")
cat("Total N =", N_control + N_support, "\n")

cat("\nBalance diagnostics\n")
cat("Maximum |SMD| before weighting =", round(max(abs(smd_before[, 1]), na.rm = TRUE), 3), "\n")
cat("Maximum |SMD| after IPTW =", round(max(abs(smd_after[, 1]), na.rm = TRUE), 3), "\n")
cat("Number with |SMD| > 0.10 before =", sum(abs(smd_before[, 1]) > 0.10, na.rm = TRUE), "\n")
cat("Number with |SMD| > 0.10 after =", sum(abs(smd_after[, 1]) > 0.10, na.rm = TRUE), "\n")

print(baseline_table, row.names = FALSE)