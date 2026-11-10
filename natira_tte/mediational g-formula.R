data <- read.csv("imputed_data.csv")

# Variable formatting
data$group <- factor(data$group, levels = c(0, 1))
data$education <- factor(data$education)
data$work4day <- factor(data$work4day)
data$income <- factor(data$income)
data$nomarry <- factor(data$nomarry)
data$alone <- factor(data$alone)
data$satogaeri <- factor(data$satogaeri)

###epdsT5 as a mediator
##group
# Parametric mediation g-formula
data$epdsT5 <- 0 - data$epdsT5
effect_estimates_modified_fn <- function(data, indices) { 
  
  d_fit_sample <- data[indices, ]
  d_baseline_for_pred <- d_fit_sample  
  n_obs <- nrow(d_baseline_for_pred)
  
  # Keep factor levels identical to original dataset
  d_fit_sample$group <- factor(d_fit_sample$group, levels = levels(data$group))
  d_fit_sample$education <- factor(d_fit_sample$education, levels = levels(data$education))
  d_fit_sample$work4day <- factor(d_fit_sample$work4day, levels = levels(data$work4day))
  d_fit_sample$income <- factor(d_fit_sample$income, levels = levels(data$income))
  d_fit_sample$nomarry <- factor(d_fit_sample$nomarry, levels = levels(data$nomarry))
  d_fit_sample$alone <- factor(d_fit_sample$alone, levels = levels(data$alone))
  d_fit_sample$satogaeri <- factor(d_fit_sample$satogaeri, levels = levels(data$satogaeri))
  
  d_baseline_for_pred$group <- factor(d_baseline_for_pred$group, levels = levels(data$group))
  d_baseline_for_pred$education <- factor(d_baseline_for_pred$education, levels = levels(data$education))
  d_baseline_for_pred$work4day <- factor(d_baseline_for_pred$work4day, levels = levels(data$work4day))
  d_baseline_for_pred$income <- factor(d_baseline_for_pred$income, levels = levels(data$income))
  d_baseline_for_pred$nomarry <- factor(d_baseline_for_pred$nomarry, levels = levels(data$nomarry))
  d_baseline_for_pred$alone <- factor(d_baseline_for_pred$alone, levels = levels(data$alone))
  d_baseline_for_pred$satogaeri <- factor(d_baseline_for_pred$satogaeri, levels = levels(data$satogaeri))
  
  # Time-fixed confounding
  fixed_baseline_confounders_formula_part <- "age + week + education + work4day +
  income + nomarry + alone + satogaeri + support + wellbeingT1"
  
  # Mediator model
  current_L2_A0_V_formula_part <- paste("group +", fixed_baseline_confounders_formula_part)
  
  formula_M_epdsT5 <- paste("epdsT5 ~", current_L2_A0_V_formula_part)
  epdsT5_model <- lm(formula(formula_M_epdsT5),
                     data = d_fit_sample[!is.na(d_fit_sample$epdsT5), ])
  sigma_epdsT5 <- sigma(epdsT5_model); if(is.null(sigma_epdsT5) || is.na(sigma_epdsT5) || sigma_epdsT5 <= 1e-9) sigma_epdsT5 <- 1e-6
  
  # Outcome model
  outcome_predictors <- c(
    "group", fixed_baseline_confounders_formula_part, 
    "epdsT5",
    "group:epdsT5"
  )
  outcome_model_formula_string <- paste(unique(outcome_predictors), collapse = " + ")
  outcome_model <- glm(formula(paste("childscreen24Mone ~", outcome_model_formula_string)),
                       data = d_fit_sample[!is.na(d_fit_sample$childscreen24Mone) & !is.na(d_fit_sample$epdsT5), ],
                       family = binomial)
  
  # Simulate full history 
  simulate_full_history_fn <- function(baseline_data, A0_val) {
    sim_data <- baseline_data 
    sim_data$group <- factor(A0_val, levels = levels(data$group))
    n_sim <- nrow(sim_data)
    
    # Simulated mediator distribution
    sim_data$epdsT5 <- rnorm(n_sim,
                             predict(epdsT5_model, newdata = sim_data),
                             sigma_epdsT5)
    
    return(sim_data)
  }
  
  # Simulate data under A=1 and A=0, allowing mediator to respond naturally
  sim_data_A0_1 <- simulate_full_history_fn(d_baseline_for_pred, 1) 
  sim_data_A0_0 <- simulate_full_history_fn(d_baseline_for_pred, 0) 
  
  realized_epdsT5_hist_A0_0 <- sim_data_A0_0[, c("epdsT5")]
  realized_epdsT5_hist_A0_1 <- sim_data_A0_1[, c("epdsT5")]
  
  # Obtain G terms
  G_epdsT5_hist_A0_0 <- realized_epdsT5_hist_A0_0
  G_epdsT5_hist_A0_1 <- realized_epdsT5_hist_A0_1
  
  # Calculate Q terms ---
  create_Q_dataset_modified_full <- function(baseline_data, A0_val_for_Q, 
                                             G_epdsT5_hist_for_Q) { 
    q_data <- baseline_data 
    q_data$group <- factor(A0_val_for_Q, levels = levels(data$group))
    n_q_obs <- nrow(q_data)
    
    # Assign G history
    q_data$epdsT5 <- G_epdsT5_hist_for_Q
    
    return(q_data)
  }
  
  # Predicted counterfactual values for outcome
  Q_A1_G0_L1 <- mean(predict(outcome_model,
                             newdata = create_Q_dataset_modified_full(d_baseline_for_pred, 1, 
                                                                      G_epdsT5_hist_A0_0),
                             type = "response"), na.rm=TRUE)
  
  Q_A0_G0_L0 <- mean(predict(outcome_model,
                             newdata = create_Q_dataset_modified_full(d_baseline_for_pred, 0, 
                                                                      G_epdsT5_hist_A0_0),
                             type = "response"), na.rm=TRUE)
  
  Q_A1_G1_L1 <- mean(predict(outcome_model,
                             newdata = create_Q_dataset_modified_full(d_baseline_for_pred, 1, 
                                                                      G_epdsT5_hist_A0_1),
                             type = "response"), na.rm=TRUE)
  
  # Calculate TE, IDE, TIE, OE, and Difference
  IDE <- Q_A1_G0_L1 - Q_A0_G0_L0
  TIE <- Q_A1_G1_L1 - Q_A1_G0_L1
  OE <- IDE + TIE
  
  EY_A0_1_natural <- mean(predict(outcome_model,
                                  newdata = sim_data_A0_1,
                                  type = "response"), na.rm=TRUE)
  EY_A0_0_natural <- mean(predict(outcome_model,
                                  newdata = sim_data_A0_0,
                                  type = "response"), na.rm=TRUE)
  TE <- EY_A0_1_natural - EY_A0_0_natural
  
  Difference <- TE - OE 
  
  return(c(TE = TE, OE = OE, IDE = IDE, TIE = TIE, Difference = Difference))
}

# Run on original dataset
set.seed(11111)
results_modified <- effect_estimates_modified_fn(data, seq_len(nrow(data)))

mean_col_names_modified <- c("TE", "OE", "IDE", "TIE", "Difference")
means_modified <- results_modified[mean_col_names_modified]

# Averaged estimates
TE_mean_final <- means_modified["TE"]
OE_mean_final <- means_modified["OE"]
IDE_mean_final <- means_modified["IDE"]
TIE_mean_final <- means_modified["TIE"]
Diff_mean_final <- means_modified["Difference"]

# Calculate PM 
PM_final <- NA
if (!is.na(TE_mean_final) && TE_mean_final != 0 && !is.na(TIE_mean_final) && !is.na(Diff_mean_final)) {
  PM_final <- (TIE_mean_final + Diff_mean_final) / TE_mean_final
}

## Bootstrap CIs
all_bootstrap_results <- list()
metric_names <- c("TE", "OE", "IDE", "TIE", "Difference")

set.seed(11111)
n_bootstrap <- 2000
n_obs_current_data <- nrow(data)

for (b in 1:n_bootstrap) {
  if (b %% 100 == 0) cat("Bootstrap:", b, "/", n_bootstrap, "\n")
  
  bootstrap_indices <- sample(1:n_obs_current_data, n_obs_current_data, replace = TRUE)
  
  res <- tryCatch({
    effect_estimates_modified_fn(data, bootstrap_indices)
  }, error = function(e) {
    warning(paste("Error in bootstrap sample", b, ":", e$message))
    setNames(rep(NA, length(metric_names)), metric_names)
  })
  
  if (!is.null(res) && is.numeric(res) && all(metric_names %in% names(res))) {
    all_bootstrap_results[[b]] <- res[metric_names]
  } else {
    warning(paste("Unexpected result format for bootstrap sample", b))
    all_bootstrap_results[[b]] <- setNames(rep(NA, length(metric_names)), metric_names)
  }
}

all_bootstrap_results <- all_bootstrap_results[
  sapply(all_bootstrap_results,
         function(x) !is.null(x) && is.numeric(x) && length(x) == length(metric_names))
]

bootstrap_results_matrix <- do.call(rbind, all_bootstrap_results)
bootstrap_results_df <- as.data.frame(bootstrap_results_matrix)

cat("\nSuccessful bootstrap samples:",
    sum(complete.cases(bootstrap_results_df[, metric_names])),
    "/",
    n_bootstrap,
    "\n")

boot_means_2000 <- colMeans(bootstrap_results_df[, metric_names, drop = FALSE],
                            na.rm = TRUE)

PM_boot_mean_2000 <- NA_real_
if (!is.na(boot_means_2000["TE"]) && boot_means_2000["TE"] != 0 &&
    !is.na(boot_means_2000["TIE"]) && !is.na(boot_means_2000["Difference"])) {
  PM_boot_mean_2000 <- (boot_means_2000["TIE"] + boot_means_2000["Difference"]) /
    boot_means_2000["TE"]
}

# Percentile boot CIs
alpha <- 0.05
lower_quantile <- alpha / 2
upper_quantile <- 1 - (alpha / 2)

calculate_percentile_ci <- function(estimates) {
  estimates <- estimates[!is.na(estimates)]
  if (length(estimates) < 2) {
    return(c(lower = NA, upper = NA))
  }
  quantile(estimates,
           probs = c(lower_quantile, upper_quantile),
           na.rm = TRUE)
}

ci_TE <- calculate_percentile_ci(bootstrap_results_df[, "TE"])
ci_OE <- calculate_percentile_ci(bootstrap_results_df[, "OE"])
ci_IDE <- calculate_percentile_ci(bootstrap_results_df[, "IDE"])
ci_TIE <- calculate_percentile_ci(bootstrap_results_df[, "TIE"])
ci_Difference <- calculate_percentile_ci(bootstrap_results_df[, "Difference"])

bootstrap_results_df$PM <- NA_real_
valid_te_pm <- bootstrap_results_df$TE != 0 & !is.na(bootstrap_results_df$TE)
valid_tie_pm <- !is.na(bootstrap_results_df$TIE)
valid_diff_pm <- !is.na(bootstrap_results_df$Difference)
valid_pm_rows <- valid_te_pm & valid_tie_pm & valid_diff_pm

bootstrap_results_df$PM[valid_pm_rows] <-
  (bootstrap_results_df$TIE[valid_pm_rows] +
     bootstrap_results_df$Difference[valid_pm_rows]) /
  bootstrap_results_df$TE[valid_pm_rows]

ci_PM <- calculate_percentile_ci(bootstrap_results_df[, "PM"])

# Point estimates
print(paste("TE:", round(TE_mean_final, 4)))
print(paste("OE:", round(OE_mean_final, 4)))
print(paste("IDE:", round(IDE_mean_final, 4)))
print(paste("TIE:", round(TIE_mean_final, 4)))
print(paste("Difference:", round(Diff_mean_final, 4)))
print(paste("PM:", round(PM_final, 4)))

# Bootstrap means and CIs
print(paste("Bootstrap mean TE:", round(boot_means_2000["TE"], 4)))
print(paste("Bootstrap mean OE:", round(boot_means_2000["OE"], 4)))
print(paste("Bootstrap mean IDE:", round(boot_means_2000["IDE"], 4)))
print(paste("Bootstrap mean TIE:", round(boot_means_2000["TIE"], 4)))
print(paste("Bootstrap mean Difference:", round(boot_means_2000["Difference"], 4)))
print(paste("Bootstrap mean PM:", round(PM_boot_mean_2000, 4)))

print(paste0("TE  (95% CI: ", round(ci_TE[1], 4), ", ", round(ci_TE[2], 4), ")"))
print(paste0("OE  (95% CI: ", round(ci_OE[1], 4), ", ", round(ci_OE[2], 4), ")"))
print(paste0("IDE (95% CI: ", round(ci_IDE[1], 4), ", ", round(ci_IDE[2], 4), ")"))
print(paste0("TIE (95% CI: ", round(ci_TIE[1], 4), ", ", round(ci_TIE[2], 4), ")"))
print(paste0("Difference (95% CI: ", round(ci_Difference[1], 4), ", ", round(ci_Difference[2], 4), ")"))
print(paste0("PM (95% CI: ", round(ci_PM[1], 4), ", ", round(ci_PM[2], 4), ")"))

###yutoriT5 as a mediator
##group
# Parametric mediation g-formula
data$yutoriT5 <- 0 - data$yutoriT5
effect_estimates_modified_fn <- function(data, indices) { 
  
  d_fit_sample <- data[indices, ]
  d_baseline_for_pred <- d_fit_sample  
  n_obs <- nrow(d_baseline_for_pred)
  
  # Keep factor levels identical to original dataset
  d_fit_sample$group <- factor(d_fit_sample$group, levels = levels(data$group))
  d_fit_sample$education <- factor(d_fit_sample$education, levels = levels(data$education))
  d_fit_sample$work4day <- factor(d_fit_sample$work4day, levels = levels(data$work4day))
  d_fit_sample$income <- factor(d_fit_sample$income, levels = levels(data$income))
  d_fit_sample$nomarry <- factor(d_fit_sample$nomarry, levels = levels(data$nomarry))
  d_fit_sample$alone <- factor(d_fit_sample$alone, levels = levels(data$alone))
  d_fit_sample$satogaeri <- factor(d_fit_sample$satogaeri, levels = levels(data$satogaeri))
  
  d_baseline_for_pred$group <- factor(d_baseline_for_pred$group, levels = levels(data$group))
  d_baseline_for_pred$education <- factor(d_baseline_for_pred$education, levels = levels(data$education))
  d_baseline_for_pred$work4day <- factor(d_baseline_for_pred$work4day, levels = levels(data$work4day))
  d_baseline_for_pred$income <- factor(d_baseline_for_pred$income, levels = levels(data$income))
  d_baseline_for_pred$nomarry <- factor(d_baseline_for_pred$nomarry, levels = levels(data$nomarry))
  d_baseline_for_pred$alone <- factor(d_baseline_for_pred$alone, levels = levels(data$alone))
  d_baseline_for_pred$satogaeri <- factor(d_baseline_for_pred$satogaeri, levels = levels(data$satogaeri))
  
  # Time-fixed confounding
  fixed_baseline_confounders_formula_part <- "age + week + education + work4day +
  income + nomarry + alone + satogaeri + support + yutoriT1"
  
  # Mediator model
  current_L2_A0_V_formula_part <- paste("group +", fixed_baseline_confounders_formula_part)
  
  formula_M_yutoriT5 <- paste("yutoriT5 ~", current_L2_A0_V_formula_part)
  yutoriT5_model <- lm(formula(formula_M_yutoriT5),
                       data = d_fit_sample[!is.na(d_fit_sample$yutoriT5), ])
  sigma_yutoriT5 <- sigma(yutoriT5_model); if(is.null(sigma_yutoriT5) || is.na(sigma_yutoriT5) || sigma_yutoriT5 <= 1e-9) sigma_yutoriT5 <- 1e-6
  
  # Outcome model
  outcome_predictors <- c(
    "group", fixed_baseline_confounders_formula_part, 
    "yutoriT5",
    "group:yutoriT5"
  )
  outcome_model_formula_string <- paste(unique(outcome_predictors), collapse = " + ")
  outcome_model <- glm(formula(paste("childscreen24Mone ~", outcome_model_formula_string)),
                       data = d_fit_sample[!is.na(d_fit_sample$childscreen24Mone) & !is.na(d_fit_sample$yutoriT5), ],
                       family = binomial)
  
  # Simulate full history 
  simulate_full_history_fn <- function(baseline_data, A0_val) {
    sim_data <- baseline_data 
    sim_data$group <- factor(A0_val, levels = levels(data$group))
    n_sim <- nrow(sim_data)
    
    # Simulated mediator distribution
    sim_data$yutoriT5 <- rnorm(n_sim,
                               predict(yutoriT5_model, newdata = sim_data),
                               sigma_yutoriT5)
    
    return(sim_data)
  }
  
  # Simulate data under A=1 and A=0, allowing mediator to respond naturally
  sim_data_A0_1 <- simulate_full_history_fn(d_baseline_for_pred, 1) 
  sim_data_A0_0 <- simulate_full_history_fn(d_baseline_for_pred, 0) 
  
  realized_yutoriT5_hist_A0_0 <- sim_data_A0_0[, c("yutoriT5")]
  realized_yutoriT5_hist_A0_1 <- sim_data_A0_1[, c("yutoriT5")]
  
  # Obtain G terms
  G_yutoriT5_hist_A0_0 <- realized_yutoriT5_hist_A0_0
  G_yutoriT5_hist_A0_1 <- realized_yutoriT5_hist_A0_1
  
  # Calculate Q terms ---
  create_Q_dataset_modified_full <- function(baseline_data, A0_val_for_Q, 
                                             G_yutoriT5_hist_for_Q) { 
    q_data <- baseline_data 
    q_data$group <- factor(A0_val_for_Q, levels = levels(data$group))
    n_q_obs <- nrow(q_data)
    
    # Assign G history
    q_data$yutoriT5 <- G_yutoriT5_hist_for_Q
    
    return(q_data)
  }
  
  # Predicted counterfactual values for outcome
  Q_A1_G0_L1 <- mean(predict(outcome_model,
                             newdata = create_Q_dataset_modified_full(d_baseline_for_pred, 1, 
                                                                      G_yutoriT5_hist_A0_0),
                             type = "response"), na.rm=TRUE)
  
  Q_A0_G0_L0 <- mean(predict(outcome_model,
                             newdata = create_Q_dataset_modified_full(d_baseline_for_pred, 0, 
                                                                      G_yutoriT5_hist_A0_0),
                             type = "response"), na.rm=TRUE)
  
  Q_A1_G1_L1 <- mean(predict(outcome_model,
                             newdata = create_Q_dataset_modified_full(d_baseline_for_pred, 1, 
                                                                      G_yutoriT5_hist_A0_1),
                             type = "response"), na.rm=TRUE)
  
  # Calculate TE, IDE, TIE, OE, and Difference
  IDE <- Q_A1_G0_L1 - Q_A0_G0_L0
  TIE <- Q_A1_G1_L1 - Q_A1_G0_L1
  OE <- IDE + TIE
  
  EY_A0_1_natural <- mean(predict(outcome_model,
                                  newdata = sim_data_A0_1,
                                  type = "response"), na.rm=TRUE)
  EY_A0_0_natural <- mean(predict(outcome_model,
                                  newdata = sim_data_A0_0,
                                  type = "response"), na.rm=TRUE)
  TE <- EY_A0_1_natural - EY_A0_0_natural
  
  Difference <- TE - OE 
  
  return(c(TE = TE, OE = OE, IDE = IDE, TIE = TIE, Difference = Difference))
}

# Run on original dataset
set.seed(11111)
results_modified <- effect_estimates_modified_fn(data, seq_len(nrow(data)))

mean_col_names_modified <- c("TE", "OE", "IDE", "TIE", "Difference")
means_modified <- results_modified[mean_col_names_modified]

# Averaged estimates
TE_mean_final <- means_modified["TE"]
OE_mean_final <- means_modified["OE"]
IDE_mean_final <- means_modified["IDE"]
TIE_mean_final <- means_modified["TIE"]
Diff_mean_final <- means_modified["Difference"]

# Calculate PM 
PM_final <- NA
if (!is.na(TE_mean_final) && TE_mean_final != 0 && !is.na(TIE_mean_final) && !is.na(Diff_mean_final)) {
  PM_final <- (TIE_mean_final + Diff_mean_final) / TE_mean_final
}

## Bootstrap CIs
all_bootstrap_results <- list()
metric_names <- c("TE", "OE", "IDE", "TIE", "Difference")

set.seed(11111)
n_bootstrap <- 2000
n_obs_current_data <- nrow(data)

for (b in 1:n_bootstrap) {
  if (b %% 100 == 0) cat("Bootstrap:", b, "/", n_bootstrap, "\n")
  
  bootstrap_indices <- sample(1:n_obs_current_data, n_obs_current_data, replace = TRUE)
  
  res <- tryCatch({
    effect_estimates_modified_fn(data, bootstrap_indices)
  }, error = function(e) {
    warning(paste("Error in bootstrap sample", b, ":", e$message))
    setNames(rep(NA, length(metric_names)), metric_names)
  })
  
  if (!is.null(res) && is.numeric(res) && all(metric_names %in% names(res))) {
    all_bootstrap_results[[b]] <- res[metric_names]
  } else {
    warning(paste("Unexpected result format for bootstrap sample", b))
    all_bootstrap_results[[b]] <- setNames(rep(NA, length(metric_names)), metric_names)
  }
}

all_bootstrap_results <- all_bootstrap_results[
  sapply(all_bootstrap_results,
         function(x) !is.null(x) && is.numeric(x) && length(x) == length(metric_names))
]

bootstrap_results_matrix <- do.call(rbind, all_bootstrap_results)
bootstrap_results_df <- as.data.frame(bootstrap_results_matrix)

cat("\nSuccessful bootstrap samples:",
    sum(complete.cases(bootstrap_results_df[, metric_names])),
    "/",
    n_bootstrap,
    "\n")

boot_means_2000 <- colMeans(bootstrap_results_df[, metric_names, drop = FALSE],
                            na.rm = TRUE)

PM_boot_mean_2000 <- NA_real_
if (!is.na(boot_means_2000["TE"]) && boot_means_2000["TE"] != 0 &&
    !is.na(boot_means_2000["TIE"]) && !is.na(boot_means_2000["Difference"])) {
  PM_boot_mean_2000 <- (boot_means_2000["TIE"] + boot_means_2000["Difference"]) /
    boot_means_2000["TE"]
}

# Percentile boot CIs
alpha <- 0.05
lower_quantile <- alpha / 2
upper_quantile <- 1 - (alpha / 2)

calculate_percentile_ci <- function(estimates) {
  estimates <- estimates[!is.na(estimates)]
  if (length(estimates) < 2) {
    return(c(lower = NA, upper = NA))
  }
  quantile(estimates,
           probs = c(lower_quantile, upper_quantile),
           na.rm = TRUE)
}

ci_TE <- calculate_percentile_ci(bootstrap_results_df[, "TE"])
ci_OE <- calculate_percentile_ci(bootstrap_results_df[, "OE"])
ci_IDE <- calculate_percentile_ci(bootstrap_results_df[, "IDE"])
ci_TIE <- calculate_percentile_ci(bootstrap_results_df[, "TIE"])
ci_Difference <- calculate_percentile_ci(bootstrap_results_df[, "Difference"])

bootstrap_results_df$PM <- NA_real_
valid_te_pm <- bootstrap_results_df$TE != 0 & !is.na(bootstrap_results_df$TE)
valid_tie_pm <- !is.na(bootstrap_results_df$TIE)
valid_diff_pm <- !is.na(bootstrap_results_df$Difference)
valid_pm_rows <- valid_te_pm & valid_tie_pm & valid_diff_pm

bootstrap_results_df$PM[valid_pm_rows] <-
  (bootstrap_results_df$TIE[valid_pm_rows] +
     bootstrap_results_df$Difference[valid_pm_rows]) /
  bootstrap_results_df$TE[valid_pm_rows]

ci_PM <- calculate_percentile_ci(bootstrap_results_df[, "PM"])

# Point estimates
print(paste("TE:", round(TE_mean_final, 4)))
print(paste("OE:", round(OE_mean_final, 4)))
print(paste("IDE:", round(IDE_mean_final, 4)))
print(paste("TIE:", round(TIE_mean_final, 4)))
print(paste("Difference:", round(Diff_mean_final, 4)))
print(paste("PM:", round(PM_final, 4)))

# Bootstrap means and CIs
print(paste("Bootstrap mean TE:", round(boot_means_2000["TE"], 4)))
print(paste("Bootstrap mean OE:", round(boot_means_2000["OE"], 4)))
print(paste("Bootstrap mean IDE:", round(boot_means_2000["IDE"], 4)))
print(paste("Bootstrap mean TIE:", round(boot_means_2000["TIE"], 4)))
print(paste("Bootstrap mean Difference:", round(boot_means_2000["Difference"], 4)))
print(paste("Bootstrap mean PM:", round(PM_boot_mean_2000, 4)))

print(paste0("TE  (95% CI: ", round(ci_TE[1], 4), ", ", round(ci_TE[2], 4), ")"))
print(paste0("OE  (95% CI: ", round(ci_OE[1], 4), ", ", round(ci_OE[2], 4), ")"))
print(paste0("IDE (95% CI: ", round(ci_IDE[1], 4), ", ", round(ci_IDE[2], 4), ")"))
print(paste0("TIE (95% CI: ", round(ci_TIE[1], 4), ", ", round(ci_TIE[2], 4), ")"))
print(paste0("Difference (95% CI: ", round(ci_Difference[1], 4), ", ", round(ci_Difference[2], 4), ")"))
print(paste0("PM (95% CI: ", round(ci_PM[1], 4), ", ", round(ci_PM[2], 4), ")"))