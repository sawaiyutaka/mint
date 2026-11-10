library(geepack)
data <- read.csv("imputed_data.csv")
data$id_msm <- seq_len(nrow(data))
data$childscreen24Mless1 <- ifelse(is.na(data$childscreen24Mone), NA, 1 - data$childscreen24Mone)

cat("\nScreen time <1 hour/day by group\n")
print(table(data$group, data$childscreen24Mless1, useNA = "ifany"))
print(prop.table(table(data$group, data$childscreen24Mless1), margin = 1))
cat("\nYutori at 24 months\n")
print(summary(data$yutori24M))

data$yutori_adhr24M <- ifelse(is.na(data$yutori24M), 0, 1)
data$childscreen24Mless1_adhr24M <- ifelse(is.na(data$childscreen24Mless1), 0, 1)

#ipw
iptw_dat <- data
iptw_num_model <- glm(group ~ 1, family = binomial(), data = iptw_dat)
iptw_den_model <- glm(group ~ age + week + factor(education) + factor(work4day) + factor(income) + factor(nomarry) + factor(alone) + factor(satogaeri) + support + yutoriT1, family = binomial(), data = iptw_dat)
iptw_dat$pA_num <- predict(iptw_num_model, newdata = iptw_dat, type = "response")
iptw_dat$pA_den <- predict(iptw_den_model, newdata = iptw_dat, type = "response")
iptw_dat$sw_iptw <- ifelse(iptw_dat$group == 1, iptw_dat$pA_num / iptw_dat$pA_den, (1 - iptw_dat$pA_num) / (1 - iptw_dat$pA_den))
data$sw_iptw <- NA_real_
data$sw_iptw[match(iptw_dat$id_msm, data$id_msm)] <- iptw_dat$sw_iptw
cat("\nIPTW summary\n")
print(summary(data$sw_iptw))
cat("\nIPTW by treatment group\n")
print(tapply(data$sw_iptw, data$group, summary))

#yutori at 24 months
yutori_wdat <- data
yutori_cens_num <- glm(yutori_adhr24M ~ group, family = binomial(), data = yutori_wdat)
yutori_cens_den <- glm(yutori_adhr24M ~ group + age + week + factor(education) + factor(work4day) + factor(income) + factor(nomarry) + factor(alone) + factor(satogaeri) + support + yutoriT1, family = binomial(), data = yutori_wdat)
yutori_wdat$pC_num <- predict(yutori_cens_num, newdata = yutori_wdat, type = "response")
yutori_wdat$pC_den <- predict(yutori_cens_den, newdata = yutori_wdat, type = "response")
yutori_wdat$sw_ipcw <- ifelse(yutori_wdat$yutori_adhr24M == 1, yutori_wdat$pC_num / yutori_wdat$pC_den, 0)
yutori_wdat$sw_total <- yutori_wdat$sw_iptw * yutori_wdat$sw_ipcw
yutori_cut <- quantile(yutori_wdat$sw_total[yutori_wdat$yutori_adhr24M == 1 & is.finite(yutori_wdat$sw_total)], probs = c(0.01, 0.99), na.rm = TRUE)
yutori_wdat$sw_total_trunc <- pmin(pmax(yutori_wdat$sw_total, yutori_cut[1]), yutori_cut[2])
yutori_msm_dat <- yutori_wdat[yutori_wdat$yutori_adhr24M == 1 & is.finite(yutori_wdat$sw_total_trunc), ]
fit_yutori_msm <- geeglm(yutori24M ~ group, id = id_msm, weights = sw_total_trunc, data = yutori_msm_dat, family = gaussian(), std.err = "san.se")
print(summary(fit_yutori_msm))
b_yutori <- coef(fit_yutori_msm)
se_yutori <- sqrt(diag(vcov(fit_yutori_msm)))
yutori_result <- cbind(Estimate = b_yutori, Lower95 = b_yutori - 1.96 * se_yutori, Upper95 = b_yutori + 1.96 * se_yutori)

# screentime at 24 months
childscreen_wdat <- data
childscreen_cens_num <- glm(childscreen24Mless1_adhr24M ~ group, family = binomial(), data = childscreen_wdat)
childscreen_cens_den <- glm(childscreen24Mless1_adhr24M ~ group + age + week + factor(education) + factor(work4day) + factor(income) + factor(nomarry) + factor(alone) + factor(satogaeri) + support + yutoriT1, family = binomial(), data = childscreen_wdat)
childscreen_wdat$pC_num <- predict(childscreen_cens_num, newdata = childscreen_wdat, type = "response")
childscreen_wdat$pC_den <- predict(childscreen_cens_den, newdata = childscreen_wdat, type = "response")
childscreen_wdat$sw_ipcw <- ifelse(childscreen_wdat$childscreen24Mless1_adhr24M == 1, childscreen_wdat$pC_num / childscreen_wdat$pC_den, 0)
childscreen_wdat$sw_total <- childscreen_wdat$sw_iptw * childscreen_wdat$sw_ipcw
screen_cut <- quantile(childscreen_wdat$sw_total[childscreen_wdat$childscreen24Mless1_adhr24M == 1 & is.finite(childscreen_wdat$sw_total)], probs = c(0.01, 0.99), na.rm = TRUE)
childscreen_wdat$sw_total_trunc <- pmin(pmax(childscreen_wdat$sw_total, screen_cut[1]), screen_cut[2])
childscreen_msm_dat <- childscreen_wdat[childscreen_wdat$childscreen24Mless1_adhr24M == 1 & is.finite(childscreen_wdat$sw_total_trunc), ]
fit_childscreen_msm <- geeglm(childscreen24Mless1 ~ group, id = id_msm, weights = sw_total_trunc, data = childscreen_msm_dat, family = binomial(link = "logit"), std.err = "san.se")
print(summary(fit_childscreen_msm))
b_screen <- coef(fit_childscreen_msm)
se_screen <- sqrt(diag(vcov(fit_childscreen_msm)))
childscreen_OR <- exp(cbind(OR = b_screen, Lower95 = b_screen - 1.96 * se_screen, Upper95 = b_screen + 1.96 * se_screen))

cat("\nNumber with observed outcomes\n")
cat("Yutori observed:", sum(data$yutori_adhr24M), "/", nrow(data), "\n")
cat("Screen time observed:", sum(data$childscreen24Mless1_adhr24M), "/", nrow(data), "\n")
cat("\nIPTW summary\n")
print(summary(data$sw_iptw))
cat("\nYutori IPCW summary\n")
print(summary(yutori_msm_dat$sw_ipcw))
cat("\nYutori final weight before truncation\n")
print(summary(yutori_msm_dat$sw_total))
cat("Yutori 1st percentile cut point:", yutori_cut[1], "\n")
cat("Yutori 99th percentile cut point:", yutori_cut[2], "\n")
cat("\nYutori final weight after truncation\n")
print(summary(yutori_msm_dat$sw_total_trunc))
cat("\nScreen time IPCW summary\n")
print(summary(childscreen_msm_dat$sw_ipcw))
cat("\nScreen final weight before truncation\n")
print(summary(childscreen_msm_dat$sw_total))
cat("Screen 1st percentile cut point:", screen_cut[1], "\n")
cat("Screen 99th percentile cut point:", screen_cut[2], "\n")
cat("\nScreen final weight after truncation\n")
print(summary(childscreen_msm_dat$sw_total_trunc))

n_y_lower <- sum(yutori_msm_dat$sw_total < yutori_cut[1])
n_y_upper <- sum(yutori_msm_dat$sw_total > yutori_cut[2])
n_s_lower <- sum(childscreen_msm_dat$sw_total < screen_cut[1])
n_s_upper <- sum(childscreen_msm_dat$sw_total > screen_cut[2])
cat("\nTruncation counts\n")
cat("Yutori below 1st percentile:", n_y_lower, "\n")
cat("Yutori above 99th percentile:", n_y_upper, "\n")
cat("Screen below 1st percentile:", n_s_lower, "\n")
cat("Screen above 99th percentile:", n_s_upper, "\n")

cat("\nTreatment probability summary\n")
print(summary(iptw_dat$pA_den))
cat("\nYutori observation probability summary\n")
print(summary(yutori_wdat$pC_den))
cat("\nScreen observation probability summary\n")
print(summary(childscreen_wdat$pC_den))

cat("\nFinal result: Yutori mean difference (final weight truncated at 1st/99th percentiles)\n")
print(yutori_result)
cat("\nFinal result: Screen time <1 hour/day odds ratio (final weight truncated at 1st/99th percentiles)\n")
print(childscreen_OR)
yutori_effect <- yutori_result["group", , drop = FALSE]
screen_effect <- childscreen_OR["group", , drop = FALSE]
cat("\nExposure effects only\n")
cat("Yutori:\n")
print(yutori_effect)
cat("Screen time <1 hour/day:\n")
print(screen_effect)