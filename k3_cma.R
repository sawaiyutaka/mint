# ============================================================
# Causal mediation analyses
#
# Exposure:
#   intervention
#     0 = Control
#     1 = Intervention
#
# Analysis 1:
#   intervention
#     -> childscreen24M
#     -> nigobun
#
# Analysis 2:
#   intervention
#     -> childscreen24M
#     -> ASQ3_communication
#
# Analysis 3:
#   intervention
#     -> G3
#     -> childscreen24M
#
# Coding:
#   childscreen24M:
#     Continuous hours/day
#
#   nigobun:
#     C4-4 = 1 -> 0
#     C4-4 = 2 -> 1
#     1 means two-word sentence not yet acquired
#
#   ASQ3_communication:
#     0-60 points
#     Higher scores indicate better communication
#
# Missing data:
#   mice random forest
#   m = 20
#   ntree = 100
#
# Important:
#   basec_varsには、介入前に測定された交絡因子だけを指定する。
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

# 必要な場合のみ実行
# install.packages(
#   c(
#     "readxl",
#     "mice",
#     "dplyr",
#     "openxlsx",
#     "randomForest",
#     "CMAverse"
#   )
# )

library(readxl)
library(mice)
library(dplyr)
library(openxlsx)
library(randomForest)
library(CMAverse)


# ============================================================
# 1. File and analysis settings
# ============================================================

input_file <- paste0(
  "D:/mint/data_xlsx/",
  "merged_early_control.xlsx"
)

output_file <- paste0(
  "D:/mint/results/",
  "CMA_intervention_screen_G3_language.xlsx"
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

# 最終解析では1000～2000程度を推奨
nboot_cma <- 500

# 介入×媒介因子の交互作用を含める
include_em_interaction <- TRUE


# ============================================================
# 2. Variable settings
# ============================================================

id_var <- "users_id"

exposure_var <- "intervention"

af_vars <- paste0("AF", 1:6)
asq_vars <- paste0("R", 1:6)

screen_var <- "childscreen24M"
two_word_raw <- "C4-4"
two_word_var <- "nigobun"
asq_var <- "ASQ3_communication"
g3_var <- "G3"

age_var <- "age_corrected"
income_var <- "H4_P1"
education_var <- "mother_education_6grp"

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


# ============================================================
# 3. Confounder settings
# ============================================================
# 介入前に測定された交絡因子のみを入れる。
#
# まず添付コードと同様に未調整モデルを実行する場合：
basec_vars <- character(0)

# 調整モデルの例：
# 研究デザイン・DAGに基づいて選択すること。
#
# basec_vars <- c(
#   "age_corrected",
#   "H4_P1",
#   "mother_education_6grp"
# )
#
# 注意：
# 介入後に測定された変数を通常のbasecとして入れると、
# 媒介経路を遮断する可能性がある。


required_columns <- unique(
  c(
    id_var,
    exposure_var,
    af_vars,
    asq_vars,
    two_word_raw,
    g3_var,
    age_var,
    income_var,
    education_var,
    epds_vars,
    support_vars,
    basec_vars
  )
)


# ============================================================
# 4. Utility functions
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


# Rubin則によるプーリング
pool_scalar_rubin <- function(
    q,
    u,
    transform = c("identity", "log")
) {
  
  transform <- match.arg(transform)
  
  keep <- is.finite(q) &
    is.finite(u) &
    u >= 0
  
  q <- q[keep]
  u <- u[keep]
  
  m <- length(q)
  
  if (m < 2) {
    
    if (transform == "log") {
      return(
        data.frame(
          estimate = NA_real_,
          std_error_log = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_,
          p_value = NA_real_,
          df = NA_real_,
          m_used = m
        )
      )
    }
    
    return(
      data.frame(
        estimate = NA_real_,
        std_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        p_value = NA_real_,
        df = NA_real_,
        m_used = m
      )
    )
  }
  
  if (transform == "log") {
    
    if (any(q <= 0)) {
      stop(
        paste0(
          "A ratio estimate was non-positive ",
          "and could not be pooled on the log scale."
        )
      )
    }
    
    # Delta method
    u <- u / (q^2)
    q <- log(q)
  }
  
  q_bar <- mean(q)
  u_bar <- mean(u)
  b <- stats::var(q)
  
  total_var <- u_bar +
    (1 + 1 / m) * b
  
  se_total <- sqrt(total_var)
  
  if (
    !is.finite(b) ||
    b <= .Machine$double.eps
  ) {
    
    df_value <- Inf
    critical <- stats::qnorm(0.975)
    
    if (is.finite(se_total) && se_total > 0) {
      p_value <- 2 * stats::pnorm(
        -abs(q_bar / se_total)
      )
    } else {
      p_value <- NA_real_
    }
    
  } else {
    
    df_value <- (m - 1) * (
      1 +
        u_bar /
        ((1 + 1 / m) * b)
    )^2
    
    critical <- stats::qt(
      0.975,
      df = df_value
    )
    
    if (is.finite(se_total) && se_total > 0) {
      p_value <- 2 * stats::pt(
        -abs(q_bar / se_total),
        df = df_value
      )
    } else {
      p_value <- NA_real_
    }
  }
  
  conf_low <- q_bar -
    critical * se_total
  
  conf_high <- q_bar +
    critical * se_total
  
  if (transform == "log") {
    
    return(
      data.frame(
        estimate = exp(q_bar),
        std_error_log = se_total,
        conf_low = exp(conf_low),
        conf_high = exp(conf_high),
        p_value = p_value,
        df = df_value,
        m_used = m
      )
    )
  }
  
  data.frame(
    estimate = q_bar,
    std_error = se_total,
    conf_low = conf_low,
    conf_high = conf_high,
    p_value = p_value,
    df = df_value,
    m_used = m
  )
}


extract_cmest_effects <- function(
    fit,
    imputation,
    analysis_label
) {
  
  data.frame(
    imputation = imputation,
    analysis = analysis_label,
    effect = names(fit$effect.pe),
    estimate = as.numeric(fit$effect.pe),
    std_error = as.numeric(fit$effect.se),
    stringsAsFactors = FALSE
  )
}


pool_cmest_effects <- function(
    effect_data,
    outcome_type
) {
  
  effect_names <- unique(
    effect_data$effect
  )
  
  bind_rows(
    lapply(
      effect_names,
      function(effect_name) {
        
        x <- effect_data[
          effect_data$effect == effect_name,
          ,
          drop = FALSE
        ]
        
        # 二値アウトカムのratio-scale効果
        is_ratio <- outcome_type == "binary" &&
          effect_name %in% c(
            "Rcde",
            "Rpnde",
            "Rtnde",
            "Rpnie",
            "Rtnie",
            "Rte"
          )
        
        pooled <- pool_scalar_rubin(
          q = x$estimate,
          u = x$std_error^2,
          transform = ifelse(
            is_ratio,
            "log",
            "identity"
          )
        )
        
        pooled$analysis <- unique(
          x$analysis
        )
        
        pooled$effect <- effect_name
        
        pooled$scale <- ifelse(
          is_ratio,
          "ratio; pooled on log scale",
          "additive or proportion scale"
        )
        
        pooled
      }
    )
  ) %>%
    select(
      analysis,
      effect,
      scale,
      everything()
    )
}


# ============================================================
# 5. Read data
# ============================================================

df_raw <- read_excel(input_file)

names(df_raw) <- trimws(
  names(df_raw)
)

missing_columns <- setdiff(
  required_columns,
  names(df_raw)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "以下の列がありません: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

if (anyDuplicated(df_raw[[id_var]]) > 0) {
  stop(
    paste0(
      "users_idに重複があります。 ",
      "1行が1参加者になっているか確認してください。"
    )
  )
}

if (anyNA(df_raw[[id_var]])) {
  stop(
    "users_idに欠損値があります。"
  )
}

df <- as.data.frame(df_raw)

cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")


# ============================================================
# 6. Convert source variables to numeric
# ============================================================

numeric_source_vars <- unique(
  c(
    exposure_var,
    af_vars,
    asq_vars,
    two_word_raw,
    g3_var,
    age_var,
    epds_vars
  )
)

for (v in numeric_source_vars) {
  df[[v]] <- factor_to_numeric(
    df[[v]]
  )
}


# ============================================================
# 7. Validate intervention
# ============================================================

invalid_intervention <- unique(
  df[[exposure_var]][
    !is.na(df[[exposure_var]]) &
      !(df[[exposure_var]] %in% c(0, 1))
  ]
)

if (length(invalid_intervention) > 0) {
  stop(
    paste0(
      "interventionに0または1以外の値があります: ",
      paste(
        invalid_intervention,
        collapse = ", "
      )
    )
  )
}

if (anyNA(df[[exposure_var]])) {
  stop(
    paste0(
      "interventionが欠損している参加者が",
      sum(is.na(df[[exposure_var]])),
      "人います。"
    )
  )
}

if (
  !all(
    c(0, 1) %in%
    unique(df[[exposure_var]])
  )
) {
  stop(
    paste0(
      "intervention = 0と1の",
      "両方の群が必要です。"
    )
  )
}


# ============================================================
# 8. Construct continuous screen time
# ============================================================

for (v in af_vars) {
  df[[paste0(v, "_h")]] <-
    to_hours(df[[v]])
}

df[[screen_var]] <- ifelse(
  rowSums(
    is.na(df[af_vars])
  ) > 0,
  NA_real_,
  (
    (
      df$AF1_h +
        df$AF3_h +
        df$AF5_h
    ) * 5 +
      (
        df$AF2_h +
          df$AF4_h +
          df$AF6_h
      ) * 2
  ) / 7
)


# ============================================================
# 9. Construct ASQ-3 Communication score
# ============================================================
#
# Original coding:
#   1 = Yes
#   2 = Sometimes
#   3 = Not Yet
#
# Scoring:
#   Yes       = 10
#   Sometimes = 5
#   Not Yet   = 0
#
# Missing rule:
#   0 missing   -> sum
#   1-2 missing -> mean of answered items × 6
#   >=3 missing -> NA

for (v in asq_vars) {
  
  df[[paste0(v, "_score")]] <-
    dplyr::case_when(
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

df$ASQ3_communication_n_missing <-
  rowSums(
    is.na(
      df[asq_score_vars]
    )
  )

df[[asq_var]] <- ifelse(
  df$ASQ3_communication_n_missing <= 2,
  rowMeans(
    df[asq_score_vars],
    na.rm = TRUE
  ) * 6,
  NA_real_
)

df[[asq_var]][
  is.nan(df[[asq_var]])
] <- NA_real_


# ============================================================
# 10. Fix mediator reference values before MI
# ============================================================
# EMint = TRUEの場合、CDEは指定したmvalで定義される。
# ここでは観測データの中央値を使用する。

if (all(is.na(df[[screen_var]]))) {
  stop(
    "childscreen24Mがすべて欠損しています。"
  )
}

if (all(is.na(df[[g3_var]]))) {
  stop(
    "G3がすべて欠損しています。"
  )
}

screen_mval <- median(
  df[[screen_var]],
  na.rm = TRUE
)

g3_mval <- median(
  df[[g3_var]],
  na.rm = TRUE
)

cat(
  "Reference value of childscreen24M:",
  screen_mval,
  "\n"
)

cat(
  "Reference value of G3:",
  g3_mval,
  "\n"
)


# ============================================================
# 11. Dataset for multiple imputation
# ============================================================

analysis_columns <- unique(
  c(
    id_var,
    exposure_var,
    screen_var,
    two_word_raw,
    asq_var,
    g3_var,
    age_var,
    income_var,
    education_var,
    epds_vars,
    support_vars,
    basec_vars
  )
)

dat <- df[
  ,
  analysis_columns,
  drop = FALSE
]

non_id_vars <- setdiff(
  names(dat),
  id_var
)

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
  "Number of participants used for MI:",
  nrow(dat),
  "\n"
)


# ============================================================
# 12. Variable types for imputation
# ============================================================

dat[[exposure_var]] <- factor_to_numeric(
  dat[[exposure_var]]
)

dat[[screen_var]] <- factor_to_numeric(
  dat[[screen_var]]
)

dat[[asq_var]] <- factor_to_numeric(
  dat[[asq_var]]
)

dat[[g3_var]] <- factor_to_numeric(
  dat[[g3_var]]
)

dat[[age_var]] <- factor_to_numeric(
  dat[[age_var]]
)

for (v in epds_vars) {
  dat[[v]] <- factor_to_numeric(
    dat[[v]]
  )
}

# C4-4は元のカテゴリとして補完後、
# nigobunを作成する
dat[[two_word_raw]] <- factor(
  dat[[two_word_raw]],
  levels = c(1, 2)
)

# H4_P1
dat[[income_var]] <- factor(
  dat[[income_var]]
)

# E1 variables
for (v in support_vars) {
  dat[[v]] <- factor(
    dat[[v]],
    levels = 1:6
  )
}

# 母親の学歴
dat[[education_var]] <- factor(
  dat[[education_var]]
)


# ============================================================
# 13. Missing summary before imputation
# ============================================================

missing_summary <- data.frame(
  variable = names(dat),
  
  n_missing = sapply(
    dat,
    function(x) {
      sum(is.na(x))
    }
  ),
  
  prop_missing = sapply(
    dat,
    function(x) {
      mean(is.na(x))
    }
  ),
  
  stringsAsFactors = FALSE
)

print(missing_summary)


# ============================================================
# 14. Multiple imputation
# ============================================================

ini <- mice(
  dat,
  maxit = 0,
  printFlag = FALSE
)

meth <- ini$method
pred <- ini$predictorMatrix

# 最初にすべて補完なしに設定
meth[] <- ""

vars_with_missing <- names(dat)[
  colSums(
    is.na(dat)
  ) > 0
]

# 欠損がある変数をrandom forestで補完
meth[vars_with_missing] <- "rf"

# IDは補完しない・予測にも使用しない
meth[id_var] <- ""
pred[, id_var] <- 0
pred[id_var, ] <- 0

# interventionは補完しない
meth[exposure_var] <- ""

diag(pred) <- 0

cat(
  "\nVariables imputed by random forest:\n"
)

print(
  names(meth)[
    meth == "rf"
  ]
)

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
# 15. Derive variables after MI
# ============================================================

derive_cma_variables <- function(d) {
  
  intervention_num <- factor_to_numeric(
    d[[exposure_var]]
  )
  
  c4_num <- factor_to_numeric(
    d[[two_word_raw]]
  )
  
  d[[exposure_var]] <- intervention_num
  
  # nigobun:
  # 0 = acquired
  # 1 = not yet acquired
  d[[two_word_var]] <- dplyr::case_when(
    is.na(c4_num) ~ NA_real_,
    c4_num == 1 ~ 0,
    c4_num == 2 ~ 1,
    TRUE ~ NA_real_
  )
  
  d
}

analysis_list <- lapply(
  completed_list,
  derive_cma_variables
)


# ============================================================
# 16. Validate completed datasets
# ============================================================

for (i in seq_along(analysis_list)) {
  
  d <- analysis_list[[i]]
  
  variables_to_check <- unique(
    c(
      exposure_var,
      screen_var,
      two_word_var,
      asq_var,
      g3_var,
      basec_vars
    )
  )
  
  if (
    anyNA(
      d[
        ,
        variables_to_check,
        drop = FALSE
      ]
    )
  ) {
    stop(
      paste0(
        "Imputation ",
        i,
        " に欠損値が残っています。"
      )
    )
  }
  
  if (
    !all(
      c(0, 1) %in%
      unique(d[[exposure_var]])
    )
  ) {
    stop(
      paste0(
        "Imputation ",
        i,
        " にinterventionの両群がありません。"
      )
    )
  }
  
  if (
    !all(
      c(0, 1) %in%
      unique(d[[two_word_var]])
    )
  ) {
    stop(
      paste0(
        "Imputation ",
        i,
        " にnigobunの両カテゴリーがありません。"
      )
    )
  }
  
  if (
    stats::var(
      d[[screen_var]]
    ) <= 0
  ) {
    stop(
      paste0(
        "Imputation ",
        i,
        " でchildscreen24Mに変動がありません。"
      )
    )
  }
  
  if (
    stats::var(
      d[[g3_var]]
    ) <= 0
  ) {
    stop(
      paste0(
        "Imputation ",
        i,
        " でG3に変動がありません。"
      )
    )
  }
}


# ============================================================
# 17. Descriptive counts across imputations
# ============================================================

count_data <- bind_rows(
  lapply(
    seq_along(analysis_list),
    function(i) {
      
      d <- analysis_list[[i]]
      
      data.frame(
        imputation = i,
        
        control_0 = sum(
          d[[exposure_var]] == 0
        ),
        
        intervention_1 = sum(
          d[[exposure_var]] == 1
        ),
        
        two_word_acquired_0 = sum(
          d[[two_word_var]] == 0
        ),
        
        two_word_not_acquired_1 = sum(
          d[[two_word_var]] == 1
        ),
        
        mean_screen_time = mean(
          d[[screen_var]]
        ),
        
        mean_ASQ3 = mean(
          d[[asq_var]]
        ),
        
        mean_G3 = mean(
          d[[g3_var]]
        ),
        
        stringsAsFactors = FALSE
      )
    }
  )
)

count_summary <- count_data %>%
  summarise(
    across(
      -imputation,
      list(
        mean = mean,
        min = min,
        max = max
      )
    )
  )


# ============================================================
# 18. Fit the three CMA models
# ============================================================

basec_arg <- if (
  length(basec_vars) == 0
) {
  NULL
} else {
  basec_vars
}

n_imp <- length(analysis_list)

fit_screen_to_two_word <- vector(
  "list",
  n_imp
)

fit_screen_to_asq <- vector(
  "list",
  n_imp
)

fit_g3_to_screen <- vector(
  "list",
  n_imp
)

effects_screen_to_two_word <- vector(
  "list",
  n_imp
)

effects_screen_to_asq <- vector(
  "list",
  n_imp
)

effects_g3_to_screen <- vector(
  "list",
  n_imp
)


for (i in seq_along(analysis_list)) {
  
  d <- analysis_list[[i]]
  
  cat(
    "\n=====================================\n"
  )
  
  cat(
    "Fitting imputation",
    i,
    "of",
    n_imp,
    "\n"
  )
  
  cat(
    "=====================================\n"
  )
  
  
  # ----------------------------------------------------------
  # Analysis 1
  #
  # intervention
  #   -> childscreen24M
  #   -> nigobun
  #
  # Mediator: continuous
  # Outcome: binary
  #
  # nigobun = 1 means not yet acquired
  # ----------------------------------------------------------
  
  set.seed(
    seed_main + i
  )
  
  fit_screen_to_two_word[[i]] <- cmest(
    data = d,
    model = "rb",
    
    outcome = two_word_var,
    exposure = exposure_var,
    mediator = screen_var,
    basec = basec_arg,
    
    EMint = include_em_interaction,
    
    mreg = list("linear"),
    yreg = "logistic",
    
    astar = 0,
    a = 1,
    
    # CDEを観測データのスクリーンタイム中央値で評価
    mval = list(screen_mval),
    
    yval = 1,
    
    estimation = "imputation",
    inference = "bootstrap",
    nboot = nboot_cma,
    boot.ci.type = "per"
  )
  
  effects_screen_to_two_word[[i]] <-
    extract_cmest_effects(
      fit_screen_to_two_word[[i]],
      imputation = i,
      analysis_label = paste0(
        "1: intervention -> childscreen24M ",
        "-> nigobun"
      )
    )
  
  
  # ----------------------------------------------------------
  # Analysis 2
  #
  # intervention
  #   -> childscreen24M
  #   -> ASQ3_communication
  #
  # Mediator: continuous
  # Outcome: continuous
  # ----------------------------------------------------------
  
  fit_screen_to_asq[[i]] <- cmest(
    data = d,
    model = "rb",
    
    outcome = asq_var,
    exposure = exposure_var,
    mediator = screen_var,
    basec = basec_arg,
    
    EMint = include_em_interaction,
    
    mreg = list("linear"),
    yreg = "linear",
    
    astar = 0,
    a = 1,
    
    mval = list(screen_mval),
    
    estimation = "paramfunc",
    inference = "delta"
  )
  
  effects_screen_to_asq[[i]] <-
    extract_cmest_effects(
      fit_screen_to_asq[[i]],
      imputation = i,
      analysis_label = paste0(
        "2: intervention -> childscreen24M ",
        "-> ASQ3_communication"
      )
    )
  
  
  # ----------------------------------------------------------
  # Analysis 3
  #
  # intervention
  #   -> G3
  #   -> childscreen24M
  #
  # Mediator: continuous
  # Outcome: continuous
  # ----------------------------------------------------------
  
  fit_g3_to_screen[[i]] <- cmest(
    data = d,
    model = "rb",
    
    outcome = screen_var,
    exposure = exposure_var,
    mediator = g3_var,
    basec = basec_arg,
    
    EMint = include_em_interaction,
    
    mreg = list("linear"),
    yreg = "linear",
    
    astar = 0,
    a = 1,
    
    mval = list(g3_mval),
    
    estimation = "paramfunc",
    inference = "delta"
  )
  
  effects_g3_to_screen[[i]] <-
    extract_cmest_effects(
      fit_g3_to_screen[[i]],
      imputation = i,
      analysis_label = paste0(
        "3: intervention -> G3 ",
        "-> childscreen24M"
      )
    )
}


# ============================================================
# 19. Combine results from each imputation
# ============================================================

effects_1_each_mi <- bind_rows(
  effects_screen_to_two_word
)

effects_2_each_mi <- bind_rows(
  effects_screen_to_asq
)

effects_3_each_mi <- bind_rows(
  effects_g3_to_screen
)


# ============================================================
# 20. Pool results across imputations
# ============================================================

results_1_pooled <- pool_cmest_effects(
  effects_1_each_mi,
  outcome_type = "binary"
) %>%
  mutate(
    exposure_comparison = paste0(
      "Intervention (1) vs Control (0)"
    ),
    
    mediator = paste0(
      "childscreen24M, continuous hours/day"
    ),
    
    mediator_value_for_CDE = screen_mval,
    
    outcome_definition = paste0(
      "nigobun: 1 = not yet acquired; ",
      "0 = acquired"
    ),
    
    model_description = paste0(
      "Linear mediator model; ",
      "logistic outcome model; ",
      "counterfactual imputation"
    ),
    
    EM_interaction = include_em_interaction,
    
    adjustment = ifelse(
      length(basec_vars) == 0,
      "None",
      paste(
        basec_vars,
        collapse = ", "
      )
    )
  ) %>%
  select(
    analysis,
    exposure_comparison,
    mediator,
    mediator_value_for_CDE,
    outcome_definition,
    model_description,
    EM_interaction,
    adjustment,
    effect,
    scale,
    everything()
  )


results_2_pooled <- pool_cmest_effects(
  effects_2_each_mi,
  outcome_type = "continuous"
) %>%
  mutate(
    exposure_comparison = paste0(
      "Intervention (1) vs Control (0)"
    ),
    
    mediator = paste0(
      "childscreen24M, continuous hours/day"
    ),
    
    mediator_value_for_CDE = screen_mval,
    
    outcome_definition = paste0(
      "ASQ3_communication, 0-60; ",
      "higher scores indicate better communication"
    ),
    
    model_description = paste0(
      "Linear mediator model; ",
      "linear outcome model"
    ),
    
    EM_interaction = include_em_interaction,
    
    adjustment = ifelse(
      length(basec_vars) == 0,
      "None",
      paste(
        basec_vars,
        collapse = ", "
      )
    )
  ) %>%
  select(
    analysis,
    exposure_comparison,
    mediator,
    mediator_value_for_CDE,
    outcome_definition,
    model_description,
    EM_interaction,
    adjustment,
    effect,
    scale,
    everything()
  )


results_3_pooled <- pool_cmest_effects(
  effects_3_each_mi,
  outcome_type = "continuous"
) %>%
  mutate(
    exposure_comparison = paste0(
      "Intervention (1) vs Control (0)"
    ),
    
    mediator = "G3, continuous",
    
    mediator_value_for_CDE = g3_mval,
    
    outcome_definition = paste0(
      "childscreen24M, continuous hours/day"
    ),
    
    model_description = paste0(
      "Linear mediator model; ",
      "linear outcome model"
    ),
    
    EM_interaction = include_em_interaction,
    
    adjustment = ifelse(
      length(basec_vars) == 0,
      "None",
      paste(
        basec_vars,
        collapse = ", "
      )
    )
  ) %>%
  select(
    analysis,
    exposure_comparison,
    mediator,
    mediator_value_for_CDE,
    outcome_definition,
    model_description,
    EM_interaction,
    adjustment,
    effect,
    scale,
    everything()
  )


all_pooled_results <- bind_rows(
  results_1_pooled,
  results_2_pooled,
  results_3_pooled
)


# ============================================================
# 21. Analysis information
# ============================================================

analysis_information <- data.frame(
  item = c(
    "Exposure",
    "Exposure reference",
    "Exposure active",
    "Analysis 1",
    "Analysis 1 outcome coding",
    "Analysis 2",
    "Analysis 2 outcome coding",
    "Analysis 3",
    "Screen-time mediator value for CDE",
    "G3 mediator value for CDE",
    "Exposure-mediator interaction",
    "Adjustment variables",
    "MI method",
    "Number of imputations",
    "MICE iterations",
    "Random forest trees",
    "Binary-outcome CMA estimation",
    "Binary-outcome bootstrap samples"
  ),
  
  value = c(
    "intervention",
    "0 = Control",
    "1 = Intervention",
    
    paste0(
      "intervention -> childscreen24M ",
      "-> nigobun"
    ),
    
    paste0(
      "nigobun: 0 = acquired; ",
      "1 = not yet acquired"
    ),
    
    paste0(
      "intervention -> childscreen24M ",
      "-> ASQ3_communication"
    ),
    
    paste0(
      "ASQ3_communication: 0-60; ",
      "higher = better"
    ),
    
    paste0(
      "intervention -> G3 ",
      "-> childscreen24M"
    ),
    
    as.character(screen_mval),
    as.character(g3_mval),
    as.character(include_em_interaction),
    
    ifelse(
      length(basec_vars) == 0,
      paste0(
        "None: unadjusted analysis; ",
        "causal interpretation requires caution"
      ),
      paste(
        basec_vars,
        collapse = ", "
      )
    ),
    
    "Random forest via mice",
    as.character(m_imp),
    as.character(mice_maxit),
    as.character(ntree_rf),
    
    paste0(
      "Direct counterfactual imputation ",
      "with bootstrap"
    ),
    
    as.character(nboot_cma)
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# 22. Effect-name guide
# ============================================================

effect_guide <- data.frame(
  effect = c(
    "cde / Rcde",
    "pnde / Rpnde",
    "tnde / Rtnde",
    "pnie / Rpnie",
    "tnie / Rtnie",
    "te / Rte",
    "pm"
  ),
  
  interpretation = c(
    paste0(
      "Controlled direct effect at the mediator ",
      "value specified by mval"
    ),
    
    paste0(
      "Pure natural direct effect: direct effect ",
      "when the mediator follows its control-group distribution"
    ),
    
    paste0(
      "Total natural direct effect: direct effect ",
      "when the mediator follows its intervention-group distribution"
    ),
    
    paste0(
      "Pure natural indirect effect: effect through ",
      "the mediator while exposure is fixed at control"
    ),
    
    paste0(
      "Total natural indirect effect: effect through ",
      "the mediator while exposure is fixed at intervention"
    ),
    
    "Total effect",
    
    "Proportion mediated"
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# 23. Print main results
# ============================================================

cat("\n")
cat("=====================================\n")
cat("ANALYSIS 1\n")
cat("intervention -> screen time -> nigobun\n")
cat("=====================================\n")
print(results_1_pooled)

cat("\n")
cat("=====================================\n")
cat("ANALYSIS 2\n")
cat("intervention -> screen time -> ASQ-3\n")
cat("=====================================\n")
print(results_2_pooled)

cat("\n")
cat("=====================================\n")
cat("ANALYSIS 3\n")
cat("intervention -> G3 -> screen time\n")
cat("=====================================\n")
print(results_3_pooled)


# ============================================================
# 24. Save results to Excel
# ============================================================

wb <- createWorkbook()


addWorksheet(
  wb,
  "all_pooled_results"
)

writeData(
  wb,
  "all_pooled_results",
  all_pooled_results
)


addWorksheet(
  wb,
  "1_screen_to_nigobun"
)

writeData(
  wb,
  "1_screen_to_nigobun",
  results_1_pooled
)


addWorksheet(
  wb,
  "2_screen_to_ASQ3"
)

writeData(
  wb,
  "2_screen_to_ASQ3",
  results_2_pooled
)


addWorksheet(
  wb,
  "3_G3_to_screen"
)

writeData(
  wb,
  "3_G3_to_screen",
  results_3_pooled
)


addWorksheet(
  wb,
  "1_each_MI"
)

writeData(
  wb,
  "1_each_MI",
  effects_1_each_mi
)


addWorksheet(
  wb,
  "2_each_MI"
)

writeData(
  wb,
  "2_each_MI",
  effects_2_each_mi
)


addWorksheet(
  wb,
  "3_each_MI"
)

writeData(
  wb,
  "3_each_MI",
  effects_3_each_mi
)


addWorksheet(
  wb,
  "counts_each_MI"
)

writeData(
  wb,
  "counts_each_MI",
  count_data
)


addWorksheet(
  wb,
  "counts_summary"
)

writeData(
  wb,
  "counts_summary",
  count_summary
)


addWorksheet(
  wb,
  "missing_summary"
)

writeData(
  wb,
  "missing_summary",
  missing_summary
)


addWorksheet(
  wb,
  "analysis_information"
)

writeData(
  wb,
  "analysis_information",
  analysis_information
)


addWorksheet(
  wb,
  "effect_guide"
)

writeData(
  wb,
  "effect_guide",
  effect_guide
)


# MICEで記録されたイベント
if (!is.null(imp$loggedEvents)) {
  
  addWorksheet(
    wb,
    "MICE_logged_events"
  )
  
  writeData(
    wb,
    "MICE_logged_events",
    imp$loggedEvents
  )
}


# 列幅を調整
for (sheet_name in names(wb)) {
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = 1:30,
    widths = "auto"
  )
  
  freezePane(
    wb,
    sheet = sheet_name,
    firstRow = TRUE
  )
}


saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)

cat("\n")
cat("=====================================\n")
cat("Analysis completed successfully.\n")
cat("Output file:\n")
cat(output_file, "\n")
cat("=====================================\n")