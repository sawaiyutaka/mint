# ============================================================
# 記述統計および介入群・対照群の比較
#
# Groups:
#   intervention = 1: Intervention
#   intervention = 0: Control
#
# Continuous variables:
#   - Mean (SD)
#   - Welch's t-test
#
# Binary variables:
#   - n (%)
#   - Pearson's chi-squared test
#   - Fisher's exact test if any expected count is <5
#
# Missing data:
#   - MICEによる欠損値補完は行わない
#   - ASQ-3についてのみ、ASQ-3固有の欠損項目処理を行う
# ============================================================


# 必要な場合のみ実行
# install.packages(c("readxl", "dplyr", "openxlsx"))

library(readxl)
library(dplyr)
library(openxlsx)


# =========================
# 1. ファイル設定
# =========================

input_file <- paste0(
  "D:/mint/data_xlsx/",
  "merged_screen_tte.xlsx"
)

output_dir <- "D:/mint/results"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  "descriptive_statistics_by_intervention.xlsx"
)


# =========================
# 2. 変数設定
# =========================

id_var <- "users_id"
group_var <- "intervention"

age_var <- "age_corrected"
g3_var <- "G3"
income_var <- "H4_P1"
two_word_original_var <- "C4-4"

af_vars <- paste0("AF", 1:6)
asq_vars <- paste0("R", 1:6)

screen_cont_var <- "childscreen24M"
screen_binary_var <- "screen_time_1h"
low_income_var <- "low_income"
two_word_var <- "2gobun"
asq_total_var <- "ASQ3_communication"

required_columns <- c(
  id_var,
  group_var,
  age_var,
  g3_var,
  income_var,
  two_word_original_var,
  af_vars,
  asq_vars
)


# =========================
# 3. データ読み込み
# =========================

df <- read_excel(input_file)

names(df) <- trimws(names(df))

cat("===== データ読み込み =====\n")
cat("行数:", nrow(df), "\n")
cat("列数:", ncol(df), "\n")


missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "以下の列がデータにありません: ",
      paste(missing_columns, collapse = ", ")
    )
  )
}


# =========================
# 4. users_idの確認
# =========================

if (any(is.na(df[[id_var]]))) {
  warning(
    paste0(
      "users_idが欠損している行が",
      sum(is.na(df[[id_var]])),
      "行あります。"
    )
  )
}

duplicated_ids <- unique(
  df[[id_var]][
    duplicated(df[[id_var]]) &
      !is.na(df[[id_var]])
  ]
)

if (length(duplicated_ids) > 0) {
  cat("重複しているusers_idの例:\n")
  print(head(duplicated_ids, 20))
  
  stop(
    "users_idが重複しているため処理を停止しました。"
  )
}


# =========================
# 5. 数値型への変換
# =========================

numeric_vars <- c(
  group_var,
  age_var,
  g3_var,
  income_var,
  two_word_original_var,
  af_vars,
  asq_vars
)

for (var in numeric_vars) {
  df[[var]] <- suppressWarnings(
    as.numeric(df[[var]])
  )
}


# =========================
# 6. interventionの確認
# =========================

invalid_intervention <- unique(
  df[[group_var]][
    !is.na(df[[group_var]]) &
      !(df[[group_var]] %in% c(0, 1))
  ]
)

if (length(invalid_intervention) > 0) {
  stop(
    paste0(
      "interventionに0または1以外の値があります: ",
      paste(invalid_intervention, collapse = ", ")
    )
  )
}

if (any(is.na(df[[group_var]]))) {
  stop(
    paste0(
      "interventionが欠損している参加者が",
      sum(is.na(df[[group_var]])),
      "人います。"
    )
  )
}

if (!all(c(0, 1) %in% unique(df[[group_var]]))) {
  stop(
    paste0(
      "intervention = 0とintervention = 1の",
      "両方の群が必要です。"
    )
  )
}


# =========================
# 7. AF1～AF6を時間に変換
# =========================
# 1 = 0時間
# 2 = 0.5時間
# 3 = 1.5時間
# 4 = 2.5時間
# 5 = 4時間
# 6 = 6時間
# 7 = 7時間

to_hours <- function(x) {
  case_when(
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

for (v in af_vars) {
  df[[paste0(v, "_h")]] <- to_hours(df[[v]])
}


# =========================
# 8. childscreen24Mを作成
# =========================
# 平日：AF1 + AF3 + AF5
# 休日：AF2 + AF4 + AF6
#
# 週間平均：
# ((平日合計 × 5) + (休日合計 × 2)) / 7
#
# AF1～AF6のどれか1つでも欠損ならNA

df[[screen_cont_var]] <- ifelse(
  is.na(df$AF1_h) |
    is.na(df$AF2_h) |
    is.na(df$AF3_h) |
    is.na(df$AF4_h) |
    is.na(df$AF5_h) |
    is.na(df$AF6_h),
  NA_real_,
  (
    (df$AF1_h + df$AF3_h + df$AF5_h) * 5 +
      (df$AF2_h + df$AF4_h + df$AF6_h) * 2
  ) / 7
)


# =========================
# 9. screen_time_1hを作成
# =========================
# 0 = 1時間未満/日
# 1 = 1時間以上/日

df[[screen_binary_var]] <- case_when(
  is.na(df[[screen_cont_var]]) ~ NA_real_,
  df[[screen_cont_var]] < 1 ~ 0,
  df[[screen_cont_var]] >= 1 ~ 1,
  TRUE ~ NA_real_
)


# =========================
# 10. 2gobunを作成
# =========================
# C4-4 = 1  -> 0
# C4-4 = 2  -> 1
# C4-4 = NA -> NA

df[[two_word_var]] <- case_when(
  is.na(df[[two_word_original_var]]) ~ NA_real_,
  df[[two_word_original_var]] == 1 ~ 0,
  df[[two_word_original_var]] == 2 ~ 1,
  TRUE ~ NA_real_
)


# =========================
# 11. low_incomeを作成
# =========================
# 1 = H4_P1が1～4
# 0 = H4_P1が5以上

df[[low_income_var]] <- case_when(
  is.na(df[[income_var]]) ~ NA_real_,
  df[[income_var]] >= 1 &
    df[[income_var]] <= 4 ~ 1,
  df[[income_var]] >= 5 ~ 0,
  TRUE ~ NA_real_
)


# =========================
# 12. ASQ-3 Communication scoreを作成
# =========================
#
# Original coding:
#   1 = Yes
#   2 = Sometimes
#   3 = Not Yet
#
# ASQ-3 scoring:
#   Yes       = 10
#   Sometimes = 5
#   Not Yet   = 0
#
# Missing-item rule:
#   0 missing   -> sum of 6 items
#   1-2 missing -> mean of answered items × 6
#   >=3 missing -> NA
#
# これはMICEによる欠損補完ではなく、
# ASQ-3固有の採点規則である。

for (v in asq_vars) {
  
  df[[paste0(v, "_score")]] <- case_when(
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

# 欠損項目数
df$ASQ3_communication_n_missing <- rowSums(
  is.na(df[asq_score_vars])
)

# 回答済み項目数
df$ASQ3_communication_n_answered <-
  length(asq_score_vars) -
  df$ASQ3_communication_n_missing

# ASQ-3 Communication score
df[[asq_total_var]] <- ifelse(
  df$ASQ3_communication_n_missing <= 2,
  rowMeans(
    df[asq_score_vars],
    na.rm = TRUE
  ) * 6,
  NA_real_
)

# NaNが生じた場合はNAに変換
df[[asq_total_var]][
  is.nan(df[[asq_total_var]])
] <- NA_real_


# =========================
# 13. 作成変数の確認
# =========================

cat("\n===== intervention =====\n")
print(
  table(
    df[[group_var]],
    useNA = "ifany"
  )
)

cat("\n===== childscreen24M =====\n")
print(
  summary(df[[screen_cont_var]])
)

cat("\n===== screen_time_1h =====\n")
print(
  table(
    df[[screen_binary_var]],
    useNA = "ifany"
  )
)

cat("\n===== low_income =====\n")
print(
  table(
    df[[low_income_var]],
    useNA = "ifany"
  )
)

cat("\n===== ASQ3_communication =====\n")
print(
  summary(df[[asq_total_var]])
)

cat("\n===== ASQ-3 missing-item count =====\n")
print(
  table(
    df$ASQ3_communication_n_missing,
    useNA = "ifany"
  )
)

cat("\n===== 2gobun =====\n")
print(
  table(
    df[[two_word_var]],
    useNA = "ifany"
  )
)


# =========================
# 14. 記述統計の表示関数
# =========================

format_value <- function(
    x,
    digits = 2
) {
  
  if (is.na(x)) {
    return("NA")
  }
  
  formatC(
    x,
    format = "f",
    digits = digits
  )
}


# 平均（SD）
format_mean_sd <- function(
    x,
    digits = 2
) {
  
  observed <- x[!is.na(x)]
  n_observed <- length(observed)
  
  if (n_observed == 0) {
    return("NA")
  }
  
  mean_value <- mean(observed)
  
  sd_value <- if (n_observed >= 2) {
    sd(observed)
  } else {
    NA_real_
  }
  
  paste0(
    format_value(mean_value, digits),
    " (",
    format_value(sd_value, digits),
    ")"
  )
}


# 欠損人数（欠損率）
# 分母は各群の全人数
format_missing <- function(
    x,
    total_n,
    digits = 1
) {
  
  n_missing <- sum(is.na(x))
  
  if (total_n == 0) {
    return(
      paste0(n_missing, " (NA)")
    )
  }
  
  missing_percent <- 100 * n_missing / total_n
  
  paste0(
    n_missing,
    " (",
    formatC(
      missing_percent,
      format = "f",
      digits = digits
    ),
    "%)"
  )
}


# 該当人数（割合）
# 分母は当該変数が非欠損の人数
format_event <- function(
    x,
    event_value = 1,
    digits = 1
) {
  
  observed <- x[!is.na(x)]
  n_observed <- length(observed)
  
  if (n_observed == 0) {
    return("0 (NA)")
  }
  
  n_event <- sum(
    observed == event_value
  )
  
  event_percent <- 100 * n_event / n_observed
  
  paste0(
    n_event,
    " (",
    formatC(
      event_percent,
      format = "f",
      digits = digits
    ),
    "%)"
  )
}


# =========================
# 15. p値の表示関数
# =========================

format_p_value <- function(p) {
  
  if (is.na(p)) {
    return(NA_character_)
  }
  
  if (p < 0.001) {
    return("<0.001")
  }
  
  formatC(
    p,
    format = "f",
    digits = 3
  )
}


# =========================
# 16. 連続変数の検定関数
# =========================
# Welchのt検定

test_continuous <- function(
    data,
    variable,
    variable_label
) {
  
  test_data <- data %>%
    select(
      all_of(c(group_var, variable))
    ) %>%
    filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[variable]])
    )
  
  intervention_values <- test_data[[variable]][
    test_data[[group_var]] == 1
  ]
  
  control_values <- test_data[[variable]][
    test_data[[group_var]] == 0
  ]
  
  n_intervention <- length(intervention_values)
  n_control <- length(control_values)
  
  mean_intervention <- if (n_intervention > 0) {
    mean(intervention_values)
  } else {
    NA_real_
  }
  
  mean_control <- if (n_control > 0) {
    mean(control_values)
  } else {
    NA_real_
  }
  
  if (
    n_intervention < 2 ||
    n_control < 2
  ) {
    return(
      data.frame(
        Variable = variable_label,
        Test = "Welch's t-test",
        N_intervention = n_intervention,
        N_control = n_control,
        Intervention_value = mean_intervention,
        Control_value = mean_control,
        Value_type = "Mean",
        P_value_numeric = NA_real_,
        P_value = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  temp_data <- data.frame(
    value = test_data[[variable]],
    group = factor(
      test_data[[group_var]],
      levels = c(0, 1),
      labels = c("Control", "Intervention")
    )
  )
  
  test_result <- tryCatch(
    {
      t.test(
        value ~ group,
        data = temp_data,
        var.equal = FALSE
      )
    },
    error = function(e) {
      NULL
    }
  )
  
  p_value <- if (is.null(test_result)) {
    NA_real_
  } else {
    test_result$p.value
  }
  
  data.frame(
    Variable = variable_label,
    Test = "Welch's t-test",
    N_intervention = n_intervention,
    N_control = n_control,
    Intervention_value = mean_intervention,
    Control_value = mean_control,
    Value_type = "Mean",
    P_value_numeric = p_value,
    P_value = format_p_value(p_value),
    stringsAsFactors = FALSE
  )
}


# =========================
# 17. 二値変数の検定関数
# =========================

test_binary <- function(
    data,
    variable,
    variable_label
) {
  
  test_data <- data %>%
    select(
      all_of(c(group_var, variable))
    ) %>%
    filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[variable]])
    )
  
  n_intervention <- sum(
    test_data[[group_var]] == 1
  )
  
  n_control <- sum(
    test_data[[group_var]] == 0
  )
  
  event_intervention <- sum(
    test_data[[group_var]] == 1 &
      test_data[[variable]] == 1
  )
  
  event_control <- sum(
    test_data[[group_var]] == 0 &
      test_data[[variable]] == 1
  )
  
  contingency_table <- table(
    factor(
      test_data[[group_var]],
      levels = c(0, 1)
    ),
    factor(
      test_data[[variable]],
      levels = c(0, 1)
    )
  )
  
  rownames(contingency_table) <- c(
    "Control",
    "Intervention"
  )
  
  colnames(contingency_table) <- c(
    "Value_0",
    "Value_1"
  )
  
  if (any(rowSums(contingency_table) == 0)) {
    return(
      data.frame(
        Variable = variable_label,
        Test = "Not estimable",
        N_intervention = n_intervention,
        N_control = n_control,
        Intervention_value = event_intervention,
        Control_value = event_control,
        Value_type = "Events",
        P_value_numeric = NA_real_,
        P_value = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  if (sum(colSums(contingency_table) > 0) < 2) {
    return(
      data.frame(
        Variable = variable_label,
        Test = "Not estimable: no variation",
        N_intervention = n_intervention,
        N_control = n_control,
        Intervention_value = event_intervention,
        Control_value = event_control,
        Value_type = "Events",
        P_value_numeric = NA_real_,
        P_value = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  
  chi_result <- suppressWarnings(
    chisq.test(
      contingency_table,
      correct = FALSE
    )
  )
  
  if (
    any(is.na(chi_result$expected)) ||
    any(chi_result$expected < 5)
  ) {
    
    test_result <- fisher.test(
      contingency_table
    )
    
    test_name <- "Fisher's exact test"
    p_value <- test_result$p.value
    
  } else {
    
    test_name <- "Pearson's chi-squared test"
    p_value <- chi_result$p.value
  }
  
  data.frame(
    Variable = variable_label,
    Test = test_name,
    N_intervention = n_intervention,
    N_control = n_control,
    Intervention_value = event_intervention,
    Control_value = event_control,
    Value_type = "Events",
    P_value_numeric = p_value,
    P_value = format_p_value(p_value),
    stringsAsFactors = FALSE
  )
}


# =========================
# 18. Table 1に含める変数
# =========================

table_specs <- data.frame(
  variable = c(
    age_var,
    screen_cont_var,
    screen_binary_var,
    g3_var,
    low_income_var,
    asq_total_var,
    two_word_var
  ),
  
  label = c(
    "Maternal age, years",
    
    paste0(
      "Average daily screen time at ",
      "24 months, hours/day"
    ),
    
    "Screen time >=1 hour/day",
    
    "G3",
    
    "Low household income (H4_P1 = 1-4)",
    
    paste0(
      "ASQ-3 Communication score, ",
      "0-60"
    ),
    
    paste0(
      "Two-word sentence not yet acquired ",
      "(2gobun = 1)"
    )
  ),
  
  variable_type = c(
    "continuous",
    "continuous",
    "binary",
    "continuous",
    "binary",
    "continuous",
    "binary"
  ),
  
  stringsAsFactors = FALSE
)


# =========================
# 19. Table 1の行を作成
# =========================

row_labels <- data.frame(
  Variable = "Participants",
  Statistic = "N",
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(table_specs))) {
  
  statistic_label <- if (
    table_specs$variable_type[i] == "continuous"
  ) {
    "Mean (SD)"
  } else {
    "n (%)"
  }
  
  new_rows <- data.frame(
    Variable = c(
      table_specs$label[i],
      table_specs$label[i]
    ),
    
    Statistic = c(
      statistic_label,
      "Missing, n (%)"
    ),
    
    stringsAsFactors = FALSE
  )
  
  row_labels <- bind_rows(
    row_labels,
    new_rows
  )
}


# =========================
# 20. 各群の記述統計
# =========================

make_summary_column <- function(data) {
  
  total_n <- nrow(data)
  
  output <- as.character(total_n)
  
  for (i in seq_len(nrow(table_specs))) {
    
    variable <- table_specs$variable[i]
    variable_type <- table_specs$variable_type[i]
    
    if (variable_type == "continuous") {
      
      main_value <- format_mean_sd(
        data[[variable]],
        digits = 2
      )
      
    } else {
      
      main_value <- format_event(
        data[[variable]],
        event_value = 1,
        digits = 1
      )
    }
    
    missing_value <- format_missing(
      data[[variable]],
      total_n = total_n,
      digits = 1
    )
    
    output <- c(
      output,
      main_value,
      missing_value
    )
  }
  
  output
}


# =========================
# 21. 群別データ
# =========================

df_overall <- df

df_intervention <- df %>%
  filter(
    .data[[group_var]] == 1
  )

df_control <- df %>%
  filter(
    .data[[group_var]] == 0
  )


# =========================
# 22. 群間検定
# =========================

test_results_list <- vector(
  mode = "list",
  length = nrow(table_specs)
)

for (i in seq_len(nrow(table_specs))) {
  
  if (
    table_specs$variable_type[i] == "continuous"
  ) {
    
    test_results_list[[i]] <- test_continuous(
      data = df,
      variable = table_specs$variable[i],
      variable_label = table_specs$label[i]
    )
    
  } else {
    
    test_results_list[[i]] <- test_binary(
      data = df,
      variable = table_specs$variable[i],
      variable_label = table_specs$label[i]
    )
  }
}

statistical_tests <- bind_rows(
  test_results_list
)


# =========================
# 23. Table 1を作成
# =========================

table1_final <- row_labels

table1_final$Overall <- make_summary_column(
  df_overall
)

table1_final$Intervention <- make_summary_column(
  df_intervention
)

table1_final$Control <- make_summary_column(
  df_control
)


# 各変数の代表行のみにp値・検定方法を表示
p_value_column <- NA_character_
test_column <- NA_character_

for (i in seq_len(nrow(statistical_tests))) {
  
  p_value_column <- c(
    p_value_column,
    statistical_tests$P_value[i],
    NA_character_
  )
  
  test_column <- c(
    test_column,
    statistical_tests$Test[i],
    NA_character_
  )
}

table1_final$P_value <- p_value_column
table1_final$Test <- test_column

table1_final <- table1_final %>%
  select(
    Variable,
    Statistic,
    Overall,
    Intervention,
    Control,
    P_value,
    Test
  )


# =========================
# 24. 人数確認
# =========================

sample_size_check <- data.frame(
  Item = c(
    "N in original data",
    "N in Overall",
    "N in Intervention group",
    "N in Control group",
    "Missing intervention",
    "Unique users_id"
  ),
  
  Value = c(
    nrow(df),
    nrow(df_overall),
    nrow(df_intervention),
    nrow(df_control),
    sum(is.na(df[[group_var]])),
    length(unique(df[[id_var]]))
  ),
  
  stringsAsFactors = FALSE
)


# =========================
# 25. 群別人数
# =========================

group_counts <- df %>%
  mutate(
    Group = case_when(
      .data[[group_var]] == 1 ~ "Intervention",
      .data[[group_var]] == 0 ~ "Control",
      TRUE ~ "Missing"
    )
  ) %>%
  count(
    Group,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  as.data.frame()


# =========================
# 26. 二値変数の分布
# =========================

make_binary_distribution <- function(
    data,
    variable,
    variable_label
) {
  
  data %>%
    mutate(
      Group = case_when(
        .data[[group_var]] == 1 ~ "Intervention",
        .data[[group_var]] == 0 ~ "Control",
        TRUE ~ "Missing intervention"
      ),
      
      Value = case_when(
        is.na(.data[[variable]]) ~ "Missing",
        .data[[variable]] == 0 ~ "0",
        .data[[variable]] == 1 ~ "1",
        TRUE ~ "Invalid"
      )
    ) %>%
    count(
      Group,
      Value,
      name = "N"
    ) %>%
    mutate(
      Variable = variable_label
    ) %>%
    select(
      Variable,
      Group,
      Value,
      N
    ) %>%
    as.data.frame()
}

binary_distributions <- bind_rows(
  make_binary_distribution(
    df,
    screen_binary_var,
    "screen_time_1h"
  ),
  
  make_binary_distribution(
    df,
    low_income_var,
    "low_income"
  ),
  
  make_binary_distribution(
    df,
    two_word_var,
    "2gobun"
  )
)


# =========================
# 27. ASQ-3欠損項目数の分布
# =========================

asq_missing_distribution <- df %>%
  mutate(
    Group = case_when(
      .data[[group_var]] == 1 ~ "Intervention",
      .data[[group_var]] == 0 ~ "Control",
      TRUE ~ "Missing intervention"
    )
  ) %>%
  count(
    Group,
    ASQ3_communication_n_missing,
    name = "N"
  ) %>%
  arrange(
    Group,
    ASQ3_communication_n_missing
  ) %>%
  as.data.frame()


# =========================
# 28. 変数定義
# =========================

codebook <- data.frame(
  Variable = c(
    "intervention",
    "childscreen24M",
    "screen_time_1h",
    "low_income",
    "ASQ3_communication",
    "2gobun"
  ),
  
  Definition = c(
    "1 = Intervention group; 0 = Control group",
    
    paste0(
      "Average daily screen time calculated ",
      "from AF1-AF6"
    ),
    
    "1 = >=1 hour/day; 0 = <1 hour/day",
    
    paste0(
      "1 = H4_P1 values 1-4; ",
      "0 = H4_P1 value >=5"
    ),
    
    paste0(
      "R1-R6: Yes = 10, Sometimes = 5, Not Yet = 0. ",
      "For 1-2 missing items, mean of answered items x 6. ",
      "For >=3 missing items, score = missing."
    ),
    
    paste0(
      "C4-4 = 1 -> 0; ",
      "C4-4 = 2 -> 1; ",
      "missing -> missing"
    )
  ),
  
  stringsAsFactors = FALSE
)


# =========================
# 29. 解析方法の注記
# =========================

analysis_notes <- data.frame(
  Item = c(
    "General missing-data handling",
    "ASQ-3 missing-item handling",
    "Continuous-variable summary",
    "Continuous-variable comparison",
    "Binary-variable percentage denominator",
    "Binary-variable comparison",
    "Missing percentage denominator",
    "P value threshold"
  ),
  
  Description = c(
    "MICE or other general missing-value imputation was not performed.",
    
    paste0(
      "ASQ-3 scoring followed its item-level rule: ",
      "0 missing items = sum; 1-2 missing items = ",
      "mean of answered items multiplied by 6; ",
      "3 or more missing items = missing total score."
    ),
    
    paste0(
      "Mean and standard deviation were calculated ",
      "using observed values."
    ),
    
    paste0(
      "Intervention and control groups were compared ",
      "using Welch's t-test."
    ),
    
    paste0(
      "The denominator was the number of participants ",
      "with non-missing values for the variable."
    ),
    
    paste0(
      "Pearson's chi-squared test was used unless ",
      "any expected cell count was below 5, ",
      "in which case Fisher's exact test was used."
    ),
    
    paste0(
      "The denominator for missing percentages was ",
      "the total number of participants in each group."
    ),
    
    "Two-sided P <0.05 was considered statistically significant."
  ),
  
  stringsAsFactors = FALSE
)


# =========================
# 30. Excelファイルを作成
# =========================

wb <- createWorkbook()

sheet_data <- list(
  Table1 = table1_final,
  statistical_tests = statistical_tests,
  sample_size_check = sample_size_check,
  group_counts = group_counts,
  binary_distributions = binary_distributions,
  ASQ3_missing_items = asq_missing_distribution,
  codebook = codebook,
  analysis_notes = analysis_notes
)

for (sheet_name in names(sheet_data)) {
  
  addWorksheet(
    wb,
    sheet_name
  )
  
  writeData(
    wb,
    sheet = sheet_name,
    x = sheet_data[[sheet_name]]
  )
}


# =========================
# 31. Excelの書式設定
# =========================

header_style <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#4472C4",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

variable_style <- createStyle(
  textDecoration = "bold",
  fgFill = "#D9EAF7"
)

significant_style <- createStyle(
  fontColour = "#C00000",
  textDecoration = "bold"
)


# すべてのシートのヘッダー
for (sheet_name in names(sheet_data)) {
  
  current_data <- sheet_data[[sheet_name]]
  
  addStyle(
    wb,
    sheet = sheet_name,
    style = header_style,
    rows = 1,
    cols = seq_len(ncol(current_data)),
    gridExpand = TRUE
  )
  
  freezePane(
    wb,
    sheet = sheet_name,
    firstRow = TRUE
  )
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(current_data)),
    widths = "auto"
  )
}


# Table 1で各変数の開始行を強調
primary_data_rows <- 2 + 2 * (
  seq_len(nrow(table_specs)) - 1
)

variable_start_excel_rows <- c(
  2,
  primary_data_rows + 1
)

addStyle(
  wb,
  sheet = "Table1",
  style = variable_style,
  rows = variable_start_excel_rows,
  cols = 1,
  gridExpand = TRUE,
  stack = TRUE
)


# p <0.05を赤字・太字にする
significant_test_numbers <- which(
  !is.na(statistical_tests$P_value_numeric) &
    statistical_tests$P_value_numeric < 0.05
)

if (length(significant_test_numbers) > 0) {
  
  significant_excel_rows <-
    primary_data_rows[
      significant_test_numbers
    ] + 1
  
  addStyle(
    wb,
    sheet = "Table1",
    style = significant_style,
    rows = significant_excel_rows,
    cols = which(
      names(table1_final) == "P_value"
    ),
    gridExpand = TRUE,
    stack = TRUE
  )
}


# Table 1の列幅
setColWidths(
  wb,
  sheet = "Table1",
  cols = 1,
  widths = 52
)

setColWidths(
  wb,
  sheet = "Table1",
  cols = 2,
  widths = 20
)

setColWidths(
  wb,
  sheet = "Table1",
  cols = 3:5,
  widths = 22
)

setColWidths(
  wb,
  sheet = "Table1",
  cols = 6,
  widths = 12
)

setColWidths(
  wb,
  sheet = "Table1",
  cols = 7,
  widths = 28
)

setColWidths(
  wb,
  sheet = "codebook",
  cols = 2,
  widths = 75
)

setColWidths(
  wb,
  sheet = "analysis_notes",
  cols = 2,
  widths = 75
)


# =========================
# 32. Excelに保存
# =========================

saveWorkbook(
  wb,
  file = output_file,
  overwrite = TRUE
)


# =========================
# 33. 結果表示
# =========================

cat("\n===== Sample size check =====\n")
print(sample_size_check)

cat("\n===== Group counts =====\n")
print(group_counts)

cat("\n===== Statistical tests =====\n")
print(statistical_tests)

cat("\n===== Table 1 =====\n")
print(table1_final)

cat("\nExcelファイルを保存しました:\n")
cat(output_file, "\n")

cat("\n処理が完了しました。\n")

# =========================
# 34. 24か月時点のスクリーンタイム分布と群別割合（Figure A + B）
# =========================
# A: Early Partnership群とHistorical control群を合わせたヒストグラム
#     0.5時間幅、6時間以上は「6+」としてまとめる。
#     紺色 = <1 hour/day、オレンジ色 = >=1 hour/day。
# B: 各群で「1時間以下 (<=1 hour/day)」だった子どもの割合。
#     分母は各群のchildscreen24Mが非欠損の人数。
#     この図は観測値の記述統計であり、IPW等による重み付けを行わない。
#
# 注意: 元のscreen_time_1hは「<1 vs >=1」の分類です。
#       ここでは依頼どおり、Bでは「<=1」を集計します。
#       論文のアウトカムと一致させたい場合は下記の
#       include_exactly_oneをFALSEにしてください。

# 必要な場合のみ一度実行:
# install.packages(c("ggplot2", "patchwork"))
for (pkg in c("ggplot2", "patchwork")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0(
      "Figureの作成には", pkg, "が必要です。",
      "install.packages('", pkg, "')を実行してください。"
    ))
  }
}

include_exactly_one <- FALSE  # TRUE: <=1時間 / FALSE: <1時間

# 解析可能な観測値（欠損・無限大・負値を除外）
screen_plot_raw <- df %>%
  transmute(
    Group = factor(
      .data[[group_var]],
      levels = c(1, 0),
      labels = c("Early Partnership", "Historical control")
    ),
    screen_hours = .data[[screen_cont_var]]
  ) %>%
  filter(!is.na(Group), is.finite(screen_hours), screen_hours >= 0)

if (nrow(screen_plot_raw) == 0) {
  stop("描画可能なchildscreen24Mのデータがありません。")
}

# ---------
# A. 2群合算ヒストグラム（元Figureの色分けと階級を維持）
# ---------
histogram_data <- screen_plot_raw %>%
  mutate(
    # [0,0.5), [0.5,1), ..., [5.5,6), [6,+Inf)
    bin_left = pmin(floor(screen_hours / 0.5) * 0.5, 6),
    Screen_time = factor(
      if_else(bin_left < 1, "<1 hour/day", "\u22651 hour/day"),
      levels = c("<1 hour/day", "\u22651 hour/day")
    )
  ) %>%
  count(bin_left, Screen_time, name = "N")

figure_a <- ggplot2::ggplot(
  histogram_data,
  ggplot2::aes(x = bin_left + 0.25, y = N, fill = Screen_time)
) +
  ggplot2::geom_col(
    width = 0.5, colour = "white", linewidth = 0.35
  ) +
  ggplot2::geom_vline(
    xintercept = 1, linetype = "dashed", colour = "#333333", linewidth = 0.55
  ) +
  ggplot2::scale_fill_manual(
    values = c("<1 hour/day" = "#090579", "\u22651 hour/day" = "#FF9C00"),
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = 0:6,
    labels = c("0", "1", "2", "3", "4", "5", "6+"),
    limits = c(0, 6.5),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 40),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = "A  Distribution of screen time",
    x = "Average daily screen time at 24 months, hours/day",
    y = "Number of children",
    fill = NULL
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 13, margin = ggplot2::margin(b = 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = ggplot2::element_text(size = 10),
    legend.key.width = grid::unit(0.7, "lines"),
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    plot.margin = ggplot2::margin(8, 10, 8, 8)
  )

# ---------
# B. 介入群・対照群での「1時間以下」の割合
# ---------
bar_data <- screen_plot_raw %>%
  group_by(Group, .drop = FALSE) %>%
  summarise(
    n_valid = n(),
    n_at_most_one = sum(
      if (include_exactly_one) screen_hours <= 1 else screen_hours < 1
    ),
    n_exactly_one = sum(abs(screen_hours - 1) < 1e-9),
    .groups = "drop"
  ) %>%
  mutate(
    percent = if_else(n_valid > 0, 100 * n_at_most_one / n_valid, NA_real_),
    label = if_else(
      n_valid > 0,
      sprintf("%.1f%%", percent),
      "No data"
    )
  )

bar_title <- if (include_exactly_one) {
  "B  Screen time \u22641 hour/day"
} else {
  "B  Screen time <1 hour/day"
}

figure_b <- 
  ggplot2::ggplot(
    bar_data,
    ggplot2::aes(x = Group, y = percent, fill = Group)
  ) +
    ggplot2::geom_col(
      width = 0.62, na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Early Partnership" = "#F57B70",
        "Historical control" = "#22BDC3"
      ),
      guide = "none"
    ) +
  ggplot2::geom_text(
    ggplot2::aes(label = label, y = percent + 1.5),
    size = 3.6, na.rm = TRUE
  ) +
  ggplot2::scale_x_discrete(
    labels = c("Early Partnership" = "Early\nPartnership",
               "Historical control" = "Historical\ncontrol")
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 30, 10),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = bar_title,
    x = NULL,
    y = "Percentage of children (%)"
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 13, margin = ggplot2::margin(b = 8)),
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    plot.margin = ggplot2::margin(8, 8, 8, 18)
  )

# ---------
# Aを広め（およそ65%）、Bを狭め（およそ35%）で左右に配置
# ---------
screen_time_figure <- (figure_a + figure_b) +
  patchwork::plot_layout(widths = c(1.8, 1))

# RStudioのPlotsにも表示
print(screen_time_figure)

# 高解像度PNGとベクターPDFを保存
figure_png <- file.path(output_dir, "screen_time_distribution_and_proportion.png")
figure_pdf <- file.path(output_dir, "screen_time_distribution_and_proportion.pdf")

ggplot2::ggsave(
  filename = figure_png,
  plot = screen_time_figure,
  width = 13, height = 5.6, units = "in", dpi = 300, bg = "white"
)

ggplot2::ggsave(
  filename = figure_pdf,
  plot = screen_time_figure,
  width = 13, height = 5.6, units = "in", bg = "white"
)

cat("\n===== 1時間以下の割合（観測値、非欠損者を分母） =====\n")
print(bar_data)
cat("\nスクリーンタイムのFigureを保存しました:\n")
cat(figure_png, "\n")
cat(figure_pdf, "\n")


