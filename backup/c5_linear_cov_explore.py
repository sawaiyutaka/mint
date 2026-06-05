import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル読み込み
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/data_xlsx/adjusted_linear_regression_covariate_patterns.xlsx"

df = pd.read_excel(input_file)
df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 2. low_income_baseline を作成
# =========================
# H4_P1が1〜4  → 1
# H4_P1が5〜15 → 0
# H4_P1が欠損 → NaN

if "H4_P1" not in df.columns:
    raise ValueError("H4_P1 がデータに存在しません。")

df["H4_P1"] = pd.to_numeric(df["H4_P1"], errors="coerce")

df["low_income_baseline"] = pd.NA
df.loc[df["H4_P1"].between(1, 4), "low_income_baseline"] = 1
df.loc[df["H4_P1"].between(5, 15), "low_income_baseline"] = 0

df["low_income_baseline"] = df["low_income_baseline"].astype("category")

print("===== low_income_baseline の分布 =====")
print(df["low_income_baseline"].value_counts(dropna=False))

# =========================
# 3. 使用する変数を指定
# =========================

outcome = "AF3"

exposures = [
    "low_income_baseline",
    "A13_P1",
]

mediators = [
    "G3_12m",
    "G3_18m",
]

# 共変量パターン
covariate_patterns = {
    "cov_G3_P1": {
        "continuous": ["G3_P1"],
        "categorical": [],
    },
    "cov_WHO5_P1": {
        "continuous": ["WHO5_all_100_P1"],
        "categorical": [],
    },
    "cov_mother_education": {
        "continuous": [],
        "categorical": ["mother_education_6grp"],
    },
}

# カテゴリー変数として扱う変数
base_categorical_vars = [
    "low_income_baseline",
    "mother_education_6grp",
]

# =========================
# 4. 必要な列が存在するか確認
# =========================

all_covariates = []
for pattern in covariate_patterns.values():
    all_covariates += pattern["continuous"]
    all_covariates += pattern["categorical"]

required_columns = (
    [outcome] +
    exposures +
    mediators +
    all_covariates
)

required_columns = list(dict.fromkeys(required_columns))

missing_columns = [col for col in required_columns if col not in df.columns]

if missing_columns:
    print("以下の列はデータに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

# 存在する列だけに限定
exposures = [x for x in exposures if x in df.columns]
mediators = [m for m in mediators if m in df.columns]

for pattern_name, pattern in covariate_patterns.items():
    pattern["continuous"] = [
        c for c in pattern["continuous"] if c in df.columns
    ]
    pattern["categorical"] = [
        c for c in pattern["categorical"] if c in df.columns
    ]

# =========================
# 5. 型の整理
# =========================

continuous_vars = [
    outcome,
    "A13_P1",
] + mediators

for pattern in covariate_patterns.values():
    continuous_vars += pattern["continuous"]

continuous_vars = list(dict.fromkeys(continuous_vars))

for col in continuous_vars:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

for col in base_categorical_vars:
    if col in df.columns:
        df[col] = df[col].astype("category")

# =========================
# 6. 回帰実行用の関数
# =========================

def make_term(var, categorical_vars_for_formula):
    """
    カテゴリー変数は C(var) としてformulaに入れる。
    """
    if var in categorical_vars_for_formula:
        return f"C({var})"
    else:
        return var


def run_lm(
    data,
    y,
    main_x,
    covariates,
    model_type_label,
    covariate_pattern,
    categorical_vars_for_formula
):
    """
    y ~ main_x + covariates の線形回帰を実行する。
    """

    model_vars = [y, main_x] + covariates
    model_vars = list(dict.fromkeys(model_vars))

    missing_model_vars = [v for v in model_vars if v not in data.columns]

    if missing_model_vars:
        print(f"モデルで使う変数が不足しています: {y} ~ {main_x}")
        print(missing_model_vars)
        return None

    # モデルごとの完全ケース解析
    use_df = data[model_vars].dropna().copy()

    if len(use_df) < 5:
        print(
            f"解析対象者が少なすぎるためスキップ: "
            f"{covariate_pattern}: {y} ~ {main_x}, n={len(use_df)}"
        )
        return None

    if use_df[y].nunique(dropna=True) < 2:
        print(f"アウトカムの値が1種類しかないためスキップ: {y}")
        return None

    if use_df[main_x].nunique(dropna=True) < 2:
        print(f"主説明変数の値が1種類しかないためスキップ: {main_x}")
        return None

    rhs_terms = (
        [make_term(main_x, categorical_vars_for_formula)] +
        [make_term(c, categorical_vars_for_formula) for c in covariates]
    )

    rhs_terms = list(dict.fromkeys(rhs_terms))

    formula = f"{y} ~ " + " + ".join(rhs_terms)

    try:
        model = smf.ols(formula=formula, data=use_df).fit()
    except Exception as e:
        print(f"モデル実行エラー: {formula}")
        print(e)
        return None

    conf = model.conf_int()

    rows = []

    for term in model.params.index:
        if term == "Intercept":
            continue

        if main_x in categorical_vars_for_formula:
            is_main_term = term.startswith(f"C({main_x})")
        else:
            is_main_term = term == main_x

        rows.append({
            "covariate_pattern": covariate_pattern,
            "model_type": model_type_label,
            "outcome": y,
            "main_exposure": main_x,
            "term": term,
            "is_main_term": is_main_term,
            "n": int(model.nobs),
            "beta": model.params[term],
            "std_error": model.bse[term],
            "ci_lower": conf.loc[term, 0],
            "ci_upper": conf.loc[term, 1],
            "p_value": model.pvalues[term],
            "r_squared": model.rsquared,
            "formula": formula,
        })

    return pd.DataFrame(rows)


# =========================
# 7. 1つの共変量パターンで全モデルを実行
# =========================

def run_all_models_for_pattern(
    data,
    outcome_var,
    exposure_vars,
    mediator_vars,
    covariates_continuous,
    covariates_categorical,
    covariate_pattern
):
    results = []

    covariates_base = covariates_continuous + covariates_categorical

    categorical_vars_for_formula = [
        "low_income_baseline"
    ] + covariates_categorical

    # ---------------------------------
    # 7-1. 媒介因子 → アウトカム
    #      AF3 ~ G3_12m / G3_18m + low_income_baseline + A13_P1 + 共変量
    # ---------------------------------

    for mediator in mediator_vars:

        covariates = (
            exposure_vars +
            covariates_base
        )

        res = run_lm(
            data=data,
            y=outcome_var,
            main_x=mediator,
            covariates=covariates,
            model_type_label="mediator_to_outcome_adjusted",
            covariate_pattern=covariate_pattern,
            categorical_vars_for_formula=categorical_vars_for_formula
        )

        if res is not None:
            results.append(res)

    # ---------------------------------
    # 7-2. 曝露因子 → 媒介因子
    #      G3_12m / G3_18m ~ 曝露因子 + 選択していない曝露因子 + 共変量
    # ---------------------------------

    for mediator in mediator_vars:
        for exposure in exposure_vars:

            other_exposures = [x for x in exposure_vars if x != exposure]

            covariates = (
                other_exposures +
                covariates_base
            )

            res = run_lm(
                data=data,
                y=mediator,
                main_x=exposure,
                covariates=covariates,
                model_type_label="exposure_to_mediator_adjusted",
                covariate_pattern=covariate_pattern,
                categorical_vars_for_formula=categorical_vars_for_formula
            )

            if res is not None:
                results.append(res)

    # ---------------------------------
    # 7-3. 曝露因子 → アウトカム
    #      AF3 ~ 曝露因子 + 選択していない曝露因子 + 共変量
    # ---------------------------------

    for exposure in exposure_vars:

        other_exposures = [x for x in exposure_vars if x != exposure]

        covariates = (
            other_exposures +
            covariates_base
        )

        res = run_lm(
            data=data,
            y=outcome_var,
            main_x=exposure,
            covariates=covariates,
            model_type_label="exposure_to_outcome_adjusted",
            covariate_pattern=covariate_pattern,
            categorical_vars_for_formula=categorical_vars_for_formula
        )

        if res is not None:
            results.append(res)

    if results:
        return pd.concat(results, ignore_index=True)
    else:
        return pd.DataFrame()


# =========================
# 8. 全パターンを実行
# =========================

all_results_list = []

for pattern_name, pattern in covariate_patterns.items():

    print(f"===== 解析中: {pattern_name} =====")

    res = run_all_models_for_pattern(
        data=df,
        outcome_var=outcome,
        exposure_vars=exposures,
        mediator_vars=mediators,
        covariates_continuous=pattern["continuous"],
        covariates_categorical=pattern["categorical"],
        covariate_pattern=pattern_name
    )

    if not res.empty:
        all_results_list.append(res)

if all_results_list:
    all_results = pd.concat(all_results_list, ignore_index=True)
else:
    all_results = pd.DataFrame()

# =========================
# 9. 結果を整える
# =========================

if not all_results.empty:
    all_results_rounded = all_results.copy()

    for col in [
        "beta",
        "std_error",
        "ci_lower",
        "ci_upper",
        "p_value",
        "r_squared",
    ]:
        all_results_rounded[col] = all_results_rounded[col].round(4)

    main_results = all_results_rounded[
        all_results_rounded["is_main_term"] == True
    ].copy()

else:
    all_results_rounded = all_results
    main_results = all_results
    print("回帰結果が作成されませんでした。変数名や欠損を確認してください。")

print("===== 主説明変数の結果 =====")
print(main_results)

# =========================
# 10. Excelに保存
# =========================

with pd.ExcelWriter(output_file, engine="openpyxl") as writer:

    all_results_rounded.to_excel(
        writer,
        sheet_name="all_coefficients",
        index=False
    )

    main_results.to_excel(
        writer,
        sheet_name="main_results",
        index=False
    )

    if not all_results_rounded.empty:
        for pattern_name in covariate_patterns.keys():
            tmp = all_results_rounded[
                all_results_rounded["covariate_pattern"] == pattern_name
            ]

            if not tmp.empty:
                tmp.to_excel(
                    writer,
                    sheet_name=pattern_name[:31],
                    index=False
                )

print(f"完了: {output_file} を作成しました")