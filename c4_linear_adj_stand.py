import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル読み込み
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/data_xlsx/adjusted_linear_regression_results_with_standardized.xlsx"

df = pd.read_excel(input_file)
df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 2. low_income_baseline を作成
# =========================
# low_income_baseline:
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

base_covariates_continuous = [
    "G3_P1",
    "WHO5_all_100_P1",
]

base_covariates_categorical = [
    "mother_education_6grp",
    "father_education_6grp",
]

# =========================
# 4. 必要な列が存在するか確認
# =========================

required_columns = (
    [outcome] +
    exposures +
    mediators +
    base_covariates_continuous +
    base_covariates_categorical
)

missing_columns = [col for col in required_columns if col not in df.columns]

if missing_columns:
    print("以下の列はデータに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

# 存在する列だけに限定
exposures = [x for x in exposures if x in df.columns]
mediators = [m for m in mediators if m in df.columns]
base_covariates_continuous = [
    c for c in base_covariates_continuous if c in df.columns
]
base_covariates_categorical = [
    c for c in base_covariates_categorical if c in df.columns
]

# =========================
# 5. 型の整理
# =========================

continuous_vars = (
    [outcome] +
    [x for x in exposures if x != "low_income_baseline"] +
    mediators +
    base_covariates_continuous
)

continuous_vars = list(dict.fromkeys(continuous_vars))

for col in continuous_vars:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

categorical_vars = [
    "low_income_baseline"
] + base_covariates_categorical

for col in categorical_vars:
    if col in df.columns:
        df[col] = df[col].astype("category")

# =========================
# 6. 連続変数の標準化版を作成
# =========================
# カテゴリー変数は標準化しない
# low_income_baseline, mother_education_6grp, father_education_6grp はそのまま使う

for col in continuous_vars:
    if col in df.columns:
        mean = df[col].mean(skipna=True)
        sd = df[col].std(skipna=True)

        if pd.notna(sd) and sd != 0:
            df[f"z_{col}"] = (df[col] - mean) / sd
        else:
            df[f"z_{col}"] = pd.NA
            print(f"警告: {col} は標準偏差が0または欠損のため標準化できません。")

# =========================
# 7. 回帰実行用の関数
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
    coefficient_type,
    categorical_vars_for_formula
):
    """
    y ~ main_x + covariates の線形回帰を実行する。
    """

    model_vars = [y, main_x] + covariates
    model_vars = list(dict.fromkeys(model_vars))

    existing_model_vars = [v for v in model_vars if v in data.columns]
    missing_model_vars = [v for v in model_vars if v not in data.columns]

    if missing_model_vars:
        print(f"モデルで使う変数が不足しています: {y} ~ {main_x}")
        print(missing_model_vars)
        return None

    # 欠損はモデルごとに完全ケース解析
    use_df = data[existing_model_vars].dropna().copy()

    if len(use_df) < 5:
        print(f"解析対象者が少なすぎるためスキップ: {y} ~ {main_x}, n={len(use_df)}")
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
            "coefficient_type": coefficient_type,
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
# 8. 回帰を一括実行する関数
# =========================

def run_all_models(
    data,
    outcome_var,
    exposure_vars,
    mediator_vars,
    base_covariates_cont,
    base_covariates_cat,
    categorical_vars_for_formula,
    coefficient_type
):
    """
    以下の3種類のモデルをまとめて実行する。

    1. 媒介因子 → アウトカム
    2. 曝露因子 → 媒介因子
    3. 曝露因子 → アウトカム
    """

    results = []

    # ---------------------------------
    # 8-1. 媒介因子 → アウトカム
    #      AF3 ~ G3_12m / G3_18m + 共変量
    # ---------------------------------
    # 媒介因子とアウトカムの関連では、
    # low_income_baseline と A13_P1 の両方を共変量として入れる

    for mediator in mediator_vars:

        covariates = (
            exposure_vars +
            base_covariates_cont +
            base_covariates_cat
        )

        res = run_lm(
            data=data,
            y=outcome_var,
            main_x=mediator,
            covariates=covariates,
            model_type_label="mediator_to_outcome_adjusted",
            coefficient_type=coefficient_type,
            categorical_vars_for_formula=categorical_vars_for_formula
        )

        if res is not None:
            results.append(res)

    # ---------------------------------
    # 8-2. 曝露因子 → 媒介因子
    #      G3_12m / G3_18m ~ 曝露因子 + 選択していない曝露因子 + 共変量
    # ---------------------------------

    for mediator in mediator_vars:
        for exposure in exposure_vars:

            other_exposures = [x for x in exposure_vars if x != exposure]

            covariates = (
                other_exposures +
                base_covariates_cont +
                base_covariates_cat
            )

            res = run_lm(
                data=data,
                y=mediator,
                main_x=exposure,
                covariates=covariates,
                model_type_label="exposure_to_mediator_adjusted",
                coefficient_type=coefficient_type,
                categorical_vars_for_formula=categorical_vars_for_formula
            )

            if res is not None:
                results.append(res)

    # ---------------------------------
    # 8-3. 曝露因子 → アウトカム
    #      AF3 ~ 曝露因子 + 選択していない曝露因子 + 共変量
    # ---------------------------------

    for exposure in exposure_vars:

        other_exposures = [x for x in exposure_vars if x != exposure]

        covariates = (
            other_exposures +
            base_covariates_cont +
            base_covariates_cat
        )

        res = run_lm(
            data=data,
            y=outcome_var,
            main_x=exposure,
            covariates=covariates,
            model_type_label="exposure_to_outcome_adjusted",
            coefficient_type=coefficient_type,
            categorical_vars_for_formula=categorical_vars_for_formula
        )

        if res is not None:
            results.append(res)

    if results:
        all_results = pd.concat(results, ignore_index=True)
    else:
        all_results = pd.DataFrame()

    return all_results


# =========================
# 9. 非標準化モデル
# =========================

unstd_outcome = outcome

unstd_exposures = [
    "low_income_baseline",
    "A13_P1",
]

unstd_mediators = [
    "G3_12m",
    "G3_18m",
]

unstd_base_covariates_continuous = [
    "G3_P1",
    "WHO5_all_100_P1",
]

unstd_base_covariates_categorical = [
    "mother_education_6grp",
    "father_education_6grp",
]

unstd_categorical_vars_for_formula = [
    "low_income_baseline",
    "mother_education_6grp",
    "father_education_6grp",
]

unstd_results = run_all_models(
    data=df,
    outcome_var=unstd_outcome,
    exposure_vars=unstd_exposures,
    mediator_vars=unstd_mediators,
    base_covariates_cont=unstd_base_covariates_continuous,
    base_covariates_cat=unstd_base_covariates_categorical,
    categorical_vars_for_formula=unstd_categorical_vars_for_formula,
    coefficient_type="unstandardized"
)

# =========================
# 10. 標準化モデル
# =========================
# 連続変数だけ z_ 付きに差し替える
# カテゴリー変数はそのまま

std_outcome = "z_AF3"

std_exposures = [
    "low_income_baseline",
    "z_A13_P1",
]

std_mediators = [
    "z_G3_12m",
    "z_G3_18m",
]

std_base_covariates_continuous = [
    "z_G3_P1",
    "z_WHO5_all_100_P1",
]

std_base_covariates_categorical = [
    "mother_education_6grp",
    "father_education_6grp",
]

std_categorical_vars_for_formula = [
    "low_income_baseline",
    "mother_education_6grp",
    "father_education_6grp",
]

std_results = run_all_models(
    data=df,
    outcome_var=std_outcome,
    exposure_vars=std_exposures,
    mediator_vars=std_mediators,
    base_covariates_cont=std_base_covariates_continuous,
    base_covariates_cat=std_base_covariates_categorical,
    categorical_vars_for_formula=std_categorical_vars_for_formula,
    coefficient_type="standardized_continuous_vars"
)

# =========================
# 11. 結果を整える
# =========================

def format_results(results):
    if results.empty:
        return results, results

    results_rounded = results.copy()

    for col in [
        "beta",
        "std_error",
        "ci_lower",
        "ci_upper",
        "p_value",
        "r_squared",
    ]:
        results_rounded[col] = results_rounded[col].round(4)

    main_results = results_rounded[
        results_rounded["is_main_term"] == True
    ].copy()

    return results_rounded, main_results


unstd_results_rounded, unstd_main_results = format_results(unstd_results)
std_results_rounded, std_main_results = format_results(std_results)

print("===== 非標準化モデル：主説明変数の結果 =====")
print(unstd_main_results)

print("===== 標準化モデル：主説明変数の結果 =====")
print(std_main_results)

# =========================
# 12. Excelに保存
# =========================

with pd.ExcelWriter(output_file, engine="openpyxl") as writer:

    unstd_results_rounded.to_excel(
        writer,
        sheet_name="unstd_all_coefficients",
        index=False
    )

    unstd_main_results.to_excel(
        writer,
        sheet_name="unstd_main_results",
        index=False
    )

    std_results_rounded.to_excel(
        writer,
        sheet_name="std_all_coefficients",
        index=False
    )

    std_main_results.to_excel(
        writer,
        sheet_name="std_main_results",
        index=False
    )

print(f"完了: {output_file} を作成しました")