import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル読み込み
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/data_xlsx/adjusted_linear_regression_results.xlsx"

df = pd.read_excel(input_file)
df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 2. low_income_baseline を作成
# =========================

if "H4_P1" not in df.columns:
    raise ValueError("H4_P1 がデータに存在しません。")

df["H4_P1"] = pd.to_numeric(df["H4_P1"], errors="coerce")

df["low_income_baseline"] = pd.NA
df.loc[df["H4_P1"].between(1, 4), "low_income_baseline"] = 1
df.loc[df["H4_P1"].between(5, 15), "low_income_baseline"] = 0

# カテゴリー変数として扱う
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

numeric_columns = (
    [outcome] +
    mediators +
    ["A13_P1"] +
    base_covariates_continuous
)

for col in numeric_columns:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

categorical_columns = [
    "low_income_baseline"
] + base_covariates_categorical

for col in categorical_columns:
    if col in df.columns:
        df[col] = df[col].astype("category")

# =========================
# 6. 回帰実行用の関数
# =========================

def make_term(var):
    """
    カテゴリー変数は C(var) としてformulaに入れる。
    """
    categorical_vars = [
        "low_income_baseline",
        "mother_education_6grp",
        "father_education_6grp",
    ]

    if var in categorical_vars:
        return f"C({var})"
    else:
        return var


def run_lm(data, y, main_x, covariates, model_type_label):
    """
    y ~ main_x + covariates の線形回帰を実行する。
    main_xがカテゴリー変数の場合はダミー変数化する。
    """

    model_vars = [y, main_x] + covariates
    model_vars = list(dict.fromkeys(model_vars))  # 重複削除

    existing_model_vars = [v for v in model_vars if v in data.columns]
    missing_model_vars = [v for v in model_vars if v not in data.columns]

    if missing_model_vars:
        print(f"モデルで使う変数が不足しています: {y} ~ {main_x}")
        print(missing_model_vars)
        return None

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

    rhs_terms = [make_term(main_x)] + [make_term(c) for c in covariates]
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

        # 主説明変数かどうかを判定
        if main_x in ["low_income_baseline", "mother_education_6grp", "father_education_6grp"]:
            is_main_term = term.startswith(f"C({main_x})")
        else:
            is_main_term = term == main_x

        rows.append({
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
# 7. 回帰を一括実行
# =========================

results = []

# ---------------------------------
# 7-1. 媒介因子 → アウトカム
#      AF3 ~ G3_12m / G3_18m + 共変量
# ---------------------------------
# ここでは、媒介因子とアウトカムの関連を見るため、
# low_income_baseline と A13_P1 の両方を共変量として入れます。

for mediator in mediators:

    covariates = (
        exposures +
        base_covariates_continuous +
        base_covariates_categorical
    )

    res = run_lm(
        data=df,
        y=outcome,
        main_x=mediator,
        covariates=covariates,
        model_type_label="mediator_to_outcome_adjusted"
    )

    if res is not None:
        results.append(res)

# ---------------------------------
# 7-2. 曝露因子 → 媒介因子
#      G3_12m / G3_18m ~ 曝露因子 + 選択していない曝露因子 + 共変量
# ---------------------------------

for mediator in mediators:
    for exposure in exposures:

        other_exposures = [x for x in exposures if x != exposure]

        covariates = (
            other_exposures +
            base_covariates_continuous +
            base_covariates_categorical
        )

        res = run_lm(
            data=df,
            y=mediator,
            main_x=exposure,
            covariates=covariates,
            model_type_label="exposure_to_mediator_adjusted"
        )

        if res is not None:
            results.append(res)

# ---------------------------------
# 7-3. 曝露因子 → アウトカム
#      AF3 ~ 曝露因子 + 選択していない曝露因子 + 共変量
# ---------------------------------

for exposure in exposures:

    other_exposures = [x for x in exposures if x != exposure]

    covariates = (
        other_exposures +
        base_covariates_continuous +
        base_covariates_categorical
    )

    res = run_lm(
        data=df,
        y=outcome,
        main_x=exposure,
        covariates=covariates,
        model_type_label="exposure_to_outcome_adjusted"
    )

    if res is not None:
        results.append(res)

# =========================
# 8. 結果をまとめる
# =========================

if results:
    regression_results = pd.concat(results, ignore_index=True)
else:
    regression_results = pd.DataFrame()

if not regression_results.empty:
    regression_results_rounded = regression_results.copy()

    for col in [
        "beta",
        "std_error",
        "ci_lower",
        "ci_upper",
        "p_value",
        "r_squared",
    ]:
        regression_results_rounded[col] = regression_results_rounded[col].round(4)

    # 主説明変数の結果だけを抜き出した表
    main_results = regression_results_rounded[
        regression_results_rounded["is_main_term"] == True
    ].copy()

    print("===== 主説明変数の回帰結果 =====")
    print(main_results)

else:
    regression_results_rounded = regression_results
    main_results = regression_results
    print("回帰結果が作成されませんでした。変数名や欠損を確認してください。")

# =========================
# 9. Excelに保存
# =========================

with pd.ExcelWriter(output_file, engine="openpyxl") as writer:

    regression_results_rounded.to_excel(
        writer,
        sheet_name="all_coefficients",
        index=False
    )

    main_results.to_excel(
        writer,
        sheet_name="main_results",
        index=False
    )

    if not regression_results_rounded.empty:
        regression_results_rounded[
            regression_results_rounded["model_type"] == "mediator_to_outcome_adjusted"
        ].to_excel(
            writer,
            sheet_name="mediator_to_outcome",
            index=False
        )

        regression_results_rounded[
            regression_results_rounded["model_type"] == "exposure_to_mediator_adjusted"
        ].to_excel(
            writer,
            sheet_name="exposure_to_mediator",
            index=False
        )

        regression_results_rounded[
            regression_results_rounded["model_type"] == "exposure_to_outcome_adjusted"
        ].to_excel(
            writer,
            sheet_name="exposure_to_outcome",
            index=False
        )

print(f"完了: {output_file} を作成しました")
