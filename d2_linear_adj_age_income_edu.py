import os
import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル読み込み
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/results/adjusted_linear_regression_results_lowincome_motheredu.xlsx"

os.makedirs(os.path.dirname(output_file), exist_ok=True)

df = pd.read_excel(input_file)

df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 2. 変数作成
# =========================

# low_income_baseline:
# H4_P1が1〜4   → 2
# H4_P1が5〜8   → 1
# H4_P1が9〜15  → 0
# H4_P1が欠損   → NaN

if "H4_P1" not in df.columns:
    raise ValueError("H4_P1 がデータに存在しません。")

df["H4_P1"] = pd.to_numeric(df["H4_P1"], errors="coerce")

df["low_income_baseline"] = pd.NA

df.loc[df["H4_P1"].between(1, 4), "low_income_baseline"] = 2
df.loc[df["H4_P1"].between(5, 8), "low_income_baseline"] = 1
df.loc[df["H4_P1"].between(9, 15), "low_income_baseline"] = 0

df["low_income_baseline"] = df["low_income_baseline"].astype("category")

print("===== low_income_baseline の分布 =====")
print(df["low_income_baseline"].value_counts(dropna=False))

if "mother_education_6grp" in df.columns:
    df["mother_education_6grp"] = df["mother_education_6grp"].astype("category")

    print("===== mother_education_6grp の分布 =====")
    print(df["mother_education_6grp"].value_counts(dropna=False))
else:
    print("mother_education_6grp はデータに存在しませんでした。")

# =========================
# 3. 変数指定
# =========================

outcome = "AF3"

exposures = [
    "low_income_baseline",
    "mother_education_6grp",
    "A13_P1",
]

mediators = [
    "G3_12m",
    "G3_18m",
]

base_covariates = [
    "G3_P1",
    "WHO5_all_100_P1",
]

categorical_vars = [
    "low_income_baseline",
    "mother_education_6grp",
]

continuous_vars = [
    "A13_P1",
    "G3_P1",
    "WHO5_all_100_P1",
    "G3_12m",
    "G3_18m",
    "AF3",
]

# =========================
# 4. 必要な列が存在するか確認
# =========================

all_required_columns = (
    [outcome] +
    exposures +
    mediators +
    base_covariates
)

missing_columns = [col for col in all_required_columns if col not in df.columns]

if missing_columns:
    print("以下の列はデータに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

exposures = [x for x in exposures if x in df.columns]
mediators = [m for m in mediators if m in df.columns]
base_covariates = [c for c in base_covariates if c in df.columns]
categorical_vars = [c for c in categorical_vars if c in df.columns]
continuous_vars = [c for c in continuous_vars if c in df.columns]

# =========================
# 5. 型の整理
# =========================

for col in continuous_vars:
    df[col] = pd.to_numeric(df[col], errors="coerce")

for col in categorical_vars:
    df[col] = df[col].astype("category")

# =========================
# 6. 補助関数
# =========================

def to_python_scalar(value):
    """
    numpy型をPython標準型に変換する。
    np.int64(0) や np.float64(0.0) が formula に入ると
    NameError: name 'np' is not defined
    になるため、その対策。
    """
    if hasattr(value, "item"):
        return value.item()
    return value


def make_formula_term(var, use_df, categorical_vars):
    """
    変数がカテゴリ変数なら、n最大カテゴリを基準にしたC()項を作る。
    連続変数ならそのまま返す。
    """
    if var in categorical_vars:
        use_df[var] = use_df[var].astype("category")

        category_counts = use_df[var].value_counts(dropna=True)

        if category_counts.empty:
            return None, None

        reference_category = category_counts.idxmax()
        reference_category = to_python_scalar(reference_category)

        term = f"C({var}, Treatment(reference={repr(reference_category)}))"

        return term, reference_category

    else:
        return var, None


def identify_term_variable(term, variables):
    """
    statsmodelsの出力termが、どの変数に対応しているかを判定する。
    """
    for var in variables:
        if term == var:
            return var
        if term.startswith(f"C({var},"):
            return var
    return None

# =========================
# 7. 調整済み回帰を実行する関数
# =========================

def run_adjusted_lm(data, y, main_x, covariates, categorical_vars, model_type_label=""):
    required_cols = [y, main_x] + covariates
    required_cols = list(dict.fromkeys(required_cols))

    missing = [col for col in required_cols if col not in data.columns]
    if missing:
        print(f"スキップ: {y} ~ {main_x} に必要な列がありません: {missing}")
        return None

    use_df = data[required_cols].dropna().copy()

    if len(use_df) < 3:
        return None

    if use_df[y].nunique(dropna=True) < 2:
        return None

    if use_df[main_x].nunique(dropna=True) < 2:
        return None

    # 共変量のうち、値が1種類しかないものはモデルから除外
    valid_covariates = []
    dropped_covariates = []

    for cov in covariates:
        if use_df[cov].nunique(dropna=True) >= 2:
            valid_covariates.append(cov)
        else:
            dropped_covariates.append(cov)

    model_vars = [main_x] + valid_covariates

    formula_terms = []
    reference_map = {}

    for var in model_vars:
        term, ref = make_formula_term(var, use_df, categorical_vars)

        if term is None:
            return None

        formula_terms.append(term)

        if ref is not None:
            reference_map[var] = ref

    formula = f"{y} ~ " + " + ".join(formula_terms)

    try:
        model = smf.ols(formula=formula, data=use_df).fit()
    except Exception as e:
        print(f"モデル実行エラー: {formula}")
        print(e)
        return None

    conf = model.conf_int()
    params = model.params
    pvalues = model.pvalues
    ses = model.bse

    rows = []

    for term in params.index:
        if term == "Intercept":
            continue

        term_variable = identify_term_variable(term, model_vars)

        if term_variable == main_x:
            term_role = "main_predictor"
        else:
            term_role = "covariate"

        rows.append({
            "model_type": model_type_label,
            "outcome": y,
            "main_predictor": main_x,
            "term": term,
            "term_variable": term_variable,
            "term_role": term_role,
            "n": int(model.nobs),
            "reference_category": reference_map.get(term_variable, None),
            "covariates": ", ".join(valid_covariates),
            "dropped_covariates": ", ".join(dropped_covariates),
            "beta": params[term],
            "std_error": ses[term],
            "ci_lower": conf.loc[term, 0],
            "ci_upper": conf.loc[term, 1],
            "p_value": pvalues[term],
            "r_squared": model.rsquared,
            "adj_r_squared": model.rsquared_adj,
            "formula": formula,
        })

    return pd.DataFrame(rows)

# =========================
# 8. 調整済み回帰を一括実行
# =========================

results = []

# ---------------------------------
# 8-1. 曝露因子 → 媒介因子
#
# G3_12m ~ 選択した曝露因子
#          + 選ばなかった曝露因子
#          + G3_P1
#          + WHO5_all_100_P1
#
# G3_18m も同様
# ---------------------------------

for mediator in mediators:
    for exposure in exposures:

        covariates = (
            [x for x in exposures if x != exposure] +
            base_covariates
        )

        res = run_adjusted_lm(
            data=df,
            y=mediator,
            main_x=exposure,
            covariates=covariates,
            categorical_vars=categorical_vars,
            model_type_label="adjusted_exposure_to_mediator"
        )

        if res is not None:
            results.append(res)

# ---------------------------------
# 8-2. 曝露因子 → アウトカム
#
# AF3 ~ 選択した曝露因子
#       + 選ばなかった曝露因子
#       + G3_P1
#       + WHO5_all_100_P1
# ---------------------------------

for exposure in exposures:

    covariates = (
        [x for x in exposures if x != exposure] +
        base_covariates
    )

    res = run_adjusted_lm(
        data=df,
        y=outcome,
        main_x=exposure,
        covariates=covariates,
        categorical_vars=categorical_vars,
        model_type_label="adjusted_exposure_to_outcome"
    )

    if res is not None:
        results.append(res)

# ---------------------------------
# 8-3. 媒介因子 → アウトカム
#
# AF3 ~ G3_12m
#       + 全曝露因子
#       + G3_P1
#       + WHO5_all_100_P1
#
# AF3 ~ G3_18m も同様
# ---------------------------------

for mediator in mediators:

    covariates = exposures + base_covariates

    res = run_adjusted_lm(
        data=df,
        y=outcome,
        main_x=mediator,
        covariates=covariates,
        categorical_vars=categorical_vars,
        model_type_label="adjusted_mediator_to_outcome"
    )

    if res is not None:
        results.append(res)

# =========================
# 9. 結果をまとめる
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
        "adj_r_squared",
    ]:
        regression_results_rounded[col] = regression_results_rounded[col].round(4)

    print("===== 調整済み回帰結果 =====")
    print(regression_results_rounded)

else:
    regression_results_rounded = regression_results
    print("回帰結果が作成されませんでした。変数名や欠損を確認してください。")

# =========================
# 10. Excelに保存
# =========================

with pd.ExcelWriter(output_file, engine="openpyxl") as writer:

    regression_results_rounded.to_excel(
        writer,
        sheet_name="all_results",
        index=False
    )

    if not regression_results_rounded.empty:

        regression_results_rounded[
            regression_results_rounded["model_type"] == "adjusted_exposure_to_mediator"
        ].to_excel(
            writer,
            sheet_name="exposure_to_mediator",
            index=False
        )

        regression_results_rounded[
            regression_results_rounded["model_type"] == "adjusted_exposure_to_outcome"
        ].to_excel(
            writer,
            sheet_name="exposure_to_outcome",
            index=False
        )

        regression_results_rounded[
            regression_results_rounded["model_type"] == "adjusted_mediator_to_outcome"
        ].to_excel(
            writer,
            sheet_name="mediator_to_outcome",
            index=False
        )

        # 主説明変数の係数だけを抜き出したシート
        regression_results_rounded[
            regression_results_rounded["term_role"] == "main_predictor"
        ].to_excel(
            writer,
            sheet_name="main_predictor_only",
            index=False
        )

print(f"完了: {output_file} を作成しました")
