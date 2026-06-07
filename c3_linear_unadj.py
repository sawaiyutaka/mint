import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル読み込み
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/results/simple_linear_regression_results_lowincome_motheredu_reversed.xlsx"

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

# mother_education_6grp もカテゴリ変数として扱う
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

timepoints = ["P1", "P2", "1m", "6m", "12m", "18m"]

mediators = [
    f"G{i}_{tp}"
    for tp in timepoints
    for i in range(1, 6)
]

# A13_P1 は連続変数
exposures_continuous = [
    "A13_P1",
]

# カテゴリ変数
# それぞれ最も n が大きいカテゴリを基準にする
exposures_categorical = [
    "low_income_baseline",
    "mother_education_6grp",
]

# =========================
# 4. 必要な列が存在するか確認
# =========================

all_required_columns = (
    [outcome] +
    mediators +
    exposures_continuous +
    exposures_categorical
)

missing_columns = [col for col in all_required_columns if col not in df.columns]

if missing_columns:
    print("以下の列はデータに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

mediators = [m for m in mediators if m in df.columns]
exposures_continuous = [x for x in exposures_continuous if x in df.columns]
exposures_categorical = [x for x in exposures_categorical if x in df.columns]

# =========================
# 5. 型の整理
# =========================

numeric_columns = [outcome] + mediators + exposures_continuous

for col in numeric_columns:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

for col in exposures_categorical:
    if col in df.columns:
        df[col] = df[col].astype("category")

# =========================
# 5-1. reverse score の作成
# =========================
# 解釈しやすくするため、
# ポジティブな要素を 0 からマイナスして使用する。
#
# G3_12m, G3_18m: 経済的ゆとり
# A13_P1: 母親の年齢
#
# 例:
#   元の G3_12m が 5 の場合 → -5
#   元の A13_P1 が 30 の場合 → -30
#
# 以後の回帰では、元の列名のまま reverse 後の値を使う。

reverse_score_vars = [
    "G3_12m",
    "G3_18m",
    "A13_P1",
]

for col in reverse_score_vars:
    if col in df.columns:
        df[col] = 0 - df[col]
        print(f"{col} を reverse score に変換しました: 0 - {col}")
    else:
        print(f"{col} はデータに存在しないため、reverse score 変換をスキップしました。")

# =========================
# 6. 単回帰を実行する関数
# =========================

def run_simple_lm(data, y, x, x_is_categorical=False, model_type_label=""):
    if y not in data.columns or x not in data.columns:
        return None

    use_df = data[[y, x]].dropna().copy()

    if len(use_df) < 3:
        return None

    if use_df[y].nunique(dropna=True) < 2:
        return None

    if use_df[x].nunique(dropna=True) < 2:
        return None

    reference_category = None

    if x_is_categorical:
        use_df[x] = use_df[x].astype("category")

        category_counts = use_df[x].value_counts(dropna=True)

        if category_counts.empty:
            return None

        reference_category = category_counts.idxmax()

        # ★ここが重要：numpy型をPython標準型に変換する
        if hasattr(reference_category, "item"):
            reference_category_for_formula = reference_category.item()
        else:
            reference_category_for_formula = reference_category

        formula_x = (
            f"C({x}, Treatment(reference={repr(reference_category_for_formula)}))"
        )

    else:
        formula_x = x

    formula = f"{y} ~ {formula_x}"

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

        rows.append({
            "model_type": model_type_label,
            "outcome": y,
            "exposure": x,
            "term": term,
            "n": int(model.nobs),
            "reference_category": reference_category,
            "beta": params[term],
            "std_error": ses[term],
            "ci_lower": conf.loc[term, 0],
            "ci_upper": conf.loc[term, 1],
            "p_value": pvalues[term],
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
#      AF3 ~ G1_●● ～ G5_●●
# ---------------------------------

for mediator in mediators:
    res = run_simple_lm(
        data=df,
        y=outcome,
        x=mediator,
        x_is_categorical=False,
        model_type_label="mediator_to_outcome"
    )
    if res is not None:
        results.append(res)

# ---------------------------------
# 7-2. 曝露因子 → 媒介因子
#      G●_●● ~ A13_P1
#      G●_●● ~ low_income_baseline
#      G●_●● ~ mother_education_6grp
# ---------------------------------

for mediator in mediators:

    for exposure in exposures_continuous:
        res = run_simple_lm(
            data=df,
            y=mediator,
            x=exposure,
            x_is_categorical=False,
            model_type_label="exposure_to_mediator"
        )
        if res is not None:
            results.append(res)

    for exposure in exposures_categorical:
        res = run_simple_lm(
            data=df,
            y=mediator,
            x=exposure,
            x_is_categorical=True,
            model_type_label="exposure_to_mediator"
        )
        if res is not None:
            results.append(res)

# ---------------------------------
# 7-3. 曝露因子 → アウトカム
#      AF3 ~ A13_P1
#      AF3 ~ low_income_baseline
#      AF3 ~ mother_education_6grp
# ---------------------------------

for exposure in exposures_continuous:
    res = run_simple_lm(
        data=df,
        y=outcome,
        x=exposure,
        x_is_categorical=False,
        model_type_label="exposure_to_outcome"
    )
    if res is not None:
        results.append(res)

for exposure in exposures_categorical:
    res = run_simple_lm(
        data=df,
        y=outcome,
        x=exposure,
        x_is_categorical=True,
        model_type_label="exposure_to_outcome"
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

    for col in ["beta", "std_error", "ci_lower", "ci_upper", "p_value", "r_squared"]:
        regression_results_rounded[col] = regression_results_rounded[col].round(4)

    print("===== 回帰結果 =====")
    print(regression_results_rounded)

else:
    regression_results_rounded = regression_results
    print("回帰結果が作成されませんでした。変数名や欠損を確認してください。")

# =========================
# 9. Excelに保存
# =========================

with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
    regression_results_rounded.to_excel(
        writer,
        sheet_name="all_results",
        index=False
    )

    if not regression_results_rounded.empty:
        regression_results_rounded[
            regression_results_rounded["model_type"] == "mediator_to_outcome"
        ].to_excel(
            writer,
            sheet_name="mediator_to_outcome",
            index=False
        )

        regression_results_rounded[
            regression_results_rounded["model_type"] == "exposure_to_mediator"
        ].to_excel(
            writer,
            sheet_name="exposure_to_mediator",
            index=False
        )

        regression_results_rounded[
            regression_results_rounded["model_type"] == "exposure_to_outcome"
        ].to_excel(
            writer,
            sheet_name="exposure_to_outcome",
            index=False
        )

print(f"完了: {output_file} を作成しました")
