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

# H4_P1 が 1〜4 → 1
df.loc[df["H4_P1"].between(1, 4), "low_income_baseline"] = 1

# H4_P1 が 5〜15 → 0
df.loc[df["H4_P1"].between(5, 15), "low_income_baseline"] = 0

# カテゴリ変数として扱う
df["low_income_baseline"] = df["low_income_baseline"].astype("category")

print("===== low_income_baseline の分布 =====")
print(df["low_income_baseline"].value_counts(dropna=False))

# =========================
# 3. 変数指定
# =========================

outcome = "AF3"

exposures_continuous = [
    "A13_P1",
]

exposures_categorical = [
    "low_income_baseline",
]

mediators = [
    "G3_12m",
    "G3_18m",
]

baseline_covariates = [
    "G3_P1",
    "WHO5_all_100_P1",
]

# =========================
# 4. 必要な列が存在するか確認
# =========================

all_required_columns = (
    [outcome] +
    exposures_continuous +
    exposures_categorical +
    mediators +
    baseline_covariates
)

missing_columns = [col for col in all_required_columns if col not in df.columns]

if missing_columns:
    print("以下の列はデータに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

# 存在する列だけに絞る
mediators = [m for m in mediators if m in df.columns]
exposures_continuous = [x for x in exposures_continuous if x in df.columns]
exposures_categorical = [x for x in exposures_categorical if x in df.columns]
baseline_covariates = [x for x in baseline_covariates if x in df.columns]

# =========================
# 5. 型の整理
# =========================

numeric_columns = (
    [outcome] +
    exposures_continuous +
    mediators +
    baseline_covariates
)

for col in numeric_columns:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")

for col in exposures_categorical:
    if col in df.columns:
        df[col] = df[col].astype("category")

# =========================
# 6. 回帰を実行する関数
# =========================

def make_formula(y, main_exposure, covariates, categorical_vars=None):
    """
    formula文字列を作成する。
    categorical_vars に含まれる変数は C() で囲む。
    """
    if categorical_vars is None:
        categorical_vars = []

    terms = []

    all_xs = [main_exposure] + covariates

    for x in all_xs:
        if x in categorical_vars:
            terms.append(f"C({x})")
        else:
            terms.append(x)

    formula = f"{y} ~ " + " + ".join(terms)

    return formula


def extract_main_exposure_results(model, y, main_exposure, model_type_label, formula):
    """
    モデル結果から、main_exposureに対応する係数だけを抽出する。
    カテゴリ変数の場合は C(main_exposure) で始まる項を抽出する。
    """
    conf = model.conf_int()
    rows = []

    for term in model.params.index:
        if term == "Intercept":
            continue

        is_main_term = False

        if term == main_exposure:
            is_main_term = True

        if term.startswith(f"C({main_exposure})"):
            is_main_term = True

        if not is_main_term:
            continue

        rows.append({
            "model_type": model_type_label,
            "outcome": y,
            "main_exposure": main_exposure,
            "term": term,
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


def run_adjusted_lm(
    data,
    y,
    main_exposure,
    covariates,
    categorical_vars=None,
    model_type_label=""
):
    """
    共変量調整ありの線形回帰を実行し、
    main_exposureの結果だけを返す。
    """
    if categorical_vars is None:
        categorical_vars = []

    use_columns = [y, main_exposure] + covariates
    use_columns = list(dict.fromkeys(use_columns))

    missing = [col for col in use_columns if col not in data.columns]
    if missing:
        print(f"スキップ: {y} ~ {main_exposure} で列が不足しています: {missing}")
        return None

    use_df = data[use_columns].dropna().copy()

    if len(use_df) < 3:
        print(f"スキップ: {y} ~ {main_exposure} は有効データが少なすぎます。n={len(use_df)}")
        return None

    if use_df[y].nunique(dropna=True) < 2:
        print(f"スキップ: {y} の値にばらつきがありません。")
        return None

    if use_df[main_exposure].nunique(dropna=True) < 2:
        print(f"スキップ: {main_exposure} の値にばらつきがありません。")
        return None

    formula = make_formula(
        y=y,
        main_exposure=main_exposure,
        covariates=covariates,
        categorical_vars=categorical_vars
    )

    try:
        model = smf.ols(formula=formula, data=use_df).fit()
    except Exception as e:
        print(f"モデル実行エラー: {formula}")
        print(e)
        return None

    result = extract_main_exposure_results(
        model=model,
        y=y,
        main_exposure=main_exposure,
        model_type_label=model_type_label,
        formula=formula
    )

    return result


# =========================
# 7. 回帰を一括実行
# =========================

results = []

categorical_vars = exposures_categorical

# ---------------------------------
# 7-1. 媒介因子 → アウトカム
#      AF3 ~ G3_12m + G3_P1 + WHO5_all_100_P1
#      AF3 ~ G3_18m + G3_P1 + WHO5_all_100_P1
# ---------------------------------

for mediator in mediators:
    covariates = baseline_covariates.copy()

    res = run_adjusted_lm(
        data=df,
        y=outcome,
        main_exposure=mediator,
        covariates=covariates,
        categorical_vars=categorical_vars,
        model_type_label="mediator_to_outcome"
    )

    if res is not None and not res.empty:
        results.append(res)

# ---------------------------------
# 7-2. 曝露因子 → 媒介因子
#      G3_12m / G3_18m ~ exposure + もう一方のexposure + G3_P1 + WHO5_all_100_P1
# ---------------------------------

all_exposures = exposures_continuous + exposures_categorical

for mediator in mediators:
    for exposure in all_exposures:

        # 選択していないほうの曝露因子を共変量に入れる
        other_exposures = [x for x in all_exposures if x != exposure]

        covariates = other_exposures + baseline_covariates

        res = run_adjusted_lm(
            data=df,
            y=mediator,
            main_exposure=exposure,
            covariates=covariates,
            categorical_vars=categorical_vars,
            model_type_label="exposure_to_mediator"
        )

        if res is not None and not res.empty:
            results.append(res)

# ---------------------------------
# 7-3. 曝露因子 → アウトカム
#      AF3 ~ exposure + もう一方のexposure + G3_P1 + WHO5_all_100_P1
# ---------------------------------

for exposure in all_exposures:

    # 選択していないほうの曝露因子を共変量に入れる
    other_exposures = [x for x in all_exposures if x != exposure]

    covariates = other_exposures + baseline_covariates

    res = run_adjusted_lm(
        data=df,
        y=outcome,
        main_exposure=exposure,
        covariates=covariates,
        categorical_vars=categorical_vars,
        model_type_label="exposure_to_outcome"
    )

    if res is not None and not res.empty:
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
