import os
import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル設定
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/results/linear_regression_income.xlsx"

os.makedirs(os.path.dirname(output_file), exist_ok=True)

# =========================
# 2. データ読み込み
# =========================
df = pd.read_excel(input_file)
df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 3. 使用する変数
# =========================
outcome = "AF3"                  # 24か月のスクリーンタイム
exposure = "G3_18m"              # 経済的ゆとり
income = "low_income_baseline"   # ベースライン世帯収入
covariate = "age_corrected"      # 母親年齢 ※必要に応じて修正

# low_income_baseline の作成元
income_source = "H4_P1"

required_columns = [
    outcome,
    exposure,
    covariate,
    income_source,
]

missing_columns = [col for col in required_columns if col not in df.columns]
if missing_columns:
    raise ValueError(f"以下の列がデータに存在しません: {missing_columns}")

# =========================
# 4. 型の整理
# =========================
for col in [outcome, exposure, covariate, income_source]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

# =========================
# 5. low_income_baseline を作成
# =========================
df[income] = pd.NA

df.loc[df[income_source].between(1, 4), income] = 2
df.loc[df[income_source].between(5, 8), income] = 1
df.loc[df[income_source].between(9, 15), income] = 0

df[income] = df[income].astype("category")

print(f"{income} を作成しました")
print(df[income].value_counts(dropna=False).sort_index())

# =========================
# 6. reverse score
# =========================
# G3_18m: 経済的ゆとりがないほど大きい値にする
# H4_P1: 母親年齢が若いほど大きい値にする

for col in [exposure, covariate]:
    df[col] = 0 - df[col]
    print(f"{col} を reverse score に変換しました: 0 - {col}")

# =========================
# 7. 回帰分析
# =========================
analysis_columns = [
    outcome,
    exposure,
    income,
    covariate,
]

analysis_df = df[analysis_columns].dropna().copy()

formula = f"{outcome} ~ {exposure} + C({income}) + {covariate}"

model = smf.ols(formula=formula, data=analysis_df).fit()

print("===== 回帰式 =====")
print(formula)

print("===== 回帰結果 =====")
print(model.summary())

# =========================
# 8. 結果を表にまとめる
# =========================
conf = model.conf_int()

results = pd.DataFrame({
    "term": model.params.index,
    "beta": model.params.values,
    "std_error": model.bse.values,
    "ci_lower": conf[0].values,
    "ci_upper": conf[1].values,
    "p_value": model.pvalues.values,
})

results = results[results["term"] != "Intercept"].copy()

results["n"] = int(model.nobs)
results["r_squared"] = model.rsquared
results["adj_r_squared"] = model.rsquared_adj
results["formula"] = formula

# 小数第4位で丸める
round_cols = [
    "beta",
    "std_error",
    "ci_lower",
    "ci_upper",
    "p_value",
    "r_squared",
    "adj_r_squared",
]

results[round_cols] = results[round_cols].round(4)

print("===== 保存用の結果 =====")
print(results)

# =========================
# 9. Excelに保存
# =========================
with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
    results.to_excel(
        writer,
        sheet_name="regression_results",
        index=False
    )

print(f"完了: {output_file} を作成しました")
