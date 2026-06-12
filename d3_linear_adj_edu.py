import os
import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル設定
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"
output_file = r"D:/mint/results/linear_regression_screen_time.xlsx"

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
outcome = "AF3"  # 24か月のスクリーンタイム

exposures = [
    "mother_education_6grp",  # 母親学歴
    "G3_12m",                 # 経済的ゆとり
]

covariates = [
    "H4_P1",                  # 母親年齢
]

categorical_vars = [
    "mother_education_6grp",
]

continuous_vars = [
    "AF3",
    "G3_12m",
    "H4_P1",
]

all_vars = [outcome] + exposures + covariates

# =========================
# 4. 変数の存在確認
# =========================
missing_cols = [col for col in all_vars if col not in df.columns]

if missing_cols:
    raise ValueError(f"以下の列がデータに存在しません: {missing_cols}")

# =========================
# 5. 型の整理
# =========================
for col in continuous_vars:
    df[col] = pd.to_numeric(df[col], errors="coerce")

for col in categorical_vars:
    df[col] = df[col].astype("category")

# =========================
# 6. 解析用データ作成
# =========================
analysis_df = df[all_vars].dropna().copy()

print(f"解析対象者数: {len(analysis_df)}")

# =========================
# 7. 回帰分析
# =========================
# AF3 ~ 母親学歴 + 経済的ゆとり + 母親年齢

formula = "AF3 ~ C(mother_education_6grp) + G3_12m + H4_P1"

model = smf.ols(formula=formula, data=analysis_df).fit()

print(model.summary())

# =========================
# 8. 結果を表に整理
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

results["n"] = int(model.nobs)
results["r_squared"] = model.rsquared
results["adj_r_squared"] = model.rsquared_adj
results["formula"] = formula

# Interceptを除外
results = results[results["term"] != "Intercept"].copy()

# 小数点以下4桁に丸める
round_cols = [
    "beta",
    "std_error",
    "ci_lower",
    "ci_upper",
    "p_value",
    "r_squared",
    "adj_r_squared",
]

for col in round_cols:
    results[col] = results[col].round(4)

print("===== 回帰分析結果 =====")
print(results)

# =========================
# 9. Excelに保存
# =========================
with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
    results.to_excel(writer, sheet_name="regression_results", index=False)

print(f"完了: {output_file} を作成しました")
