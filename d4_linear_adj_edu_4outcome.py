import os
import pandas as pd
import statsmodels.formula.api as smf

# =========================
# 1. ファイル設定
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected_age_corrected.xlsx"
output_file = r"D:/mint/results/linear_regression_edu_AF3_4grp.xlsx"

os.makedirs(os.path.dirname(output_file), exist_ok=True)

# =========================
# 2. データ読み込み
# =========================
df = pd.read_excel(input_file)
df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 3. アウトカムの再分類
# =========================
# 元の AF3:
# 1 = None
# 2 = Less than 1 hour
# 3 = 1 to less than 2 hours
# 4 = 2 to less than 3 hours
# 5 = 3 to less than 5 hours
# 6 = 5 to less than 7 hours
# 7 = 7 hours or more
#
# 新しい AF3_4grp:
# 1 = None
# 2 = Less than 1 hour
# 3 = 1 to less than 2 hours
# 4 = 2 hours or more

if "AF3" not in df.columns:
    raise ValueError("AF3 がデータに存在しません。")

df["AF3"] = pd.to_numeric(df["AF3"], errors="coerce")

df["AF3_4grp"] = df["AF3"].copy()
df.loc[df["AF3"].between(4, 7), "AF3_4grp"] = 4

print("===== AF3 の分布 =====")
print(df["AF3"].value_counts(dropna=False).sort_index())

print("===== AF3_4grp の分布 =====")
print(df["AF3_4grp"].value_counts(dropna=False).sort_index())

# =========================
# 4. 使用する変数
# =========================
outcome = "AF3_4grp"                # 24か月のスクリーンタイム：4カテゴリ版
exposure = "G3"                     # 経済的ゆとり
education = "mother_education_6grp" # 母親学歴
covariate = "age_corrected"         # 母親年齢

required_columns = [
    outcome,
    exposure,
    education,
    covariate,
]

missing_columns = [col for col in required_columns if col not in df.columns]
if missing_columns:
    raise ValueError(f"以下の列がデータに存在しません: {missing_columns}")

# =========================
# 5. 型の整理
# =========================
for col in [outcome, exposure, covariate]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

df[education] = df[education].astype("category")

# =========================
# 6. reverse score
# =========================
# G3: 経済的ゆとりがないほど大きい値にする
# age_corrected: 母親年齢が若いほど大きい値にする

for col in [exposure, covariate]:
    df[col] = 0 - df[col]
    print(f"{col} を reverse score に変換しました: 0 - {col}")

# =========================
# 7. 回帰分析
# =========================
analysis_df = df[required_columns].dropna().copy()

formula = f"{outcome} ~ {exposure} + C({education}) + {covariate}"

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
