import os
import pandas as pd
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt

# =========================
# 1. ファイル設定
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected_age_corrected.xlsx"
output_file = r"D:/mint/results/linear_regression_edu_binary_interaction.xlsx"
graph_file = r"D:/mint/results/interaction_plot_edu_income_95CI.png"

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
outcome = "AF3"                              # 24か月のスクリーンタイム
exposure = "G3_18m"                          # 経済的ゆとり
education_original = "mother_education_6grp" # 元の母親学歴
education = "mother_education_binary"        # 2値化した母親学歴
covariate = "age_corrected"                  # 母親年齢

required_columns = [
    outcome,
    exposure,
    education_original,
    covariate,
]

missing_columns = [col for col in required_columns if col not in df.columns]
if missing_columns:
    raise ValueError(f"以下の列がデータに存在しません: {missing_columns}")

# =========================
# 4. 型の整理
# =========================
for col in [outcome, exposure, education_original, covariate]:
    df[col] = pd.to_numeric(df[col], errors="coerce")

# =========================
# 5. 母親学歴を2値化
# =========================
# mother_education_6grp:
# 2 -> 1
# 1 -> 0
# 0 -> 0

df[education] = pd.NA
df.loc[df[education_original] == 2, education] = 1
df.loc[df[education_original].isin([0, 1]), education] = 0

df[education] = pd.to_numeric(df[education], errors="coerce")

print("===== 母親学歴の2値化確認 =====")
print(df[[education_original, education]].value_counts(dropna=False).sort_index())

# =========================
# 6. reverse score
# =========================
# G3_18m: 経済的ゆとりがないほど大きい値にする
# age_corrected: 母親年齢が若いほど大きい値にする

for col in [exposure, covariate]:
    df[col] = 0 - df[col]
    print(f"{col} を reverse score に変換しました: 0 - {col}")

# =========================
# 7. 回帰分析
# =========================
analysis_columns = [
    outcome,
    exposure,
    education,
    covariate,
]

analysis_df = df[analysis_columns].dropna().copy()

print(f"解析対象者数: {len(analysis_df)}")

# 母親学歴2値 × 経済的ゆとりの交互作用
formula = f"{outcome} ~ {exposure} * {education} + {covariate}"

model = smf.ols(formula=formula, data=analysis_df).fit()

print("===== 回帰式 =====")
print(formula)

print("===== 回帰結果 =====")
print(model.summary())

# =========================
# 8. 交互作用プロット：95%CI付き
# =========================

# 経済的ゆとりの範囲を作成
x_min = analysis_df[exposure].min()
x_max = analysis_df[exposure].max()

x_values = pd.Series(
    [x_min + (x_max - x_min) * i / 100 for i in range(101)]
)

# 共変量は平均値に固定
covariate_mean = analysis_df[covariate].mean()

# 学歴が低い群 education = 1
plot_df_low = pd.DataFrame({
    exposure: x_values,
    education: 1,
    covariate: covariate_mean,
})

# 学歴が高い群 education = 0
plot_df_high = pd.DataFrame({
    exposure: x_values,
    education: 0,
    covariate: covariate_mean,
})

# 予測値と95%信頼区間を計算
pred_low = model.get_prediction(plot_df_low).summary_frame(alpha=0.05)
pred_high = model.get_prediction(plot_df_high).summary_frame(alpha=0.05)

plot_df_low["predicted_screen_time"] = pred_low["mean"]
plot_df_low["ci_lower"] = pred_low["mean_ci_lower"]
plot_df_low["ci_upper"] = pred_low["mean_ci_upper"]

plot_df_high["predicted_screen_time"] = pred_high["mean"]
plot_df_high["ci_lower"] = pred_high["mean_ci_lower"]
plot_df_high["ci_upper"] = pred_high["mean_ci_upper"]

# グラフ作成
plt.figure(figsize=(7, 5))

# 学歴が低い群
plt.plot(
    plot_df_low[exposure],
    plot_df_low["predicted_screen_time"],
    label="Lower maternal education"
)

plt.fill_between(
    plot_df_low[exposure],
    plot_df_low["ci_lower"],
    plot_df_low["ci_upper"],
    alpha=0.2
)

# 学歴が高い群
plt.plot(
    plot_df_high[exposure],
    plot_df_high["predicted_screen_time"],
    label="Higher maternal education"
)

plt.fill_between(
    plot_df_high[exposure],
    plot_df_high["ci_lower"],
    plot_df_high["ci_upper"],
    alpha=0.2
)

plt.xlabel("Financial difficulty at 18 months")
plt.ylabel("Screen time at 24 months")
plt.title("Interaction between maternal education and financial difficulty")
plt.legend()
plt.tight_layout()

plt.savefig(graph_file, dpi=300)
plt.show()

print(f"95%CI付き交互作用プロットを保存しました: {graph_file}")

# =========================
# 9. 結果を表にまとめる
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
# 10. Excelに保存
# =========================
with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
    results.to_excel(
        writer,
        sheet_name="regression_results",
        index=False
    )

print(f"完了: {output_file} を作成しました")
