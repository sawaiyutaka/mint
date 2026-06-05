import pandas as pd

# =========================
# 1. ファイル読み込み
# =========================
input_file = r"D:/mint/data_xlsx/merged_selected.xlsx"

df = pd.read_excel(input_file)

# 列名の前後の空白を削除
df.columns = df.columns.astype(str).str.strip()

print(f"読み込み行数: {len(df)}")
print(f"読み込み列数: {len(df.columns)}")

# =========================
# 2. 記述統計を見たい列
# =========================

timepoints = ["P1", "P2", "1m", "6m", "12m", "18m"]

continuous_columns = (
    [
        "A13_P1",  # baseline age
    ] +

    [f"G{i}_{tp}" for tp in timepoints for i in range(1, 6)] +

    [f"WHO5_all_100_{tp}" for tp in timepoints] +

    [
        "EPDS_1m",
        "EPDS_6m",
        "EPDS_12m",
        "EPDS_18m",
    ]
)

categorical_columns = [
    "H4_P1",  # baseline income
    "mother_education_6grp",
    "father_education_6grp",
    "AF3",
    "AF4",
]

# =========================
# 3. 連続変数の記述統計
# =========================

existing_continuous_columns = [col for col in continuous_columns if col in df.columns]
missing_continuous_columns = [col for col in continuous_columns if col not in df.columns]

if missing_continuous_columns:
    print("以下の連続変数列はデータに存在しませんでした:")
    for col in missing_continuous_columns:
        print(f"  - {col}")

df_cont = df[existing_continuous_columns].apply(pd.to_numeric, errors="coerce")

continuous_summary = pd.DataFrame({
    "variable": existing_continuous_columns,
    "n": df_cont.notna().sum().values,
    "missing": df_cont.isna().sum().values,
    "mean": df_cont.mean().values,
    "sd": df_cont.std().values,
    "median": df_cont.median().values,
    "min": df_cont.min().values,
    "max": df_cont.max().values,
})

continuous_summary_rounded = continuous_summary.copy()
for col in ["mean", "sd", "median", "min", "max"]:
    continuous_summary_rounded[col] = continuous_summary_rounded[col].round(2)

print("===== 連続変数の記述統計 =====")
print(continuous_summary_rounded)

# =========================
# 4. カテゴリ変数の度数・割合
# =========================

categorical_summaries = []

existing_categorical_columns = [col for col in categorical_columns if col in df.columns]
missing_categorical_columns = [col for col in categorical_columns if col not in df.columns]

if missing_categorical_columns:
    print("以下のカテゴリ変数列はデータに存在しませんでした:")
    for col in missing_categorical_columns:
        print(f"  - {col}")

for col in existing_categorical_columns:
    tmp = df[[col]].copy()

    # categoryを数値化して並べ替え用に使う
    tmp["category_num"] = pd.to_numeric(tmp[col], errors="coerce")

    # 表示用カテゴリ
    tmp["category"] = tmp[col].astype("object")
    tmp.loc[tmp[col].isna(), "category"] = "missing"

    # 度数集計
    summary = (
        tmp
        .groupby(["category_num", "category"], dropna=False)
        .size()
        .reset_index(name="n")
    )

    # 割合
    summary["percent"] = summary["n"] / len(df) * 100
    summary["percent"] = summary["percent"].round(1)

    # missingを最後にするための並べ替えキー
    summary["missing_sort"] = summary["category"].eq("missing").astype(int)

    summary = summary.sort_values(
        by=["missing_sort", "category_num"],
        ascending=[True, True]
    )

    summary.insert(0, "variable", col)

    summary = summary[
        ["variable", "category", "n", "percent"]
    ]

    categorical_summaries.append(summary)

if categorical_summaries:
    categorical_summary = pd.concat(categorical_summaries, ignore_index=True)
else:
    categorical_summary = pd.DataFrame(
        columns=["variable", "category", "n", "percent"]
    )

print("===== カテゴリ変数の記述統計 =====")
print(categorical_summary)

# =========================
# 5. Excelに保存
# =========================

output_file = r"D:/mint/results/descriptive_statistics.xlsx"

with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
    continuous_summary_rounded.to_excel(
        writer,
        sheet_name="continuous_summary",
        index=False
    )

    categorical_summary.to_excel(
        writer,
        sheet_name="categorical_summary",
        index=False
    )

print(f"完了: {output_file} を作成しました")
