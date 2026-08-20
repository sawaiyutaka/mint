import pandas as pd
import re

# =========================
# 1. ファイル名の指定
# =========================
files = {
    "a": r"D:/mint/data_xlsx/a.xlsx",
    "b": r"D:/mint/data_xlsx/b.xlsx",  # r"D:/mint/data_xlsx/b.xlsx",
    "c": r"D:/mint/data_xlsx/c.xlsx",
    "d": r"D:/mint/data_xlsx/d.xlsx",
}

output_file = r"D:/mint/data_xlsx/merged_ipv.xlsx"

# =========================
# 2. 抽出したい列名
# =========================
g_timepoints = ["P1", "P2", "1m", "6m", "12m", "18m"]

target_columns = (
    ["users_id"] +

    [f"G{i}_{tp}" for tp in g_timepoints for i in range(1, 6)] +
    [f"G{i}" for i in range(1, 6)] +
    [f"D{i}" for i in range(1, 6)] +
    [f"R{i}" for i in range(1, 7)] +

    [f"WHO5_all_100_{tp}" for tp in g_timepoints] +
    [f"EPDS_{tp}" for tp in ["1m", "6m", "12m", "18m"]] +
    [f"E1_{tp}" for tp in ["1m", "6m", "12m", "18m"]] +

    [
        "A13_P1",
        "age_corrected",
        "H4_P1",
        "C4-4",
        "C4-6",
    ] +

    [f"I11_{i}_P1" for i in range(1, 8)] +
    [f"I12_{i}_P1" for i in range(1, 8)] +
    [f"I21_{i}_P1" for i in range(1, 8)] +

    [
        "mother_education_6grp",
    ] +

    [f"I22_{i}_P1" for i in range(1, 8)] +
    [f"I23_{i}_P1" for i in range(1, 8)] +

    [
        "father_education_6grp",
        "AF3",
        "AF4",
        "AF1",
        "AF2",
        "AF5",
        "AF6",
    ] +

    [
        "PC1",
        "PC2",
        "PC3",
        "PC4",
        "PC5",
        "PC6",
    ]
)

# =========================
# 3. users_id の正規化関数
# =========================
def normalize_users_id(x):
    """
    Excel由来のID表記ゆれを補正する。
    例:
      123.0 -> 123
      " 123 " -> 123
      空欄 -> NA
    """
    if pd.isna(x):
        return pd.NA

    x = str(x).strip()

    if x == "":
        return pd.NA

    # Excelで数値として読まれた 123.0 を 123 にする
    if re.fullmatch(r"\d+\.0", x):
        x = x[:-2]

    return x


# =========================
# 4. Excelの読み込み関数
# =========================
def read_excel_clean(path):
    # users_idの型崩れを避けるため、まず文字列として読む
    df = pd.read_excel(path, dtype={"users_id": str})

    # 列名の前後の空白を削除
    df.columns = df.columns.astype(str).str.strip()

    if "users_id" not in df.columns:
        raise ValueError(f"{path} に users_id 列がありません。")

    df["users_id"] = df["users_id"].apply(normalize_users_id)

    # users_id が空の行は除外
    df = df[df["users_id"].notna()].copy()

    return df


data = {name: read_excel_clean(path) for name, path in files.items()}

a = data["a"]
b = data["b"]
c = data["c"]
d = data["d"]

# =========================
# 5. a.xlsx と b.xlsx を縦方向に結合
# =========================
a = a.copy()
b = b.copy()

a["_source_file"] = "a"
b["_source_file"] = "b"

ab = pd.concat([a, b], axis=0, ignore_index=True)

# =========================
# 6. d.xlsx に含まれる users_id のみに絞る前の確認
# =========================
a_ids = set(a["users_id"].dropna().unique())
b_ids = set(b["users_id"].dropna().unique())
c_ids = set(c["users_id"].dropna().unique())
d_ids = set(d["users_id"].dropna().unique())
ab_ids = set(ab["users_id"].dropna().unique())

print("===== ID確認 =====")
print(f"a.xlsx のID数: {len(a_ids)}")
print(f"b.xlsx のID数: {len(b_ids)}")
print(f"c.xlsx のID数: {len(c_ids)}")
print(f"d.xlsx のID数: {len(d_ids)}")
print(f"a ∩ d のID数: {len(a_ids & d_ids)}")
print(f"b ∩ d のID数: {len(b_ids & d_ids)}")
print(f"c ∩ d のID数: {len(c_ids & d_ids)}")
print(f"a ∪ b のID数: {len(ab_ids)}")
print(f"(a ∪ b) ∩ d のID数: {len(ab_ids & d_ids)}")

# bにあるがdにないIDの例
b_not_in_d = sorted(b_ids - d_ids)

if b_not_in_d:
    print("b.xlsxにはあるがd.xlsxにはないusers_idの例:")
    print(b_not_in_d[:20])

# a+b後の重複確認
duplicated_ab_ids = ab.loc[ab["users_id"].duplicated(), "users_id"].unique()

if len(duplicated_ab_ids) > 0:
    print("警告: a.xlsx と b.xlsx を縦結合した後に重複 users_id があります。")
    print("例:", duplicated_ab_ids[:10])

# =========================
# 7. d.xlsx に含まれる users_id のみに絞る
# =========================
base_ids = d[["users_id"]].drop_duplicates()

merged = base_ids.merge(
    ab,
    on="users_id",
    how="left"
)

print("===== d基準でab結合後 =====")
print(merged["_source_file"].value_counts(dropna=False))

# =========================
# 8. c.xlsx, d.xlsx を横方向に結合
# =========================
merged = merged.merge(
    c,
    on="users_id",
    how="left",
    suffixes=("", "_c")
)

merged = merged.merge(
    d,
    on="users_id",
    how="left",
    suffixes=("", "_d")
)

# =========================
# 9. 母・父の最終学歴グループを作成
# =========================
def is_checked(value):
    """
    チェックボックス型の回答を判定する関数。
    Excel上で 1, "1", "1.0" の場合にチェックありとみなす。
    """
    if pd.isna(value):
        return False

    return str(value).strip() in ["1", "1.0"]


def classify_education(row, prefix):
    """
    最終学歴を3グループに分類する。

    0 = 4年制大学卒、大学院・6年制大学卒
    1 = 短大・専門学校卒
    2 = 小学校卒、中学校卒、高校卒
    """

    # 大学院・6年制大学卒
    if is_checked(row.get(f"{prefix}_7_P1")):
        return 0

    # 4年制大学卒
    if is_checked(row.get(f"{prefix}_6_P1")):
        return 0

    # 短大・専門学校卒
    if is_checked(row.get(f"{prefix}_4_P1")) or is_checked(row.get(f"{prefix}_5_P1")):
        return 1

    # 高校卒・中学校卒・小学校卒
    if (
        is_checked(row.get(f"{prefix}_3_P1")) or
        is_checked(row.get(f"{prefix}_2_P1")) or
        is_checked(row.get(f"{prefix}_1_P1"))
    ):
        return 2

    return pd.NA


merged["mother_education_6grp"] = merged.apply(
    classify_education,
    axis=1,
    prefix="I12"
)

merged["father_education_6grp"] = merged.apply(
    classify_education,
    axis=1,
    prefix="I22"
)

print("===== 母の最終学歴分類 =====")
print(merged["mother_education_6grp"].value_counts(dropna=False))

print("===== 父の最終学歴分類 =====")
print(merged["father_education_6grp"].value_counts(dropna=False))

# =========================
# 10. 必要な列だけ抽出
# =========================
existing_columns = [col for col in target_columns if col in merged.columns]
missing_columns = [col for col in target_columns if col not in merged.columns]

if missing_columns:
    print("以下の列は結合後データに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

result = merged[existing_columns].copy()

# =========================
# 11. 保存
# =========================
result.to_excel(output_file, index=False)

print(f"完了: {output_file} を作成しました")
print(f"出力行数: {len(result)}")
print(f"出力列数: {len(result.columns)}")