import pandas as pd
import re

# =========================
# 1. ファイル名の指定
# =========================
files = {
    "a": r"D:/mint/data_xlsx/a.xlsx",
    "b": r"D:/mint/data_xlsx/b.xlsx",
    "c": r"D:/mint/data_xlsx/c.xlsx",
    "d": r"D:/mint/data_xlsx/d.xlsx",
}

# =========================
# 2. 抽出したい列名
# =========================
target_columns = (
    ["users_id"] +

    [f"G{i}_P1" for i in range(1, 6)] +
    [f"G{i}_P2" for i in range(1, 6)] +
    [f"G{i}_1m" for i in range(1, 6)] +
    [f"G{i}_6m" for i in range(1, 6)] +
    [f"G{i}_12m" for i in range(1, 6)] +
    [f"G{i}_18m" for i in range(1, 6)] +

    [
        "WHO5_all_100_P1",
        "WHO5_all_100_P2",
        "WHO5_all_100_1m",
        "WHO5_all_100_6m",
        "WHO5_all_100_12m",
        "WHO5_all_100_12m",
        "EPDS_1m",
        "EPDS_6m",
        "EPDS_12m",
        "EPDS_18m",
        "A13_P1",
        "age_corrected",
        "H4_P1",
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

    # users_idを正規化
    df["users_id"] = df["users_id"].apply(normalize_users_id)

    # users_id が空の行は除外
    df = df[df["users_id"].notna()].copy()

    return df

a = read_excel_clean(files["a"])
b = read_excel_clean(files["b"])
c = read_excel_clean(files["c"])
d = read_excel_clean(files["d"])

# =========================
# 5. a.xlsx と b.xlsx を縦方向に結合
# =========================
a["_source_file"] = "a"
b["_source_file"] = "b"

ab = pd.concat(
    [a, b],
    axis=0,
    ignore_index=True
)

# =========================
# 6. c.xlsx に含まれる users_id のみに絞る前の確認
# =========================
c_ids = set(c["users_id"].dropna().unique())
a_ids = set(a["users_id"].dropna().unique())
b_ids = set(b["users_id"].dropna().unique())

print("===== ID確認 =====")
print(f"a.xlsx のID数: {len(a_ids)}")
print(f"b.xlsx のID数: {len(b_ids)}")
print(f"c.xlsx のID数: {len(c_ids)}")
print(f"a ∩ c のID数: {len(a_ids & c_ids)}")
print(f"b ∩ c のID数: {len(b_ids & c_ids)}")
print(f"a ∪ b のID数: {len(set(ab['users_id']))}")
print(f"(a ∪ b) ∩ c のID数: {len(set(ab['users_id']) & c_ids)}")

# bにあるがcにないIDの例
b_not_in_c = sorted(list(b_ids - c_ids))
if len(b_not_in_c) > 0:
    print("b.xlsxにはあるがc.xlsxにはないusers_idの例:")
    print(b_not_in_c[:20])

# a+b後の重複確認
duplicated_ab_ids = ab[ab["users_id"].duplicated()]["users_id"].unique()

if len(duplicated_ab_ids) > 0:
    print("警告: a.xlsx と b.xlsx を縦結合した後に重複 users_id があります。")
    print("例:", duplicated_ab_ids[:10])

# =========================
# 7. c.xlsx に含まれる users_id のみに絞る
# =========================
base_ids = c[["users_id"]].drop_duplicates()

merged = base_ids.merge(
    ab,
    on="users_id",
    how="left"
)

print("===== c基準でab結合後 =====")
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
# 9. 必要な列だけ抽出
# =========================

# =========================
# 母の最終学歴グループを作成
# =========================

def is_checked(value):
    """
    チェックボックス型の回答を判定する関数。
    Excel上で 1, "1", True, "TRUE", "○" などの場合にチェックありとみなす。
    必要に応じて条件は調整してください。
    """
    if pd.isna(value):
        return False

    value = str(value).strip()

    return value in ["1", "1.0"]


def classify_mother_education(row):
    """
    母の最終学歴を6グループに分類する。
   スコア:
      2 = 小学校卒
      2 = 中学校卒
      2 = 高校卒
      1 = 短大・専門学校卒
      0 = 4年制大学卒
      0 = 大学院・6年制大学卒

    """

    # 5: 大学院・6年制大学卒
    if is_checked(row.get("I12_7_P1")):
        return 0

    # 4: 大卒
    if is_checked(row.get("I12_6_P1")):
        return 0

    # 3: 短大・専門学校卒
    if is_checked(row.get("I12_4_P1")) or is_checked(row.get("I12_5_P1")):
        return 1

    # 2: 高卒
    if is_checked(row.get("I12_3_P1")):
        return 2

    # 1: 中卒
    if is_checked(row.get("I12_2_P1")):
        return 2

    # 小学校卒業
    if is_checked(row.get("I12_1_P1")):
        return 2

    # どれにも該当しない場合
    return pd.NA


merged["mother_education_6grp"] = merged.apply(classify_mother_education, axis=1)

print("===== 母の最終学歴 6分類 =====")
print(merged["mother_education_6grp"].value_counts(dropna=False))


def classify_father_education(row):
    """
    父の最終学歴を6グループに分類する。
   スコア:
      2 = 小学校卒
      2 = 中学校卒
      2 = 高校卒
      1 = 短大・専門学校卒
      0 = 4年制大学卒
      0 = 大学院・6年制大学卒

    """

    # 5: 大学院・6年制大学卒
    if is_checked(row.get("I22_7_P1")):
        return 0

    # 4: 大卒
    if is_checked(row.get("I22_6_P1")):
        return 0

    # 3: 短大・専門学校卒
    if is_checked(row.get("I22_4_P1")) or is_checked(row.get("I22_5_P1")):
        return 1

    # 2: 高卒
    if is_checked(row.get("I22_3_P1")):
        return 2

    # 1: 中卒
    if is_checked(row.get("I22_2_P1")):
        return 2

    # 小学校卒業
    if is_checked(row.get("I22_1_P1")):
        return 2

    # どれにも該当しない場合
    return pd.NA


merged["father_education_6grp"] = merged.apply(classify_father_education, axis=1)

print("===== 父の最終学歴 6分類 =====")
print(merged["father_education_6grp"].value_counts(dropna=False))


existing_columns = [col for col in target_columns if col in merged.columns]
missing_columns = [col for col in target_columns if col not in merged.columns]

if missing_columns:
    print("以下の列は結合後データに存在しませんでした:")
    for col in missing_columns:
        print(f"  - {col}")

result = merged[existing_columns]

# =========================
# 10. 保存
# =========================
result.to_excel(r"D:/mint/data_xlsx/merged_selected_age_corrected.xlsx", index=False)

print("完了: merged_selected.xlsx を作成しました")
print(f"出力行数: {len(result)}")
print(f"出力列数: {len(result.columns)}")