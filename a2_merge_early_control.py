import pandas as pd
import re

# =========================
# 1. ファイル名の指定
# =========================
files = {
    "a": r"D:/mint/data_xlsx/a.xlsx",
    "b": r"D:/mint/20260526共有用_MINT/20260526共有用/P1,P2,1m,6m,12m_control_data/20241212_control_P1-12m_U25_Pilot_excluded_N=158.xlsx",
    "c": r"D:/mint/data_xlsx/c.xlsx",
    "d": r"D:/mint/data_xlsx/d.xlsx",
}

output_file = r"D:/mint/data_xlsx/merged_early_control.xlsx"


# =========================
# 2. 抽出したい列名
# =========================
g_timepoints = ["P1", "P2", "1m", "6m", "12m", "18m"]

target_columns = (
    [
        "users_id",
        "intervention",
    ] +

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
    ]
)


# =========================
# 3. users_idの正規化関数
# =========================
def normalize_users_id(x):
    """
    Excel由来のID表記ゆれを補正する。

    例:
      123.0   -> 123
      " 123 " -> 123
      空欄    -> NA
    """
    if pd.isna(x):
        return pd.NA

    x = str(x).strip()

    if x == "":
        return pd.NA

    # Excelで数値として読み込まれた「123.0」を「123」にする
    if re.fullmatch(r"\d+\.0", x):
        x = x[:-2]

    return x


# =========================
# 4. Excelの読み込み関数
# =========================
def read_excel_clean(path):
    """
    Excelファイルを読み込み、列名とusers_idを整える。
    """
    # users_idの型崩れを避けるため、文字列として読み込む
    df = pd.read_excel(
        path,
        dtype={"users_id": str}
    )

    # 列名の前後の空白を削除
    df.columns = df.columns.astype(str).str.strip()

    if "users_id" not in df.columns:
        raise ValueError(
            f"{path} に users_id 列がありません。"
        )

    # users_idを正規化
    df["users_id"] = df["users_id"].apply(
        normalize_users_id
    )

    # users_idが欠損している行を除外
    df = df.loc[
        df["users_id"].notna()
    ].copy()

    return df


# =========================
# 5. Excelファイルの読み込み
# =========================
data = {
    name: read_excel_clean(path)
    for name, path in files.items()
}

a = data["a"]
b = data["b"]
c = data["c"]
d = data["d"]


# =========================
# 6. intervention変数の作成
# =========================
a = a.copy()
b = b.copy()

# a由来の参加者
a["intervention"] = 1

# b由来の参加者
b["intervention"] = 0

# データ確認用の変数
a["_source_file"] = "a"
b["_source_file"] = "b"


# =========================
# 7. 各ファイル内のID重複を確認
# =========================
def check_duplicate_users_id(df, file_name):
    """
    1つのファイル内でusers_idが重複していないか確認する。
    重複があれば処理を停止する。
    """
    duplicated_ids = (
        df.loc[
            df["users_id"].duplicated(keep=False),
            "users_id"
        ]
        .dropna()
        .unique()
    )

    if len(duplicated_ids) > 0:
        print(
            f"{file_name}.xlsx内に重複する"
            "users_idがあります。"
        )
        print("重複IDの例:")
        print(duplicated_ids[:20])

        raise ValueError(
            f"{file_name}.xlsx内のusers_idが"
            "一意ではありません。"
        )


check_duplicate_users_id(a, "a")
check_duplicate_users_id(b, "b")
check_duplicate_users_id(c, "c")
check_duplicate_users_id(d, "d")


# =========================
# 8. 各ファイルのID集合を作成
# =========================
a_ids = set(a["users_id"].dropna().unique())
b_ids = set(b["users_id"].dropna().unique())
c_ids = set(c["users_id"].dropna().unique())
d_ids = set(d["users_id"].dropna().unique())

ab_ids = a_ids | b_ids
overlap_ab_ids = a_ids & b_ids
analysis_ids = ab_ids & d_ids


# =========================
# 9. ID数の確認
# =========================
print("===== ID確認 =====")
print(f"a.xlsx のID数: {len(a_ids)}")
print(f"b.xlsx のID数: {len(b_ids)}")
print(f"c.xlsx のID数: {len(c_ids)}")
print(f"d.xlsx のID数: {len(d_ids)}")

print(f"a ∩ b のID数: {len(overlap_ab_ids)}")
print(f"a ∩ d のID数: {len(a_ids & d_ids)}")
print(f"b ∩ d のID数: {len(b_ids & d_ids)}")
print(f"c ∩ d のID数: {len(c_ids & d_ids)}")

print(f"a ∪ b のID数: {len(ab_ids)}")
print(
    f"(a ∪ b) ∩ d のID数: "
    f"{len(analysis_ids)}"
)


# =========================
# 10. aとbの両方に存在するIDを確認
# =========================
if overlap_ab_ids:
    print(
        "a.xlsxとb.xlsxの両方に存在する"
        "users_idがあります。"
    )
    print("重複IDの例:")
    print(sorted(overlap_ab_ids)[:20])

    raise ValueError(
        f"a.xlsxとb.xlsxの両方に存在する"
        f"users_idが{len(overlap_ab_ids)}件あります。"
        "interventionを一意に定義できないため、"
        "処理を停止しました。"
    )


# =========================
# 11. dに含まれないIDを確認
# =========================
a_not_in_d = sorted(a_ids - d_ids)
b_not_in_d = sorted(b_ids - d_ids)

if a_not_in_d:
    print(
        "a.xlsxにはあるがd.xlsxにはない"
        "users_idの例:"
    )
    print(a_not_in_d[:20])

if b_not_in_d:
    print(
        "b.xlsxにはあるがd.xlsxにはない"
        "users_idの例:"
    )
    print(b_not_in_d[:20])


# =========================
# 12. a.xlsxとb.xlsxを縦方向に結合
# =========================
ab = pd.concat(
    [a, b],
    axis=0,
    ignore_index=True,
    sort=False
)

print("===== aとbの縦結合後 =====")
print(f"行数: {len(ab)}")
print(f"users_id数: {ab['users_id'].nunique()}")

print("interventionの分布:")
print(
    ab["intervention"]
    .value_counts(dropna=False)
    .sort_index()
)


# =========================
# 13. (aまたはb) かつdに含まれるIDだけを残す
# =========================

# dからusers_idだけを取り出す
d_ids_df = (
    d[["users_id"]]
    .drop_duplicates()
)

# inner mergeにより、abとdの両方に存在するIDだけを残す
merged = ab.merge(
    d_ids_df,
    on="users_id",
    how="inner",
    validate="one_to_one"
)

print("===== (a ∪ b) ∩ d の抽出結果 =====")
print(f"抽出後の行数: {len(merged)}")
print(
    f"抽出後のusers_id数: "
    f"{merged['users_id'].nunique()}"
)

print("interventionの分布:")
print(
    merged["intervention"]
    .value_counts(dropna=False)
    .sort_index()
)

print("出典ファイルの分布:")
print(
    merged["_source_file"]
    .value_counts(dropna=False)
)


# =========================
# 14. 想定ID数との一致を確認
# =========================
expected_n = len(analysis_ids)
actual_n = merged["users_id"].nunique()

if actual_n != expected_n:
    raise ValueError(
        f"想定ID数は{expected_n}人ですが、"
        f"実際の抽出結果は{actual_n}人でした。"
    )


# =========================
# 15. c.xlsxを横方向に結合
# =========================
n_before_c_merge = len(merged)

merged = merged.merge(
    c,
    on="users_id",
    how="left",
    suffixes=("", "_c"),
    validate="one_to_one"
)

if len(merged) != n_before_c_merge:
    raise ValueError(
        "c.xlsxとの結合後に行数が変化しました。"
    )


# =========================
# 16. d.xlsxを横方向に結合
# =========================
n_before_d_merge = len(merged)

merged = merged.merge(
    d,
    on="users_id",
    how="left",
    suffixes=("", "_d"),
    validate="one_to_one"
)

if len(merged) != n_before_d_merge:
    raise ValueError(
        "d.xlsxとの結合後に行数が変化しました。"
    )

print("===== cおよびdとの横結合後 =====")
print(f"行数: {len(merged)}")
print(f"列数: {len(merged.columns)}")


# =========================
# 17. 母・父の最終学歴グループを作成
# =========================
def is_checked(value):
    """
    チェックボックス型の回答を判定する。

    Excel上で以下の場合にチェックありとみなす:
      1
      "1"
      "1.0"
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
    if (
        is_checked(row.get(f"{prefix}_4_P1")) or
        is_checked(row.get(f"{prefix}_5_P1"))
    ):
        return 1

    # 小学校卒・中学校卒・高校卒
    if (
        is_checked(row.get(f"{prefix}_1_P1")) or
        is_checked(row.get(f"{prefix}_2_P1")) or
        is_checked(row.get(f"{prefix}_3_P1"))
    ):
        return 2

    return pd.NA


# 母親の最終学歴
merged["mother_education_6grp"] = merged.apply(
    classify_education,
    axis=1,
    prefix="I12"
)

# 父親の最終学歴
merged["father_education_6grp"] = merged.apply(
    classify_education,
    axis=1,
    prefix="I22"
)

# 欠損値を扱える整数型に変換
merged["mother_education_6grp"] = (
    merged["mother_education_6grp"]
    .astype("Int64")
)

merged["father_education_6grp"] = (
    merged["father_education_6grp"]
    .astype("Int64")
)

# interventionも整数型に統一
merged["intervention"] = (
    merged["intervention"]
    .astype("Int64")
)

print("===== 母の最終学歴分類 =====")
print(
    merged["mother_education_6grp"]
    .value_counts(dropna=False)
    .sort_index()
)

print("===== 父の最終学歴分類 =====")
print(
    merged["father_education_6grp"]
    .value_counts(dropna=False)
    .sort_index()
)


# =========================
# 18. 必要な列だけ抽出
# =========================
existing_columns = [
    col for col in target_columns
    if col in merged.columns
]

missing_columns = [
    col for col in target_columns
    if col not in merged.columns
]

if missing_columns:
    print(
        "以下の列は結合後データに"
        "存在しませんでした:"
    )

    for col in missing_columns:
        print(f"  - {col}")

result = merged[existing_columns].copy()


# =========================
# 19. 最終データの確認
# =========================
print("===== 最終データ確認 =====")
print(f"出力予定行数: {len(result)}")
print(f"出力予定列数: {len(result.columns)}")
print(
    f"users_idのユニーク数: "
    f"{result['users_id'].nunique()}"
)

print("interventionの分布:")
print(
    result["intervention"]
    .value_counts(dropna=False)
    .sort_index()
)

# 最終データでusers_idが重複していないか確認
final_duplicated_ids = (
    result.loc[
        result["users_id"].duplicated(keep=False),
        "users_id"
    ]
    .dropna()
    .unique()
)

if len(final_duplicated_ids) > 0:
    print("最終データに重複IDがあります。")
    print("例:")
    print(final_duplicated_ids[:20])

    raise ValueError(
        "最終データでusers_idが重複しています。"
    )

# interventionが0/1以外になっていないか確認
invalid_intervention = result.loc[
    result["intervention"].notna() &
    ~result["intervention"].isin([0, 1]),
    "intervention"
].unique()

if len(invalid_intervention) > 0:
    raise ValueError(
        "interventionに0または1以外の値があります。"
    )


# =========================
# 20. Excelファイルとして保存
# =========================
result.to_excel(
    output_file,
    index=False
)

print("===== 保存完了 =====")
print(f"保存先: {output_file}")
print(f"出力行数: {len(result)}")
print(f"出力列数: {len(result.columns)}")
