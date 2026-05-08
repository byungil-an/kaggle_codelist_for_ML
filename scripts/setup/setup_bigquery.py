"""
setup_bigquery.py
BigQueryのデータセット・テーブル・FULLTEXTインデックスを作成します。
初回のみ実行してください。

実行方法:
    cd scripts/
    python setup/setup_bigquery.py
"""
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import PROJECT_ID, DATASET, TABLE_REF, validate

from google.cloud import bigquery


def create_dataset(client: bigquery.Client) -> None:
    dataset = bigquery.Dataset(f"{PROJECT_ID}.{DATASET}")
    dataset.location = "US"
    client.create_dataset(dataset, exists_ok=True)
    print(f"✅ データセット作成: {DATASET}")


def create_table(client: bigquery.Client) -> None:
    schema = [
        bigquery.SchemaField("doc_id",     "STRING",    mode="REQUIRED"),
        bigquery.SchemaField("comp_name",  "STRING",    mode="REQUIRED"),
        bigquery.SchemaField("domain",     "STRING"),
        bigquery.SchemaField("doc_type",   "STRING"),
        # doc_type の種類:
        #   "discussion"   : KaggleのDiscussion投稿
        #   "code_meta"    : JSONメタデータの概要
        #   "code_summary" : Claudeによるコード要約
        #   "code_feature" : コードから抽出した特徴量スニペット
        bigquery.SchemaField("title",      "STRING"),
        bigquery.SchemaField("content",    "STRING"),
        bigquery.SchemaField("score_rank", "INT64"),
        bigquery.SchemaField("votes",      "INT64"),
        bigquery.SchemaField("url",        "STRING"),
        bigquery.SchemaField("created_at", "TIMESTAMP"),
    ]
    table = bigquery.Table(TABLE_REF, schema=schema)
    client.create_table(table, exists_ok=True)
    print(f"✅ テーブル作成: {TABLE_REF}")


def create_search_index(client: bigquery.Client) -> None:
    sql = f"""
        CREATE SEARCH INDEX IF NOT EXISTS doc_search_idx
        ON `{TABLE_REF}`(content, title)
    """
    client.query(sql).result()
    print("✅ FULLTEXTインデックス作成完了")


def main() -> None:
    validate()
    client = bigquery.Client(project=PROJECT_ID)
    print(f"📦 プロジェクト: {PROJECT_ID}")

    create_dataset(client)
    create_table(client)
    create_search_index(client)

    print("\n🎉 セットアップ完了！")
    print("   次のステップ: python process/load_repo_to_bq.py")


if __name__ == "__main__":
    main()
