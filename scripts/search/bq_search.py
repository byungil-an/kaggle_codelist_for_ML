"""
bq_search.py
BigQueryのFULLTEXT検索で過去コンペの知識を検索します。
単体で動作確認するときや、他スクリプトからimportして使います。

実行方法:
    cd scripts/
    python search/bq_search.py
"""
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import PROJECT_ID, TABLE_REF, validate

from google.cloud import bigquery

client = bigquery.Client(project=PROJECT_ID)


def search(
    query: str,
    comp_name: str | None = None,
    domain: str | None = None,
    doc_types: list[str] | None = None,
    limit: int = 10,
) -> list[dict]:
    """
    BigQuery FULLTEXTでキーワード検索します。

    Args:
        query:     検索キーワード（例: "Target Encoding churn"）
        comp_name: コンペ名で絞り込み（Noneなら全コンペ横断）
        domain:    ドメインで絞り込み（例: "churn", "finance"）
        doc_types: ドキュメント種別（例: ["code_feature", "discussion"]）
        limit:     取得件数
    """
    conditions = ["SEARCH(content, @query)"]
    params = [bigquery.ScalarQueryParameter("query", "STRING", query)]

    if comp_name:
        conditions.append("comp_name = @comp_name")
        params.append(
            bigquery.ScalarQueryParameter("comp_name", "STRING", comp_name)
        )
    if domain:
        conditions.append("domain = @domain")
        params.append(
            bigquery.ScalarQueryParameter("domain", "STRING", domain)
        )
    if doc_types:
        placeholders = ", ".join(f"@dt_{i}" for i in range(len(doc_types)))
        conditions.append(f"doc_type IN ({placeholders})")
        for i, dt in enumerate(doc_types):
            params.append(
                bigquery.ScalarQueryParameter(f"dt_{i}", "STRING", dt)
            )

    params.append(bigquery.ScalarQueryParameter("limit", "INT64", limit))

    sql = f"""
        SELECT title, content, doc_type, comp_name, domain, votes, url
        FROM `{TABLE_REF}`
        WHERE {" AND ".join(conditions)}
        ORDER BY
            CASE doc_type
                WHEN 'code_feature' THEN 1
                WHEN 'code_summary' THEN 2
                WHEN 'discussion'   THEN 3
                WHEN 'code_meta'    THEN 4
                ELSE 5
            END,
            COALESCE(votes, 0) DESC
        LIMIT @limit
    """
    cfg  = bigquery.QueryJobConfig(query_parameters=params)
    rows = list(client.query(sql, cfg).result())
    return [dict(r) for r in rows]


def print_results(results: list[dict]) -> None:
    if not results:
        print("   ヒットなし")
        return
    for r in results:
        print(f"\n  [{r['doc_type']}] {r['title']}")
        print(f"  comp: {r['comp_name']} | domain: {r['domain']}")
        print(f"  {r['content'][:200]}...")


if __name__ == "__main__":
    validate()

    print("=== 全コンペ横断: Target Encoding ===")
    print_results(search("Target Encoding", limit=5))

    print("\n=== churn絞り込み: feature engineering ===")
    print_results(search("feature engineering", domain="churn", limit=5))

    print("\n=== 特徴量コードのみ: LightGBM ===")
    print_results(
        search("LightGBM", doc_types=["code_feature", "code_summary"], limit=5)
    )
