"""
code_analyzer.py
GCSに保存されたwinner解法コードをClaude APIで解析し、
特徴量エンジニアリングのロジックを抽出してBigQueryに格納します。
Week 3以降、github_collector.py の後に実行してください。

実行方法:
    cd scripts/
    python process/code_analyzer.py
"""
import sys
import json
import time
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import PROJECT_ID, GCS_BUCKET, TABLE_REF, ANTHROPIC_API_KEY, validate

import anthropic
from google.cloud import storage, bigquery

claude_client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)
bq_client     = bigquery.Client(project=PROJECT_ID)


def analyze_code(code: str, repo: str, filename: str) -> dict:
    """Claude APIでコードを解析して特徴量を抽出します。"""
    prompt = f"""
以下はKaggleコンペの解法コードです。
リポジトリ: {repo} / ファイル: {filename}

```python
{code[:4000]}
```

JSONのみを返してください。他のテキストは一切含めないでください。

{{
  "summary": "このコードの概要（2〜3文）",
  "domain": "churn / finance / ecommerce / health / sales / energy のいずれか",
  "features": [
    {{
      "name": "特徴量名",
      "code": "生成コード（10行以内）",
      "effect": "なぜ効くか"
    }}
  ],
  "models": ["使用モデル名"],
  "key_techniques": ["重要テクニック名"]
}}
"""
    try:
        resp = claude_client.messages.create(
            model="claude-sonnet-4-6",
            max_tokens=1500,
            messages=[{"role": "user", "content": prompt}],
        )
        return json.loads(resp.content[0].text.strip())
    except Exception as e:
        print(f"   ⚠️  解析エラー: {e}")
        return {"summary": "", "features": [], "domain": "unknown"}


def process_gcs_solutions(comp_keyword: str) -> None:
    """GCSのコードを読み込み → Claude解析 → BigQuery格納。"""
    gcs    = storage.Client(project=PROJECT_ID)
    bucket = gcs.bucket(GCS_BUCKET)
    blobs  = list(bucket.list_blobs(prefix=f"solutions/{comp_keyword}/"))

    print(f"📦 {len(blobs)} ファイルを処理します: {comp_keyword}")
    rows: list[dict] = []

    for blob in blobs:
        try:
            data     = json.loads(blob.download_as_text())
            analysis = analyze_code(
                data.get("content", ""),
                data.get("repo", ""),
                data.get("filename", ""),
            )
            time.sleep(1)

            # 特徴量を1行1ドキュメントとして格納
            for i, feat in enumerate(analysis.get("features", [])):
                rows.append({
                    "doc_id":     f"{blob.name}_{i}",
                    "comp_name":  comp_keyword,
                    "domain":     analysis.get("domain", "unknown"),
                    "doc_type":   "code_feature",
                    "title":      feat.get("name", ""),
                    "content": (
                        f"{feat.get('code', '')}\n"
                        f"# 効果: {feat.get('effect', '')}"
                    ),
                    "score_rank": None,
                    "votes":      data.get("stars", 0),
                    "url":        data.get("url", ""),
                })

            # コード全体のサマリーも格納
            rows.append({
                "doc_id":     blob.name,
                "comp_name":  comp_keyword,
                "domain":     analysis.get("domain", "unknown"),
                "doc_type":   "code_summary",
                "title":      f"{data.get('repo','')} / {data.get('filename','')}",
                "content":    analysis.get("summary", ""),
                "score_rank": None,
                "votes":      data.get("stars", 0),
                "url":        data.get("url", ""),
            })

        except Exception as e:
            print(f"   ⚠️  スキップ: {blob.name} ({e})")

    # 500件ずつBigQueryに投入
    chunk_size = 500
    for i in range(0, len(rows), chunk_size):
        chunk  = rows[i:i + chunk_size]
        errors = bq_client.insert_rows_json(TABLE_REF, chunk)
        if errors:
            print(f"   ⚠️  投入エラー: {errors[:2]}")
        else:
            print(f"   {i + len(chunk)}/{len(rows)} 件投入済み")

    print(f"✅ {len(rows)} 件をBigQueryに格納しました")


if __name__ == "__main__":
    validate()
    process_gcs_solutions("playground-series-s4e1-churn")
