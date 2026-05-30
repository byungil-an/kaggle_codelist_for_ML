"""
load_repo_to_bq.py
リポジトリ内のコンペフォルダを全走査し、
JSONメタデータ・コード要約・Discussionを BigQuery に一括投入します。
auto_enrich.py の後に実行してください。

実行方法:
    cd scripts/
    python process/load_repo_to_bq.py
"""
import sys
import json
import time
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import (
    PROJECT_ID, TABLE_REF, CODELIST_DIR,
    ANTHROPIC_API_KEY, EXCLUDE_DIRS, validate,
)

import anthropic
from google.cloud import bigquery

bq_client     = bigquery.Client(project=PROJECT_ID)
claude_client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)

BASE_URL = "https://github.com/byungil-an/kaggle_codelist_for_ML/tree/main/kaggle_comps"


def summarize_code(code_text: str, meta: dict) -> str:
    """Claude APIでコードを要約します。"""
    prompt = f"""
以下はKaggleコンペ「{meta.get('title', '')}」の解法コードです。
モデル: {meta.get('model', [])}

```python
{code_text[:3000]}
```

「特徴量エンジニアリングの手法」と「モデル設定のポイント」を
それぞれ箇条書きで5点ずつ日本語で抽出してください。
コードスニペットも含めてください。
"""
    try:
        resp = claude_client.messages.create(
            model="claude-sonnet-4-6",
            max_tokens=1000,
            messages=[{"role": "user", "content": prompt}],
        )
        time.sleep(1)
        return resp.content[0].text
    except Exception as e:
        print(f"   ⚠️  要約エラー: {e}")
        return ""


def process_comp_dir(comp_dir: Path) -> list[dict]:
    """コンペフォルダを処理してBQ投入用の行リストを返します。"""
    rows: list[dict] = []
    url = f"{BASE_URL}/{comp_dir.name}"

    # ── JSONメタデータ ──────────────────────────────
    for json_file in comp_dir.glob("*.json"):
        if "discussion" in json_file.name:
            continue
        try:
            with open(json_file, encoding="utf-8") as f:
                meta = json.load(f)
        except Exception as e:
            print(f"   ⚠️  JSON読み込みエラー: {json_file.name} ({e})")
            continue

        rows.append({
            "doc_id":     f"{comp_dir.name}_{json_file.stem}_meta",
            "comp_name":  comp_dir.name,
            "domain":     meta.get("domain") or meta.get("type", "unknown"),
            "doc_type":   "code_meta",
            "title":      meta.get("title", ""),
            "content":    meta.get("overview", ""),
            "score_rank": None,
            "votes":      int(meta.get("score", 0) * 1000),
            "url":        url,
        })

        # コードを要約してBQに格納
        code_path = comp_dir / meta.get("code", "")
        if code_path.exists():
            try:
                code_text = code_path.read_text(
                    encoding="utf-8", errors="ignore"
                )
                print(f"   🤖 要約中: {code_path.name}")
                summary = summarize_code(code_text, meta)
                if summary:
                    rows.append({
                        "doc_id": f"{comp_dir.name}_{json_file.stem}_summary",
                        "comp_name":  comp_dir.name,
                        "domain": meta.get("domain") or meta.get("type", "unknown"),
                        "doc_type":   "code_summary",
                        "title": (
                            f"[要約] {meta.get('title', '')} "
                            f"({meta.get('medal', '')} / {meta.get('score', '')})"
                        ),
                        "content":    summary,
                        "score_rank": None,
                        "votes":      int(meta.get("score", 0) * 1000),
                        "url":        url,
                    })
            except Exception as e:
                print(f"   ⚠️  コード読み込みエラー: {code_path.name} ({e})")

    # ── Discussion JSON ─────────────────────────────
    for disc_file in comp_dir.rglob("*discussion*.json"):
        try:
            with open(disc_file, encoding="utf-8") as f:
                discussions = json.load(f)
            for d in discussions:
                d.setdefault("comp_name", comp_dir.name)
                rows.append(d)
        except Exception as e:
            print(f"   ⚠️  Discussion読み込みエラー: {disc_file.name} ({e})")

    return rows


def insert_rows(rows: list[dict]) -> None:
    """500件ずつBigQueryに投入します。"""
    chunk_size = 500
    for i in range(0, len(rows), chunk_size):
        chunk  = rows[i:i + chunk_size]
        errors = bq_client.insert_rows_json(TABLE_REF, chunk)
        if errors:
            print(f"   ⚠️  投入エラー: {errors[:2]}")
        else:
            print(f"   {i + len(chunk)}/{len(rows)} 件投入済み")


def main() -> None:
    validate()
    print(f"📂 リポジトリ: {CODELIST_DIR.resolve()}")
    all_rows: list[dict] = []

    for comp_dir in sorted(CODELIST_DIR.iterdir()):
        if not comp_dir.is_dir():
            continue
        if comp_dir.name in EXCLUDE_DIRS or comp_dir.name.startswith("."):
            continue

        print(f"\n📁 {comp_dir.name}")
        rows = process_comp_dir(comp_dir)
        all_rows.extend(rows)
        print(f"   {len(rows)} 件を準備")

    print(f"\n📤 BigQueryに投入中... 合計 {len(all_rows)} 件")
    insert_rows(all_rows)
    print("\n🎉 完了！rag_advisor.py でアドバイスを取得できます。")


if __name__ == "__main__":
    main()
