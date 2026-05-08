"""
auto_enrich.py
リポジトリ内のJSONファイルを読み込み、
type / evaluation / domain を Claude API で自動補完して上書き保存します。
notebook追加後に最初に実行してください。

実行方法:
    cd scripts/
    python process/auto_enrich.py
"""
import sys
import json
import time
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import CODELIST_DIR, ANTHROPIC_API_KEY, EXCLUDE_DIRS, validate

import anthropic

client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)

REQUIRED_KEYS = ["type", "evaluation", "domain"]


def needs_enrichment(meta: dict) -> bool:
    return any(not meta.get(k) for k in REQUIRED_KEYS)


def infer_meta(code_text: str, meta: dict) -> dict:
    """コード本文からtype/evaluation/domainをClaudeで推定します。"""
    prompt = f"""
以下のKaggleコンペ解法コードとメタ情報から、不足項目を推定してください。
JSONのみを返してください。他のテキストは一切含めないでください。

既存メタ情報:
{json.dumps(meta, ensure_ascii=False)}

コード冒頭:
```python
{code_text[:2000]}
```

以下のJSONを返してください:
{{
  "type": "Classification / Regression / Time Series のいずれか",
  "evaluation": "評価指標名（例: ROC-AUC, RMSLE, Accuracy）",
  "domain": "churn / finance / ecommerce / health / sales / energy のいずれか"
}}
"""
    try:
        resp = client.messages.create(
            model="claude-sonnet-4-20250514",
            max_tokens=200,
            messages=[{"role": "user", "content": prompt}],
        )
        return json.loads(resp.content[0].text.strip())
    except Exception as e:
        print(f"   ⚠️  Claude API エラー: {e}")
        return {}


def enrich_json(json_path: Path, comp_dir: Path) -> bool:
    """JSONを読み込み、不足項目を補完して上書き保存します。"""
    with open(json_path, encoding="utf-8") as f:
        meta = json.load(f)

    if not needs_enrichment(meta):
        return False

    code_path = comp_dir / meta.get("code", "")
    code_text = ""
    if code_path.exists():
        try:
            code_text = code_path.read_text(encoding="utf-8", errors="ignore")
        except Exception:
            pass

    if not code_text:
        print(f"   ⚠️  コードファイルが見つかりません: {meta.get('code')}")
        return False

    print(f"   🤖 Claude で補完中: {json_path.name}")
    inferred = infer_meta(code_text, meta)
    time.sleep(1)

    changed = False
    for key in REQUIRED_KEYS:
        if not meta.get(key) and inferred.get(key):
            meta[key] = inferred[key]
            print(f"      {key} = {inferred[key]}")
            changed = True

    if changed:
        with open(json_path, "w", encoding="utf-8") as f:
            json.dump(meta, f, ensure_ascii=False, indent=2)

    return changed


def main() -> None:
    validate()
    print(f"📂 リポジトリ: {CODELIST_DIR.resolve()}")
    total_changed = 0

    for comp_dir in sorted(CODELIST_DIR.iterdir()):
        if not comp_dir.is_dir():
            continue
        if comp_dir.name in EXCLUDE_DIRS or comp_dir.name.startswith("."):
            continue

        json_files = [
            f for f in comp_dir.glob("*.json")
            if "discussion" not in f.name
        ]
        if not json_files:
            continue

        print(f"\n📁 {comp_dir.name}")
        for json_file in json_files:
            if enrich_json(json_file, comp_dir):
                total_changed += 1

    print(f"\n✅ 完了: {total_changed} ファイルを補完しました")
    print("   次のステップ: python process/load_repo_to_bq.py")


if __name__ == "__main__":
    main()
