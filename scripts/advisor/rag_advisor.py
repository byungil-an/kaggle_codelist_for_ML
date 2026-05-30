"""
rag_advisor.py
BigQueryの知識ベースを参照して、Claude APIがコンペのアドバイスを生成します。
コンペに参加するたびに実行してください。

実行方法:
    cd scripts/
    python advisor/rag_advisor.py

設定ファイル:
    scripts/partipatingcomp/partipatingcomp.json を編集してから実行してください。
"""
import json
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import ANTHROPIC_API_KEY, validate
from search.hybrid_search import hybrid_search, build_context

import anthropic

client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)

# 設定ファイルのパス（scripts/partipatingcomp/partipatingcomp.json）
CONFIG_PATH = Path(__file__).parent.parent / "partipatingcomp" / "partipatingcomp.json"

SYSTEM_PROMPT = """
あなたはKaggleコンペのメダル獲得を支援するAIアドバイザーです。
過去のwinner解法とDiscussionの知見を参照し、
実際にそのまま使えるPythonコードとともに改善策を提案してください。
回答は必ず日本語で行ってください。
"""


def load_config() -> dict:
    """partipatingcomp.json を読み込んで設定を返します。"""
    if not CONFIG_PATH.exists():
        raise FileNotFoundError(f"設定ファイルが見つかりません: {CONFIG_PATH}")
    with open(CONFIG_PATH, encoding="utf-8") as f:
        cfg = json.load(f)
    # _comments キーは設定値ではないので除外
    return {k: v for k, v in cfg.items() if not k.startswith("_")}


def load_basebook(filename: str | None) -> str | None:
    """
    partipatingcomp/ フォルダ内の basebook ファイルを読み込みます。
    .ipynb はコードセルのみ抽出、.py はそのまま返します。
    ファイルが存在しない・未指定の場合は None を返します。
    """
    if not filename:
        return None

    basebook_path = CONFIG_PATH.parent / filename
    if not basebook_path.exists():
        print(f"   ⚠️  basebook が見つかりません（スキップ）: {basebook_path}")
        return None

    if basebook_path.suffix == ".ipynb":
        with open(basebook_path, encoding="utf-8") as f:
            nb = json.load(f)
        # コードセルのソースのみ結合（空セルは除外）
        code_cells = [
            "".join(cell["source"])
            for cell in nb.get("cells", [])
            if cell.get("cell_type") == "code" and cell.get("source")
        ]
        return "\n\n# ---\n\n".join(code_cells)

    # .py など通常テキストファイル
    return basebook_path.read_text(encoding="utf-8", errors="ignore")


def _search_docs(
    source_comps: list[str],
    domain: str | None,
    extra_keywords: list[str] | None,
) -> list[dict]:
    """
    source_comps が空のときは domain で全コンペ横断検索。
    複数の source_comps がある場合は各コンペを検索してマージ。
    """
    if not source_comps:
        # SOURCE_COMP が空 → DOMAIN をキーに全コンペ横断検索
        print(f"🔍 BigQueryを横断検索中 (domain={domain})")
        return hybrid_search(
            source_comp=None,
            domain=domain,
            extra_keywords=extra_keywords,
            total_limit=12,
        )

    # 複数コンペを順に検索してタイトルで重複除去
    all_docs: list[dict] = []
    seen: set[str] = set()
    for comp in source_comps:
        print(f"🔍 BigQueryを検索中: {comp}")
        docs = hybrid_search(
            source_comp=comp,
            domain=domain,
            extra_keywords=extra_keywords,
            total_limit=12,
        )
        for d in docs:
            if d["title"] not in seen:
                seen.add(d["title"])
                all_docs.append(d)

    return all_docs[:12]


def get_advice(
    question: str,
    target_comp: str,
    source_comps: list[str],
    domain: str | None = None,
    extra_keywords: list[str] | None = None,
    basebook_code: str | None = None,
) -> str:
    """
    RAGでアドバイスを生成します。

    Args:
        question:       Claudeへの質問文
        target_comp:    今参加しているコンペ名
        source_comps:   参照する過去コンペ名のリスト（空なら domain で横断検索）
        domain:         ドメイン（例: "churn"）
        extra_keywords: 追加検索キーワード
        basebook_code:  ベースコード（Noneなら知識ベースのみで提案）
    """
    docs = _search_docs(source_comps, domain, extra_keywords)
    print(f"   {len(docs)} 件の知識を取得")

    context = build_context(docs)

    # 参照元コンペ名をプロンプトに表示（空なら domain 横断と明示）
    source_label = (
        "、".join(source_comps) if source_comps else f"domain={domain} の全コンペ"
    )

    if basebook_code:
        # basebook あり：既存コードへの改善案を提示
        # トークン節約のため先頭 4000 文字に制限
        code_snippet = basebook_code[:4000]
        prompt = f"""
## 現在のベースコード（{target_comp}）

```python
{code_snippet}
```

---

## 参照コンペの知識（{source_label}）

{context}

---

## 質問

{question}

---

上記のベースコードを出発点として、過去コンペの知見を踏まえた改善案を提案してください：

1. **ベースコードに追加できる特徴量コード**（既存の add_features 関数への追記形式で5個以上）
2. **モデル設定の改善ポイント**（ハイパーパラメータ・学習方法の具体的な変更箇所）
3. **このコンペ固有の注意点**（過去コンペとの違い・ベースコードで未対応の部分）
"""
    else:
        # basebook なし：知識ベースからコードをゼロから提案
        prompt = f"""
## 参照コンペの知識（{source_label}）

{context}

---

## 今参加しているコンペ（{target_comp}）への質問

{question}

---

上記の過去コンペの知見を参照して、以下を提案してください：

1. **今すぐコピーして使える特徴量コード**（5個以上・Pythonで完全に動くもの）
2. **モデル改善の優先順位**（上から3ステップ）
3. **このコンペ固有の注意点**（過去コンペとの違いを踏まえて）
"""

    print("🤖 Claude APIでアドバイスを生成中...")
    resp = client.messages.create(
        model="claude-sonnet-4-6",
        max_tokens=6000,
        system=SYSTEM_PROMPT,
        messages=[{"role": "user", "content": prompt}],
    )
    return resp.content[0].text


def main() -> None:
    validate()

    # 設定ファイルから読み込む
    cfg = load_config()

    target_comp    = cfg["target_comp"]     # 参加中コンペ名（プロンプト用）
    source_comps   = cfg["source_comps"]    # 参照コンペ名リスト（空なら domain 横断）
    domain         = cfg.get("domain")      # ドメイン種別
    extra_keywords = cfg.get("extra_keywords")  # 追加検索キーワード（null 可）
    question       = cfg["question"]        # Claudeへの質問文
    basebook       = cfg.get("basebook")    # ベースnotebook/pyファイル名（null 可）

    # basebook ファイルを読み込む（存在しない場合は None）
    basebook_code = load_basebook(basebook)

    print(f"📋 設定ファイル: {CONFIG_PATH}")
    print(f"   target_comp:  {target_comp}")
    print(f"   source_comps: {source_comps if source_comps else '（空）→ domain横断検索'}")
    print(f"   domain:       {domain}")
    print(f"   extra_keywords: {extra_keywords}")
    print(f"   basebook:     {basebook} {'✅ 読み込み済み' if basebook_code else '（未指定 or ファイルなし）'}")

    advice = get_advice(
        question=question,
        target_comp=target_comp,
        source_comps=source_comps,
        domain=domain,
        extra_keywords=extra_keywords,
        basebook_code=basebook_code,
    )

    print("\n" + "=" * 60)
    print(advice)
    print("=" * 60)

    out_path = Path("advice_output.md")
    out_path.write_text(advice, encoding="utf-8")
    print(f"\n💾 結果を保存: {out_path}")


if __name__ == "__main__":
    main()
