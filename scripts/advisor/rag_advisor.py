"""
rag_advisor.py
BigQueryの知識ベースを参照して、Claude APIがコンペのアドバイスを生成します。
コンペに参加するたびに実行してください。

実行方法:
    cd scripts/
    python advisor/rag_advisor.py
"""
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import ANTHROPIC_API_KEY, validate
from search.hybrid_search import hybrid_search, build_context

import anthropic

client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)

SYSTEM_PROMPT = """
あなたはKaggleコンペのメダル獲得を支援するAIアドバイザーです。
過去のwinner解法とDiscussionの知見を参照し、
実際にそのまま使えるPythonコードとともに改善策を提案してください。
回答は必ず日本語で行ってください。
"""


def get_advice(
    question: str,
    target_comp: str,
    source_comp: str,
    domain: str | None = None,
    extra_keywords: list[str] | None = None,
) -> str:
    """
    RAGでアドバイスを生成します。

    Args:
        question:       ユーザーの質問
        target_comp:    今参加しているコンペ名
        source_comp:    参照する過去コンペ名
        domain:         ドメイン（例: "churn"）
        extra_keywords: 追加検索キーワード
    """
    print(f"🔍 BigQueryを検索中: {source_comp}")
    docs = hybrid_search(
        source_comp=source_comp,
        domain=domain,
        extra_keywords=extra_keywords,
        total_limit=12,
    )
    print(f"   {len(docs)} 件の知識を取得")

    context = build_context(docs)

    prompt = f"""
## 参照コンペの知識（{source_comp}）

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
        model="claude-sonnet-4-20250514",
        max_tokens=3000,
        system=SYSTEM_PROMPT,
        messages=[{"role": "user", "content": prompt}],
    )
    return resp.content[0].text


def main() -> None:
    # ── ここを変更して使う ────────────────────────────
    TARGET_COMP    = "playground-series-s5e8"
    SOURCE_COMP    = "playground-series-s4e1"
    DOMAIN         = "churn"
    EXTRA_KEYWORDS = None  # 追加したいキーワードがあれば ["XGBoost"] 等

    QUESTION = """
    現在S5E8に参加中です。
    LightGBMのベースラインでCV AUCが0.891まで来ました。
    次にどの特徴量を追加すればスコアが上がりますか？
    具体的なPythonコードで教えてください。
    """
    # ─────────────────────────────────────────────────

    validate()
    advice = get_advice(
        question=QUESTION,
        target_comp=TARGET_COMP,
        source_comp=SOURCE_COMP,
        domain=DOMAIN,
        extra_keywords=EXTRA_KEYWORDS,
    )

    print("\n" + "=" * 60)
    print(advice)
    print("=" * 60)

    out_path = Path("advice_output.md")
    out_path.write_text(advice, encoding="utf-8")
    print(f"\n💾 結果を保存: {out_path}")


if __name__ == "__main__":
    main()
