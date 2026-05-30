"""
medal_advisor.py
複数の過去コンペを横断参照して、メダル獲得に向けた
包括的なアドバイスを生成します。
Week 2以降、スコアが伸び悩んだときに使ってください。

実行方法:
    cd scripts/
    python advisor/medal_advisor.py
"""
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import ANTHROPIC_API_KEY, validate
from search.bq_search import search

import anthropic

client = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)


def multi_comp_search(
    query: str,
    comp_names: list[str],
    limit_per_comp: int = 4,
) -> list[dict]:
    """複数コンペを横断して検索します。"""
    all_docs: list[dict] = []
    seen: set[str] = set()

    for comp in comp_names:
        for r in search(query=query, comp_name=comp, limit=limit_per_comp):
            if r["title"] not in seen:
                seen.add(r["title"])
                all_docs.append(r)

    return all_docs


def get_medal_advice(
    target_comp: str,
    current_score: float,
    source_comps: list[str],
    question: str,
) -> str:
    """
    複数の過去コンペを参照してメダル獲得アドバイスを生成します。

    Args:
        target_comp:   今参加しているコンペ名
        current_score: 現在のCVスコア（例: 0.891）
        source_comps:  参照するコンペ名のリスト
        question:      具体的な質問
    """
    keywords = [
        "what worked", "ensemble", "stacking",
        "feature engineering", "score improve", "medal",
    ]

    all_docs: list[dict] = []
    seen: set[str] = set()
    for kw in keywords:
        for d in multi_comp_search(kw, source_comps, limit_per_comp=3):
            if d["title"] not in seen:
                seen.add(d["title"])
                all_docs.append(d)

    code_ctx = "\n\n".join(
        f"### [{d['comp_name']}] {d['title']}\n```\n{d['content'][:400]}\n```"
        for d in all_docs
        if d["doc_type"] in ("code_feature", "code_summary")
    )[:4000]

    disc_ctx = "\n\n".join(
        f"### [{d['comp_name']} / {d.get('votes',0)}votes] {d['title']}\n{d['content'][:300]}"
        for d in all_docs
        if d["doc_type"] == "discussion"
    )[:3000]

    prompt = f"""
## 参照コンペ
{', '.join(source_comps)}

## 過去解法コード・特徴量
{code_ctx}

## Discussion知見
{disc_ctx}

---

## 現在の状況
- コンペ: {target_comp}
- 現在のCVスコア: {current_score}
- 質問: {question}

---

メダル獲得に向けて以下を提案してください：

1. **次の一手（最優先でやること）**
   - 理由と期待スコア改善幅も含めて

2. **特徴量エンジニアリング（コード付き・3〜5個）**
   - 過去コンペで実際に効いたものを優先

3. **アンサンブル戦略**
   - 今のスコアから上位何%を狙えるか

4. **やってはいけないこと（過去コンペの失敗例）**
"""

    resp = client.messages.create(
        model="claude-sonnet-4-6",
        max_tokens=3000,
        system="あなたはKaggleメダル獲得を支援するAIです。日本語で回答してください。",
        messages=[{"role": "user", "content": prompt}],
    )
    return resp.content[0].text


def main() -> None:
    # ── ここを変更して使う ────────────────────────────
    TARGET_COMP   = "playground-series-s5e8"
    CURRENT_SCORE = 0.891

    SOURCE_COMPS = [
        "playground-series-s4e1",
        "Home Credit Default Risk",
    ]

    QUESTION = """
    CV AUCが0.891で止まっています。
    アンサンブルを試したいのですが、LightGBM・XGBoost・CatBoostの
    最適な組み合わせ方と、その前に試すべき特徴量改善を教えてください。
    """
    # ─────────────────────────────────────────────────

    validate()
    advice = get_medal_advice(
        target_comp=TARGET_COMP,
        current_score=CURRENT_SCORE,
        source_comps=SOURCE_COMPS,
        question=QUESTION,
    )

    print("\n" + "=" * 60)
    print(advice)
    print("=" * 60)

    out_path = Path("medal_advice_output.md")
    out_path.write_text(advice, encoding="utf-8")
    print(f"\n💾 結果を保存: {out_path}")


if __name__ == "__main__":
    main()
