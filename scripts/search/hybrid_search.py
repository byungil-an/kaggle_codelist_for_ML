"""
hybrid_search.py
複数キーワードで横断検索し、重複除去して上位ドキュメントを返します。
advisor/ スクリプトから呼び出して使います。

実行方法:
    cd scripts/
    python search/hybrid_search.py
"""
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from search.bq_search import search

DOMAIN_KEYWORDS: dict[str, list[str]] = {
    "churn": [
        "Target Encoding", "feature engineering", "LightGBM",
        "what worked", "ensemble", "balance ratio",
    ],
    "finance": [
        "credit risk", "feature engineering", "XGBoost",
        "ROC-AUC", "what worked", "imbalanced",
    ],
    "ecommerce": [
        "purchase prediction", "user behavior", "Target Encoding",
        "LightGBM", "what worked", "session",
    ],
    "health": [
        "feature engineering", "LightGBM", "what worked",
        "log transform", "BMI", "interaction",
    ],
    "sales": [
        "time series", "lag features", "rolling mean",
        "LightGBM", "XGBoost", "what worked",
    ],
}

DEFAULT_KEYWORDS = [
    "feature engineering", "LightGBM", "Target Encoding",
    "what worked", "ensemble", "score improve",
]


def hybrid_search(
    source_comp: str,
    domain: str | None = None,
    extra_keywords: list[str] | None = None,
    limit_per_kw: int = 3,
    total_limit: int = 12,
) -> list[dict]:
    """
    複数キーワードで検索し、重複除去して統合結果を返します。

    Args:
        source_comp:    参照するコンペ名
        domain:         ドメイン（例: "churn"）
        extra_keywords: 追加キーワード
        limit_per_kw:   キーワードごとの取得件数
        total_limit:    最終的な最大件数
    """
    keywords = list(DOMAIN_KEYWORDS.get(domain or "", DEFAULT_KEYWORDS))
    if extra_keywords:
        keywords = extra_keywords + keywords

    all_docs: list[dict] = []
    seen: set[str] = set()

    for kw in keywords:
        for r in search(query=kw, comp_name=source_comp, limit=limit_per_kw):
            if r["title"] not in seen:
                seen.add(r["title"])
                all_docs.append(r)

    priority = {
        "code_feature": 0, "code_summary": 1,
        "discussion": 2,   "code_meta": 3,
    }
    all_docs.sort(
        key=lambda x: (
            priority.get(x["doc_type"], 9),
            -(x.get("votes") or 0),
        )
    )
    return all_docs[:total_limit]


def build_context(docs: list[dict]) -> str:
    """ドキュメントリストをプロンプト用コンテキスト文字列に変換します。"""
    code_docs = [d for d in docs if d["doc_type"] in ("code_feature", "code_summary")]
    disc_docs  = [d for d in docs if d["doc_type"] == "discussion"]

    parts: list[str] = []

    if code_docs:
        parts.append("### 過去解法コード・特徴量")
        for d in code_docs:
            parts.append(f"**{d['title']}** ({d['comp_name']})")
            parts.append(f"```python\n{d['content'][:500]}\n```")

    if disc_docs:
        parts.append("\n### Discussionの知見")
        for d in disc_docs:
            votes = d.get("votes") or 0
            parts.append(f"**[{votes}votes] {d['title']}**")
            parts.append(d["content"][:400])

    return "\n\n".join(parts)


if __name__ == "__main__":
    docs = hybrid_search(
        source_comp="playground-series-s4e1",
        domain="churn",
        total_limit=8,
    )
    print(f"取得件数: {len(docs)}")
    for d in docs:
        print(f"  [{d['doc_type']}] {d['title']}")

    print("\n=== コンテキスト文字列（先頭400文字）===")
    print(build_context(docs)[:400])
