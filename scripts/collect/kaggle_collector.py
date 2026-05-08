"""
kaggle_collector.py
Kaggle APIを使ってコンペのDiscussionを収集し、
discussions.json として保存します。

実行方法:
    cd scripts/
    python collect/kaggle_collector.py
"""
import sys
import json
import time
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import CODELIST_DIR, KAGGLE_USERNAME, KAGGLE_KEY

import os
os.environ["KAGGLE_USERNAME"] = KAGGLE_USERNAME
os.environ["KAGGLE_KEY"]      = KAGGLE_KEY

import kaggle  # 環境変数をセット後にimport


def collect_discussions(
    comp_slug: str,
    max_topics: int = 30,
) -> list[dict]:
    """Kaggle APIでDiscussionトピックを取得します。"""
    print(f"🔍 Discussion収集中: {comp_slug}")
    rows: list[dict] = []

    try:
        topics = kaggle.api.competition_list_topics(comp_slug)
        for i, topic in enumerate(topics[:max_topics]):
            rows.append({
                "doc_id":     f"{comp_slug}_disc_{i:03d}",
                "comp_name":  comp_slug,
                "domain":     "",
                "doc_type":   "discussion",
                "title":      getattr(topic, "title", ""),
                "content":    getattr(topic, "body", ""),
                "score_rank": None,
                "votes":      getattr(topic, "totalVotes", 0),
                "url": (
                    f"https://www.kaggle.com/competitions/"
                    f"{comp_slug}/discussion/{getattr(topic, 'id', '')}"
                ),
            })
            time.sleep(0.3)
    except Exception as e:
        print(f"⚠️  Kaggle API エラー: {e}")
        print("   手動で discussions.json を作成してください。")

    print(f"   {len(rows)} 件取得")
    return rows


def save_discussions(comp_slug: str, rows: list[dict]) -> None:
    """収集したDiscussionをJSONファイルに保存します。"""
    # コンペフォルダはリポジトリルートに存在する
    out_dir = CODELIST_DIR / comp_slug / "discussions"
    out_dir.mkdir(parents=True, exist_ok=True)

    out_path = out_dir / "discussions.json"
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(rows, f, ensure_ascii=False, indent=2)
    print(f"✅ 保存: {out_path}")


def main() -> None:
    # 収集するコンペを指定（必要に応じて追加）
    target_comps = [
        "playground-series-s4e1",
        "playground-series-s5e8",
    ]
    for comp_slug in target_comps:
        rows = collect_discussions(comp_slug, max_topics=30)
        if rows:
            save_discussions(comp_slug, rows)
        print()


if __name__ == "__main__":
    main()
