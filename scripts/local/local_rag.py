"""
local_rag.py
SQLiteを使ったクラウドなし版RAGです。
GCPの設定が完了する前の動作確認やローカルテストに使ってください。
BigQueryとほぼ同じインターフェースで動きます。

実行方法:
    cd scripts/
    python local/local_rag.py
"""
import sys
import json
import sqlite3
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import CODELIST_DIR, ANTHROPIC_API_KEY, EXCLUDE_DIRS, REPO_ROOT

import anthropic

DB_PATH = REPO_ROOT / "local_rag.db"   # .gitignore で除外済み
client  = anthropic.Anthropic(api_key=ANTHROPIC_API_KEY)


# ── DB初期化 ──────────────────────────────────────────

def setup_db() -> sqlite3.Connection:
    con = sqlite3.connect(DB_PATH)
    con.execute("""
        CREATE TABLE IF NOT EXISTS documents (
            doc_id     TEXT PRIMARY KEY,
            comp_name  TEXT,
            domain     TEXT,
            doc_type   TEXT,
            title      TEXT,
            content    TEXT,
            score_rank INTEGER,
            votes      INTEGER,
            url        TEXT
        )
    """)
    con.execute("""
        CREATE VIRTUAL TABLE IF NOT EXISTS docs_fts
        USING fts5(
            doc_id, title, content,
            content='documents',
            content_rowid='rowid'
        )
    """)
    con.commit()
    return con


# ── データ投入 ─────────────────────────────────────────

def load_from_repo(con: sqlite3.Connection) -> int:
    """リポジトリを走査してSQLiteに投入します。"""
    total = 0

    for comp_dir in sorted(CODELIST_DIR.iterdir()):
        if not comp_dir.is_dir():
            continue
        if comp_dir.name in EXCLUDE_DIRS or comp_dir.name.startswith("."):
            continue

        # JSONメタデータ
        for json_file in comp_dir.glob("*.json"):
            if "discussion" in json_file.name:
                continue
            try:
                with open(json_file, encoding="utf-8") as f:
                    meta = json.load(f)
                row = {
                    "doc_id":     f"{comp_dir.name}_{json_file.stem}_meta",
                    "comp_name":  comp_dir.name,
                    "domain":     meta.get("domain") or meta.get("type", ""),
                    "doc_type":   "code_meta",
                    "title":      meta.get("title", ""),
                    "content":    meta.get("overview", ""),
                    "score_rank": None,
                    "votes":      int(meta.get("score", 0) * 1000),
                    "url":        "",
                }
                con.execute(
                    "INSERT OR IGNORE INTO documents VALUES "
                    "(:doc_id,:comp_name,:domain,:doc_type,"
                    ":title,:content,:score_rank,:votes,:url)",
                    row,
                )
                total += 1
            except Exception:
                pass

        # Discussion JSON
        for disc_file in comp_dir.rglob("*discussion*.json"):
            try:
                with open(disc_file, encoding="utf-8") as f:
                    discussions = json.load(f)
                for d in discussions:
                    d.setdefault("comp_name", comp_dir.name)
                    con.execute(
                        "INSERT OR IGNORE INTO documents VALUES "
                        "(:doc_id,:comp_name,:domain,:doc_type,"
                        ":title,:content,:score_rank,:votes,:url)",
                        d,
                    )
                    total += 1
            except Exception:
                pass

    con.execute("INSERT INTO docs_fts(docs_fts) VALUES('rebuild')")
    con.commit()
    return total


# ── 検索 ──────────────────────────────────────────────

def search(
    query: str,
    comp_name: str | None = None,
    limit: int = 8,
    con: sqlite3.Connection | None = None,
) -> list[dict]:
    if con is None:
        con = sqlite3.connect(DB_PATH)

    if comp_name:
        sql = """
            SELECT d.title, d.content, d.doc_type, d.comp_name, d.votes
            FROM docs_fts f JOIN documents d ON f.doc_id = d.doc_id
            WHERE docs_fts MATCH ? AND d.comp_name = ?
            ORDER BY d.votes DESC LIMIT ?
        """
        rows = con.execute(sql, (query, comp_name, limit)).fetchall()
    else:
        sql = """
            SELECT d.title, d.content, d.doc_type, d.comp_name, d.votes
            FROM docs_fts f JOIN documents d ON f.doc_id = d.doc_id
            WHERE docs_fts MATCH ?
            ORDER BY d.votes DESC LIMIT ?
        """
        rows = con.execute(sql, (query, limit)).fetchall()

    return [
        {"title": r[0], "content": r[1], "doc_type": r[2],
         "comp_name": r[3], "votes": r[4]}
        for r in rows
    ]


# ── アドバイス生成 ────────────────────────────────────

def get_advice(
    question: str,
    target_comp: str,
    source_comp: str,
    con: sqlite3.Connection,
) -> str:
    keywords = [
        "feature engineering", "LightGBM", "Target Encoding",
        "what worked", "ensemble", "score",
    ]
    docs: list[dict] = []
    seen: set[str] = set()

    for kw in keywords:
        for r in search(kw, source_comp, limit=3, con=con):
            if r["title"] not in seen:
                seen.add(r["title"])
                docs.append(r)

    context = "\n\n".join(
        f"[{d.get('votes', 0)}votes] {d['title']}\n{d['content'][:400]}"
        for d in docs[:8]
    )

    prompt = f"""
## 参照コンペの知見（{source_comp}）
{context}

## 質問（{target_comp}向け）
{question}

知見を参照して日本語で回答してください：
1. 今すぐ試せる特徴量エンジニアリング（Pythonコード付き・5個）
2. モデル改善の優先順位（3ステップ）
3. このドメインで効く手法とその理由
"""
    resp = client.messages.create(
        model="claude-sonnet-4-6",
        max_tokens=2500,
        messages=[{"role": "user", "content": prompt}],
    )
    return resp.content[0].text


# ── メイン ────────────────────────────────────────────

def main() -> None:
    print(f"🗄️  DB: {DB_PATH}")
    con = setup_db()

    count = con.execute("SELECT COUNT(*) FROM documents").fetchone()[0]
    if count == 0:
        print("📥 リポジトリからデータを投入中...")
        total = load_from_repo(con)
        print(f"✅ {total} 件を投入しました")
    else:
        print(f"📊 既存データ: {count} 件")

    # 検索テスト
    print("\n=== 検索テスト: Target Encoding ===")
    for r in search("Target Encoding", limit=3, con=con):
        print(f"  [{r['doc_type']}] {r['title']}")

    # アドバイス生成テスト
    print("\n=== アドバイス生成テスト ===")
    advice = get_advice(
        question="CV AUCが0.891です。特徴量を追加してスコアを上げたい",
        target_comp="playground-series-s5e8",
        source_comp="playground-series-s4e1",
        con=con,
    )
    print(advice[:300] + "...\n（以下省略）")
    con.close()

    print("\n✅ ローカルRAGの動作確認完了")
    print("   GCP設定後は python setup/setup_bigquery.py を実行してください")


if __name__ == "__main__":
    main()
