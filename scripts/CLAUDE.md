# CLAUDE.md — Kaggle RAG System

## このリポジトリの目的
Kaggleコンペのwinner解法・Discussionを収集し、BigQueryに蓄積して
新しいコンペに参加するときにRAGで過去知識を参照するシステムです。
本業（Webスクールのコンバージョン改善）への応用も目的の一つです。

## リポジトリ構成
```
kaggle_codelist_for_ML/          ← リポジトリルート
├── .env                         ← APIキー（.gitignoreで除外・pushしない）
├── .env.template                ← .envのテンプレート（pushしてよい）
├── .gitignore
│
├── scripts/                     ← RAGシステムのコード（本ファイルと同じ場所）
│   ├── CLAUDE.md                ← この指示書
│   ├── requirements.txt
│   ├── setup/
│   │   └── setup_bigquery.py    ── BQテーブル・インデックス作成（初回のみ）
│   ├── collect/
│   │   ├── kaggle_collector.py  ── KaggleからDiscussionを収集
│   │   └── github_collector.py  ── GitHubからwinner解法を収集
│   ├── process/
│   │   ├── auto_enrich.py       ── JSONをClaudeで自動補完
│   │   ├── code_analyzer.py     ── コードから特徴量ロジックを抽出
│   │   └── load_repo_to_bq.py   ── リポジトリ全体をBQに一括投入
│   ├── search/
│   │   ├── bq_search.py         ── BigQuery FULLTEXT検索
│   │   └── hybrid_search.py     ── 複数キーワード横断検索
│   ├── advisor/
│   │   ├── rag_advisor.py       ── メインのアドバイス生成
│   │   └── medal_advisor.py     ── 複数コンペ横断提案
│   ├── compete/
│   │   ├── baseline_s5e8.py     ── S5E8 LightGBMベースライン
│   │   └── feature_engineering.py ── RAGから取得した特徴量を適用
│   └── local/
│       └── local_rag.py         ── SQLiteベース・クラウドなし版
│
├── playground-series-s4e1-churn/  ← 今回追加したコンペ
│   ├── *.ipynb / *.py
│   ├── *.json                     ← メタデータ（4項目のみ手入力）
│   └── discussions/
│       └── discussions.json
│
└── （既存17コンペのフォルダ・変更なし）
```

## コンペデータのルートパス
スクリプトはリポジトリルート（scripts/の1つ上）を自動で参照します。
明示的に変更する必要はありません。

## 環境変数（リポジトリルートの .env に記載）
- GCP_PROJECT_ID     : GCPプロジェクトID
- BQ_DATASET         : BigQueryデータセット名（デフォルト: kaggle_rag）
- GCS_BUCKET         : Cloud Storageバケット名
- ANTHROPIC_API_KEY  : Claude APIキー
- GITHUB_TOKEN       : GitHub Personal Access Token（90日で期限切れ）
- KAGGLE_USERNAME    : Kaggleユーザー名
- KAGGLE_KEY         : Kaggle APIキー

## よく使うコマンド（scripts/ フォルダから実行）
```bash
cd scripts/

# 初回セットアップ
python setup/setup_bigquery.py

# notebook追加後の定常作業（この2つをセットで実行）
python process/auto_enrich.py
python process/load_repo_to_bq.py

# コンペのアドバイスを取得
python advisor/rag_advisor.py

# ローカルテスト（クラウドなし・GCP設定前の確認用）
python local/local_rag.py

# S5E8ベースライン実行
python compete/baseline_s5e8.py
```

## notebook追加時のワークフロー
1. notebookファイルをコンペフォルダに配置
2. 対応する .json を作成（4項目のみ手入力）
   ```json
   {
     "title": "コンペ名",
     "overview": "解法の概要（2〜3文）",
     "model": ["LightGBM", "XGBoost"],
     "code": "対応するファイル名.ipynb"
   }
   ```
3. `python process/auto_enrich.py`（type/evaluation/domainを自動補完）
4. `python process/load_repo_to_bq.py`（BigQueryに投入）
5. `git add . && git commit -m "Add ..." && git push`

## コーディングルール
- 環境変数は scripts/ の1つ上（リポジトリルート）の .env を読み込む
- BigQueryのテーブル参照は TABLE_REF 定数で統一する
- Claude APIは常に claude-sonnet-4-20250514 を使う
- エラーは try-except で捕捉してログを出力し処理を継続する
- BQへの投入は500件ずつチャンクに分ける

## 注意事項
- .env は絶対にGitHubにpushしない（.gitignoreで除外済み）
- GITHUB_TOKEN は90日で期限切れ → 再発行後に .env を更新する
- Claude APIは従量課金 → auto_enrich.py は新規ファイルにのみ実行する
