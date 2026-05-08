"""
config.py
全スクリプト共通の設定・パス管理モジュールです。
scripts/ の1つ上（リポジトリルート）にある .env を自動で読み込みます。
各スクリプトの冒頭で `from config import *` として使います。
"""
import os
from pathlib import Path
from dotenv import load_dotenv

# ── パス設定 ──────────────────────────────────────────
# このファイルは scripts/ にあるので、1つ上がリポジトリルート
SCRIPTS_DIR  = Path(__file__).parent          # scripts/
REPO_ROOT    = SCRIPTS_DIR.parent             # kaggle_codelist_for_ML/
DOTENV_PATH  = REPO_ROOT / ".env"

# .env をリポジトリルートから読み込む
load_dotenv(DOTENV_PATH)

# ── 環境変数 ──────────────────────────────────────────
PROJECT_ID = os.getenv("GCP_PROJECT_ID", "")
DATASET    = os.getenv("BQ_DATASET", "kaggle_rag")
GCS_BUCKET = os.getenv("GCS_BUCKET", "")
TABLE_REF  = f"{PROJECT_ID}.{DATASET}.documents"

ANTHROPIC_API_KEY = os.getenv("ANTHROPIC_API_KEY", "")
GITHUB_TOKEN      = os.getenv("GITHUB_TOKEN", "")
KAGGLE_USERNAME   = os.getenv("KAGGLE_USERNAME", "")
KAGGLE_KEY        = os.getenv("KAGGLE_KEY", "")

# ── リポジトリ内のコンペデータはルートを参照 ─────────
CODELIST_DIR = REPO_ROOT   # コンペフォルダがあるディレクトリ

# ── 除外するフォルダ名（コンペデータではないフォルダ）──
EXCLUDE_DIRS = {"scripts", ".git", "__pycache__", "data", ".ipynb_checkpoints"}


def validate() -> None:
    """必須の環境変数が設定されているか確認します。"""
    missing = []
    required = {
        "GCP_PROJECT_ID": PROJECT_ID,
        "ANTHROPIC_API_KEY": ANTHROPIC_API_KEY,
    }
    for key, val in required.items():
        if not val:
            missing.append(key)
    if missing:
        raise EnvironmentError(
            f"以下の環境変数が .env に設定されていません: {missing}\n"
            f".env の場所: {DOTENV_PATH}"
        )
