"""
github_collector.py
GitHub APIを使ってKaggleコンペのwinner解法を検索し、
Pythonファイルを Cloud Storage に保存します。
Week 3以降に使用してください。

実行方法:
    cd scripts/
    python collect/github_collector.py
"""
import sys
import json
import time
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import PROJECT_ID, GCS_BUCKET, GITHUB_TOKEN, validate

from github import Github
from google.cloud import storage


def search_kaggle_solutions(
    comp_keyword: str,
    max_repos: int = 10,
) -> list:
    """GitHubでKaggle解法リポジトリを検索します。"""
    g = Github(GITHUB_TOKEN)
    query = f"kaggle {comp_keyword} solution notebook"
    print(f"🔍 GitHub検索: {query}")

    repos = []
    try:
        results = g.search_repositories(query, sort="stars")
        for repo in results[:max_repos]:
            repos.append(repo)
            print(f"   ★{repo.stargazers_count:4d}  {repo.full_name}")
            time.sleep(0.5)
    except Exception as e:
        print(f"⚠️  GitHub API エラー: {e}")

    return repos


def get_python_files(repo) -> list[dict]:
    """リポジトリ内のPythonファイルを取得します。"""
    files = []
    try:
        for item in repo.get_contents(""):
            if item.name.endswith((".py", ".ipynb")):
                try:
                    code = item.decoded_content.decode("utf-8", errors="ignore")
                    files.append({
                        "filename": item.name,
                        "content":  code,
                        "repo":     repo.full_name,
                        "stars":    repo.stargazers_count,
                        "url":      item.html_url,
                    })
                    time.sleep(0.2)
                except Exception:
                    pass
    except Exception as e:
        print(f"   ⚠️  コンテンツ取得エラー: {e}")

    return files


def save_to_gcs(data: dict, blob_name: str) -> None:
    """GCSにJSONとして保存します。"""
    client = storage.Client(project=PROJECT_ID)
    bucket = client.bucket(GCS_BUCKET)
    bucket.blob(blob_name).upload_from_string(
        json.dumps(data, ensure_ascii=False),
        content_type="application/json",
    )


def collect(comp_keyword: str, max_repos: int = 10) -> None:
    """検索 → ファイル取得 → GCS保存。"""
    validate()
    repos = search_kaggle_solutions(comp_keyword, max_repos)
    total = 0

    for repo in repos:
        files = get_python_files(repo)
        for f in files:
            blob_name = (
                f"solutions/{comp_keyword}/"
                f"{repo.full_name.replace('/', '_')}_{f['filename']}.json"
            )
            save_to_gcs(f, blob_name)
            total += 1

    print(f"\n✅ {total} ファイルをGCSに保存しました")
    print("   次のステップ: python process/code_analyzer.py")


if __name__ == "__main__":
    collect("playground-series-s4e1-churn", max_repos=10)
