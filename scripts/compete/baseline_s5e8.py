"""
baseline_s5e8.py
Playground Series S5E8 の LightGBM ベースラインです。
5-Fold CVでスコアを確認し、submission.csv を生成します。

実行方法:
    cd scripts/
    python compete/baseline_s5e8.py
    ※ データは scripts/../data/s5e8/ に配置してください
"""
import sys
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent))
from config import REPO_ROOT

import numpy as np
import pandas as pd
import lightgbm as lgb
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import roc_auc_score

# ── 設定 ──────────────────────────────────────────────
DATA_DIR = REPO_ROOT / "data" / "s5e8"
TARGET   = "Exited"   # データの列名を確認して修正してください
ID_COL   = "id"
N_SPLITS = 5
SEED     = 42

LGBM_PARAMS = {
    "objective":         "binary",
    "metric":            "auc",
    "learning_rate":     0.05,
    "num_leaves":        64,
    "min_child_samples": 20,
    "feature_fraction":  0.8,
    "bagging_fraction":  0.8,
    "bagging_freq":      1,
    "reg_alpha":         0.1,
    "reg_lambda":        0.1,
    "verbosity":         -1,
    "random_state":      SEED,
}


def load_data() -> tuple[pd.DataFrame, pd.DataFrame]:
    train = pd.read_csv(DATA_DIR / "train.csv")
    test  = pd.read_csv(DATA_DIR / "test.csv")
    print(f"train: {train.shape},  test: {test.shape}")
    print(f"columns: {train.columns.tolist()}")
    print(f"target:\n{train[TARGET].value_counts()}")
    return train, test


def preprocess(
    train: pd.DataFrame,
    test: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame, list[str]]:
    cat_cols = [
        c for c in train.select_dtypes("object").columns
        if c not in [TARGET, ID_COL]
    ]
    for col in cat_cols:
        train[col] = train[col].astype("category")
        test[col]  = test[col].astype("category")

    feature_cols = [c for c in train.columns if c not in [TARGET, ID_COL]]
    print(f"特徴量数: {len(feature_cols)}")
    return train, test, feature_cols


def train_and_predict(
    train: pd.DataFrame,
    test: pd.DataFrame,
    feature_cols: list[str],
) -> tuple[np.ndarray, np.ndarray]:
    X      = train[feature_cols]
    y      = train[TARGET]
    X_test = test[feature_cols]

    oof_preds  = np.zeros(len(train))
    test_preds = np.zeros(len(test))
    skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=SEED)

    for fold, (tr_idx, val_idx) in enumerate(skf.split(X, y), 1):
        model = lgb.LGBMClassifier(**LGBM_PARAMS, n_estimators=2000)
        model.fit(
            X.iloc[tr_idx], y.iloc[tr_idx],
            eval_set=[(X.iloc[val_idx], y.iloc[val_idx])],
            callbacks=[
                lgb.early_stopping(100, verbose=False),
                lgb.log_evaluation(200),
            ],
        )
        oof_preds[val_idx]  = model.predict_proba(X.iloc[val_idx])[:, 1]
        test_preds          += model.predict_proba(X_test)[:, 1] / N_SPLITS

        fold_auc = roc_auc_score(y.iloc[val_idx], oof_preds[val_idx])
        print(f"  Fold {fold}: AUC = {fold_auc:.5f}")

    cv_auc = roc_auc_score(y, oof_preds)
    print(f"\n✅ CV AUC: {cv_auc:.5f}  ← このスコアを記録してください")
    return oof_preds, test_preds


def save_submission(test: pd.DataFrame, test_preds: np.ndarray) -> None:
    out_path = REPO_ROOT / "submission_baseline.csv"
    sub = test[[ID_COL]].copy()
    sub[TARGET] = test_preds
    sub.to_csv(out_path, index=False)
    print(f"💾 {out_path}")


def main() -> None:
    print("=" * 50)
    print("S5E8 LightGBM ベースライン")
    print("=" * 50)

    train, test            = load_data()
    train, test, feat_cols = preprocess(train, test)
    _, test_preds          = train_and_predict(train, test, feat_cols)
    save_submission(test, test_preds)

    print("\n次のステップ:")
    print("  1. スコアを記録して advisor/rag_advisor.py の QUESTION に入力")
    print("  2. python advisor/rag_advisor.py でアドバイスを取得")
    print("  3. python compete/feature_engineering.py で特徴量を追加")


if __name__ == "__main__":
    main()
