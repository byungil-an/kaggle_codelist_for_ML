"""
feature_engineering.py
RAGアドバイザーの提案を受けて特徴量を追加し、スコア改善を確認します。
baseline_s5e8.py の後に実行してください。

実行方法:
    cd scripts/
    python compete/feature_engineering.py
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

DATA_DIR      = REPO_ROOT / "data" / "s5e8"
TARGET        = "Exited"
ID_COL        = "id"
N_SPLITS      = 5
SEED          = 42
BASELINE_SCORE = 0.891   # baseline_s5e8.py のCVスコアを入力

LGBM_PARAMS = {
    "objective":         "binary",
    "metric":            "auc",
    "learning_rate":     0.05,
    "num_leaves":        64,
    "feature_fraction":  0.8,
    "bagging_fraction":  0.8,
    "bagging_freq":      1,
    "reg_alpha":         0.1,
    "reg_lambda":        0.1,
    "verbosity":         -1,
    "random_state":      SEED,
}


# ══════════════════════════════════════════════════════
# 特徴量エンジニアリング関数
# ── RAG アドバイザーが提案した特徴量をここに追加する ──
# ══════════════════════════════════════════════════════

def add_ratio_features(df: pd.DataFrame) -> pd.DataFrame:
    """残高・給与比率など数値の比率特徴量（S4E1で有効）。"""
    if "Balance" in df.columns and "EstimatedSalary" in df.columns:
        df["balance_salary_ratio"] = df["Balance"] / (df["EstimatedSalary"] + 1)
    if "Balance" in df.columns:
        df["has_balance"] = (df["Balance"] > 0).astype(int)
    return df


def add_interaction_features(df: pd.DataFrame) -> pd.DataFrame:
    """NumOfProducts と他列の交差特徴量（S4E1上位解法で一貫して有効）。"""
    if "NumOfProducts" in df.columns and "Balance" in df.columns:
        df["products_x_balance"] = df["NumOfProducts"] * df["Balance"]
    if "NumOfProducts" in df.columns and "IsActiveMember" in df.columns:
        df["products_x_active"] = df["NumOfProducts"] * df["IsActiveMember"]
    if "Age" in df.columns and "Tenure" in df.columns:
        df["age_tenure_ratio"] = df["Age"] / (df["Tenure"] + 1)
    return df


def add_age_features(df: pd.DataFrame) -> pd.DataFrame:
    """年齢のバケット化・非線形変換。"""
    if "Age" in df.columns:
        df["age_group"] = pd.cut(
            df["Age"], bins=[0, 35, 50, 100], labels=[0, 1, 2]
        ).astype(float)
        df["age_squared"] = df["Age"] ** 2
    return df


def add_target_encoding(
    train: pd.DataFrame,
    test: pd.DataFrame,
    cat_cols: list[str],
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """OOF方式のTarget Encoding（リーク防止済み）。"""
    skf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=SEED)
    train = train.copy()
    test  = test.copy()

    for col in cat_cols:
        new_col = f"{col}_te"
        train[new_col] = np.nan

        for tr_idx, val_idx in skf.split(train, train[TARGET]):
            means = train.iloc[tr_idx].groupby(col)[TARGET].mean()
            train.loc[train.index[val_idx], new_col] = (
                train.iloc[val_idx][col].map(means)
            )

        overall_means       = train.groupby(col)[TARGET].mean()
        test[new_col]       = test[col].map(overall_means)
        global_mean         = train[TARGET].mean()
        train[new_col]      = train[new_col].fillna(global_mean)
        test[new_col]       = test[new_col].fillna(global_mean)
        print(f"  Target Encoding: {col} → {new_col}")

    return train, test


def add_all_features(
    train: pd.DataFrame,
    test: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """すべての特徴量エンジニアリングを適用します。"""
    print("🔧 特徴量エンジニアリングを適用中...")
    for df in [train, test]:
        add_ratio_features(df)
        add_interaction_features(df)
        add_age_features(df)

    cat_cols = [
        c for c in train.select_dtypes(["object", "category"]).columns
        if c not in [TARGET, ID_COL]
    ]
    if cat_cols:
        train, test = add_target_encoding(train, test, cat_cols)

    return train, test


def run_cv(
    train: pd.DataFrame,
    test: pd.DataFrame,
    feature_cols: list[str],
) -> tuple[float, np.ndarray]:
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
                lgb.log_evaluation(500),
            ],
        )
        oof_preds[val_idx]  = model.predict_proba(X.iloc[val_idx])[:, 1]
        test_preds          += model.predict_proba(X_test)[:, 1] / N_SPLITS

    cv_score = roc_auc_score(y, oof_preds)
    return cv_score, test_preds


def main() -> None:
    train = pd.read_csv(DATA_DIR / "train.csv")
    test  = pd.read_csv(DATA_DIR / "test.csv")

    cat_cols = [
        c for c in train.select_dtypes("object").columns
        if c not in [TARGET, ID_COL]
    ]
    for col in cat_cols:
        train[col] = train[col].astype("category")
        test[col]  = test[col].astype("category")

    train, test  = add_all_features(train, test)
    feature_cols = [c for c in train.columns if c not in [TARGET, ID_COL]]
    print(f"📊 特徴量数: {len(feature_cols)}")

    cv_score, test_preds = run_cv(train, test, feature_cols)
    improvement = cv_score - BASELINE_SCORE

    print(f"\n✅ CV AUC: {cv_score:.5f}")
    print(f"📈 ベースラインからの改善: {improvement:+.5f}")

    out_path = REPO_ROOT / "submission_fe.csv"
    sub = test[[ID_COL]].copy()
    sub[TARGET] = test_preds
    sub.to_csv(out_path, index=False)
    print(f"💾 {out_path}")


if __name__ == "__main__":
    main()
