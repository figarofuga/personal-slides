"""EconML model fitting and cached prediction helpers for the targets DAG."""

from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from econml.dml import NonParamDML
from econml.dr import DRLearner
from econml.metalearners import SLearner, TLearner, XLearner
from sklearn.ensemble import (
    HistGradientBoostingClassifier,
    HistGradientBoostingRegressor,
)
from sklearn.model_selection import train_test_split
from xgboost import XGBRegressor


FEATURE_NAMES = ["age", "sexm1", "bmi", "hf", "bnp", "lvef"]

XGB_BINARY_PARAMS = {
    "objective": "binary:logistic",
    "eval_metric": "logloss",
    "n_estimators": 300,
    "learning_rate": 0.05,
    "max_depth": 3,
    "min_child_weight": 10,
    "subsample": 0.8,
    "colsample_bytree": 0.8,
    "reg_lambda": 1.0,
    "random_state": 123,
    "n_jobs": -1,
}


def _toy_frame(toy_data):
    # R から渡された表をコピーし、元データを変更せず ID の型を揃える。
    data = pd.DataFrame(toy_data).copy()
    data["id"] = data["id"].astype(int)
    return data


def _split_value(split, name):
    # reticulate が返す辞書と、属性として要素を持つオブジェクトの両方に対応する。
    if isinstance(split, dict):
        value = split[name]
    else:
        value = getattr(split, name)
    return np.asarray(value, dtype=int).reshape(-1)


def _partition(toy_data, split, partition):
    data = _toy_frame(toy_data).set_index("id", drop=False)
    ids = _split_value(split, f"{partition}_ids")
    # 行番号ではなく保存済み ID で抽出し、全モデルで同じ個体・順序を使う。
    selected = data.loc[ids].copy()
    X = selected.loc[:, FEATURE_NAMES].astype(float)
    Y = selected["bin_outcome"].astype(int)
    T = selected["ca"].astype(int)
    return selected, X, Y, T


def _model_path(path):
    output = Path(path).expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    return output


def _dump_model(model, path):
    output = _model_path(path)
    # targets は返されたパスを追跡する。学習済み Python オブジェクトは joblib に保存する。
    joblib.dump(model, output)
    return str(output)


def _xgb_outcome_model():
    # 二値アウトカムの確率を predict() で返すため、logistic 目的関数の回帰器を使う。
    return XGBRegressor(**XGB_BINARY_PARAMS)


def make_ml_split(toy_data, test_size=0.2, random_state=123):
    """Return stable subject IDs for the exact split used in the slides."""
    data = _toy_frame(toy_data)
    treatment = data["ca"].astype(int)
    outcome = data["bin_outcome"].astype(int)
    # 治療 × イベントの 4 層で層化し、訓練・評価データの構成を揃える。
    strata = treatment.astype(str) + "_" + outcome.astype(str)

    train_ids, test_ids = train_test_split(
        data["id"].to_numpy(),
        test_size=test_size,
        random_state=random_state,
        stratify=strata,
    )

    return {
        "train_ids": np.asarray(train_ids, dtype=int),
        "test_ids": np.asarray(test_ids, dtype=int),
    }


def make_full_training_split(toy_data):
    """Use every development-cohort subject when refitting for external validation."""
    data = _toy_frame(toy_data)
    return {
        "train_ids": data["id"].to_numpy(dtype=int),
        # The fitting helpers only consume train_ids. Keep the same interface
        # without pretending that a development-cohort holdout is external data.
        "test_ids": np.asarray([], dtype=int),
    }


def fit_s_learner(toy_data, split, model_path):
    _, X_train, Y_train, T_train = _partition(toy_data, split, "train")
    # S-learner：治療も説明変数として一つのモデルに含め、治療を切り替えた予測を比較する。
    learner = SLearner(overall_model=_xgb_outcome_model())
    learner.fit(Y_train, T_train, X=X_train)
    return _dump_model(learner, model_path)


def fit_t_learner(toy_data, split, model_path):
    _, X_train, Y_train, T_train = _partition(toy_data, split, "train")
    # T-learner：治療群・対照群で別々にアウトカムを学習し、その予測差を効果とする。
    learner = TLearner(models=_xgb_outcome_model())
    learner.fit(Y_train, T_train, X=X_train)
    return _dump_model(learner, model_path)


def fit_x_learner(toy_data, split, model_path):
    _, X_train, Y_train, T_train = _partition(toy_data, split, "train")

    cate_model = HistGradientBoostingRegressor(
        learning_rate=0.05,
        max_iter=200,
        max_leaf_nodes=15,
        min_samples_leaf=30,
        l2_regularization=1.0,
        random_state=123,
    )
    propensity_model = HistGradientBoostingClassifier(
        learning_rate=0.05,
        max_iter=150,
        max_leaf_nodes=15,
        min_samples_leaf=30,
        l2_regularization=1.0,
        random_state=123,
    )

    # X-learner：補完した個体別効果を学習し、傾向スコアで両群の効果予測を合成する。
    learner = XLearner(
        models=_xgb_outcome_model(),
        cate_models=cate_model,
        propensity_model=propensity_model,
    )
    learner.fit(Y_train, T_train, X=X_train)
    return _dump_model(learner, model_path)


def fit_r_learner(toy_data, split, model_path):
    _, X_train, Y_train, T_train = _partition(toy_data, split, "train")

    # R-learner：5 分割の cross-fitting でアウトカムと治療を残差化し、効果を回帰する。
    learner = NonParamDML(
        model_y=HistGradientBoostingClassifier(
            learning_rate=0.05,
            max_iter=200,
            max_leaf_nodes=15,
            min_samples_leaf=30,
            l2_regularization=1.0,
            random_state=123,
        ),
        model_t=HistGradientBoostingClassifier(
            learning_rate=0.05,
            max_iter=150,
            max_leaf_nodes=15,
            min_samples_leaf=30,
            l2_regularization=1.0,
            random_state=123,
        ),
        model_final=HistGradientBoostingRegressor(
            learning_rate=0.05,
            max_iter=200,
            max_leaf_nodes=15,
            min_samples_leaf=30,
            l2_regularization=1.0,
            random_state=123,
        ),
        discrete_outcome=True,
        discrete_treatment=True,
        cv=5,
        random_state=123,
    )
    learner.fit(Y_train, T_train, X=X_train)
    return _dump_model(learner, model_path)


def fit_dr_learner(toy_data, split, model_path):
    _, X_train, Y_train, T_train = _partition(toy_data, split, "train")

    # DR-learner：アウトカム予測と傾向スコアから二重頑健な擬似アウトカムを作る。
    # min_propensity は極端な逆確率重みによる不安定化を抑える。
    learner = DRLearner(
        model_propensity=HistGradientBoostingClassifier(
            learning_rate=0.05,
            max_iter=150,
            max_leaf_nodes=15,
            min_samples_leaf=30,
            l2_regularization=1.0,
            random_state=123,
        ),
        model_regression=HistGradientBoostingClassifier(
            learning_rate=0.05,
            max_iter=200,
            max_leaf_nodes=15,
            min_samples_leaf=30,
            l2_regularization=1.0,
            random_state=123,
        ),
        model_final=HistGradientBoostingRegressor(
            learning_rate=0.05,
            max_iter=200,
            max_leaf_nodes=15,
            min_samples_leaf=30,
            l2_regularization=1.0,
            random_state=123,
        ),
        discrete_outcome=True,
        min_propensity=0.01,
        cv=5,
        random_state=123,
    )
    learner.fit(Y_train, T_train, X=X_train)
    return _dump_model(learner, model_path)
