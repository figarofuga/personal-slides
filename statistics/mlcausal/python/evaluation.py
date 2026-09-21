"""Python-only validation artifacts for the cached EconML estimators."""

from pathlib import Path
import copy
import warnings

import cloudpickle
import joblib
import numpy as np
import pandas as pd
from econml.validate import DRTester, EvaluationResults
from sklearn.ensemble import (
    HistGradientBoostingClassifier,
)


warnings.filterwarnings(
    "ignore",
    message="'force_all_finite' was renamed to 'ensure_all_finite'",
    category=FutureWarning,
)


FEATURE_NAMES = ["age", "sexm1", "bmi", "hf", "bnp", "lvef"]
MODEL_NAMES = [
    "S-learner",
    "T-learner",
    "X-learner",
    "R-learner",
    "DR-learner",
]
class _FlatEffectAdapter:
    """Express event-risk CATE as benefit (event-free probability difference)."""

    def __init__(self, estimator):
        self.estimator = estimator

    def effect(self, X, T0=0, T1=1):
        return -np.asarray(
            self.estimator.effect(X, T0=T0, T1=T1)
        ).reshape(-1)


class _PositiveProbabilityClassifier(HistGradientBoostingClassifier):
    """Expose the positive-class probability through predict() for DRTester."""

    def predict(self, X):
        return super().predict_proba(X)[:, 1]


def _toy_frame(toy_data):
    data = pd.DataFrame(toy_data).copy()
    data["id"] = data["id"].astype(int)
    return data


def _split_value(split, name):
    if isinstance(split, dict):
        value = split[name]
    else:
        value = getattr(split, name)
    return np.asarray(value, dtype=int).reshape(-1)


def _partitions(toy_data, split):
    # 学習時に保存した ID を再利用し、訓練例が評価データへ混ざらないようにする。
    data = _toy_frame(toy_data).set_index("id", drop=False)

    def select(partition):
        ids = _split_value(split, f"{partition}_ids")
        selected = data.loc[ids].copy()
        return (
            selected,
            selected.loc[:, FEATURE_NAMES].astype(float),
            selected["bin_outcome"].to_numpy(dtype=int),
            selected["ca"].to_numpy(dtype=int),
        )

    return select("train"), select("test")


def _full_sample(toy_data):
    data = _toy_frame(toy_data)
    return (
        data,
        data.loc[:, FEATURE_NAMES].astype(float),
        data["bin_outcome"].to_numpy(dtype=int),
        data["ca"].to_numpy(dtype=int),
    )


def _output_path(path):
    output = Path(path).expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    return output


def _load_models(model_paths):
    if len(model_paths) != len(MODEL_NAMES):
        raise ValueError("Expected one serialized model for each meta-learner")
    return {
        name: joblib.load(Path(path))
        for name, path in zip(MODEL_NAMES, model_paths)
    }


def write_meta_learner_predictions(
    s_model_path,
    t_model_path,
    x_model_path,
    r_model_path,
    dr_model_path,
    toy_data,
    split,
    output_path,
):
    """Persist test-set CATE predictions without returning pandas to R."""
    (_, _, _, _), (test, X_test, _, _) = _partitions(toy_data, split)
    models = _load_models(
        [
            s_model_path,
            t_model_path,
            x_model_path,
            r_model_path,
            dr_model_path,
        ]
    )

    frames = []
    for model_name, model in models.items():
        # ここで保存するのはイベントリスク差（治療あり − なし）。負なら利益を表す。
        cate = np.asarray(model.effect(X_test, T0=0, T1=1)).reshape(-1)
        frames.append(
            pd.DataFrame(
                {
                    "id": test["id"].to_numpy(dtype=int),
                    "learner": model_name,
                    "cate_rd": cate,
                }
            )
        )

    output = _output_path(output_path)
    pd.concat(frames, ignore_index=True).to_csv(output, index=False)
    return str(output)


def _nuisance_regression_model():
    # DRTester calls predict(), while sklearn classifiers ordinarily return
    # hard labels there. The adapter keeps nuisance predictions on [0, 1].
    return _PositiveProbabilityClassifier(
        learning_rate=0.05,
        max_iter=200,
        max_leaf_nodes=15,
        min_samples_leaf=30,
        l2_regularization=1.0,
        random_state=123,
    )


def _nuisance_propensity_model():
    return HistGradientBoostingClassifier(
        learning_rate=0.05,
        max_iter=150,
        max_leaf_nodes=15,
        min_samples_leaf=30,
        l2_regularization=1.0,
        random_state=123,
    )


def _evaluate_partitions(
    models,
    train_partition,
    validation_partition,
    n_groups,
    n_bootstrap,
):
    """Use one common DR outcome construction for every CATE estimator."""
    _, X_train, Y_train, T_train = train_partition
    _, X_test, Y_test, T_test = validation_partition
    X_train_array = X_train.to_numpy(dtype=float)
    X_test_array = X_test.to_numpy(dtype=float)

    first_model = _FlatEffectAdapter(next(iter(models.values())))
    nuisance_tester = DRTester(
        model_regression=_nuisance_regression_model(),
        model_propensity=_nuisance_propensity_model(),
        cate=first_model,
        cv=5,
    )
    # 効果モデルとは別に評価用の補助モデルを学習し、共通の DR アウトカムを構成する。
    nuisance_tester.fit_nuisance(
        Xval=X_test_array,
        Dval=T_test,
        yval=1 - Y_test,
        Xtrain=X_train_array,
        Dtrain=T_train,
        ytrain=1 - Y_train,
    )

    testers = {}

    for model_name, model in models.items():
        # Copy the fitted nuisance state so every learner is saved as its own
        # complete DRTester object, while all learners still use identical DR outcomes.
        tester = copy.deepcopy(nuisance_tester)
        tester.cate = _FlatEffectAdapter(model)
        tester.get_cate_preds(Xval=X_test_array, Xtrain=X_train_array)

        # evaluate_all() returns an EvaluationResults for this frozen model.
        # Refresh CATE predictions above on every iteration; nuisance DR outcomes
        # are shared by all five models. Positive values now mean risk reduction.
        np.random.seed(123)
        result = tester.evaluate_all(
            n_groups=n_groups, n_bootstrap=n_bootstrap
        )
        assert isinstance(result, EvaluationResults)
        testers[model_name] = tester

    return testers


def _evaluate_external_models(
    models,
    development_data,
    validation_data,
    n_groups,
    n_bootstrap,
):
    return _evaluate_partitions(
        models,
        _full_sample(development_data),
        _full_sample(validation_data),
        n_groups,
        n_bootstrap,
    )


def _model_slug(model_name):
    return model_name.lower().replace("-", "_").replace(" ", "_")


def _write_drtester_artifacts(testers, output_dir):
    """Save each fitted DRTester and its model-specific raw plotting tables."""
    directory = _output_path(output_dir)
    directory.mkdir(parents=True, exist_ok=True)
    paths = []

    for model_name, tester in testers.items():
        slug = _model_slug(model_name)
        result = tester.res

        tester_path = directory / f"{slug}-drtester.joblib"
        summary_path = directory / f"{slug}-summary.csv"
        gate_path = directory / f"{slug}-gate.csv"
        toc_path = directory / f"{slug}-toc.csv"
        qini_path = directory / f"{slug}-qini.csv"

        # source_python() defines the small adapters dynamically. cloudpickle
        # embeds those definitions, so the DRTester can be loaded later from a
        # plain Python session (joblib.load remains compatible with this file).
        with tester_path.open("wb") as stream:
            cloudpickle.dump(tester, stream)

        summary = result.summary().copy()
        summary.insert(0, "learner", model_name)
        summary.to_csv(summary_path, index=False)

        gate = result.cal.plot_data_dict[1].copy()
        gate.insert(0, "learner", model_name)
        gate["group"] = gate["ind"].astype(int) + 1
        gate.drop(columns="ind").to_csv(gate_path, index=False)

        for metric, uplift_result, path in (
            ("TOC", result.toc, toc_path),
            ("Qini", result.qini, qini_path),
        ):
            curve = uplift_result.curves[1].copy()
            curve.insert(0, "learner", model_name)
            curve.insert(1, "curve", metric)
            curve.to_csv(path, index=False)

        paths.extend(
            [tester_path, summary_path, gate_path, toc_path, qini_path]
        )

    return [str(path) for path in paths]


def evaluate_external_meta_learners(
    s_model_path,
    t_model_path,
    x_model_path,
    r_model_path,
    dr_model_path,
    development_data,
    validation_data,
    drtester_dir,
    n_groups=5,
    n_bootstrap=1000,
):
    """Validate frozen, fully refitted models in a separate external cohort."""
    models = _load_models(
        [
            s_model_path,
            t_model_path,
            x_model_path,
            r_model_path,
            dr_model_path,
        ]
    )
    testers = _evaluate_external_models(
        models,
        development_data,
        validation_data,
        n_groups=int(n_groups),
        n_bootstrap=int(n_bootstrap),
    )
    return _write_drtester_artifacts(testers, drtester_dir)
