"""Cached PDP and Kernel SHAP values for EconML models."""

from pathlib import Path
import warnings

import joblib
import numpy as np
import pandas as pd
import shap


warnings.filterwarnings(
    "ignore",
    message="'force_all_finite' was renamed to 'ensure_all_finite'",
    category=FutureWarning,
)


FEATURE_NAMES = ["age", "sexm1", "bmi", "hf", "bnp", "lvef"]
EXPLAIN_FEATURES = ["age", "lvef", "bnp"]
MODEL_NAMES = [
    "S-learner",
    "T-learner",
    "X-learner",
    "R-learner",
    "DR-learner",
]


def _output_path(path):
    output = Path(path).expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    return output


def _load_models(model_paths):
    return {
        name: joblib.load(Path(path))
        for name, path in zip(MODEL_NAMES, model_paths)
    }


def _features(toy_data):
    return pd.DataFrame(toy_data).loc[:, FEATURE_NAMES].astype(float)


def _predict_benefit(model, data):
    frame = pd.DataFrame(data, columns=FEATURE_NAMES).astype(float)
    # イベントリスク差の符号を反転し、正の値ほど利益が大きい尺度にする。
    return -np.asarray(model.effect(frame, T0=0, T1=1)).reshape(-1)


def _pdp_curves(models, features):
    # 全モデルで参照集団とグリッドを共有し、モデル間で比較可能にする。
    reference = features.sample(n=min(500, len(features)), random_state=42)
    grids = {
        feature: np.linspace(
            reference[feature].quantile(0.05),
            reference[feature].quantile(0.95),
            25,
        )
        for feature in EXPLAIN_FEATURES
    }

    frames = []
    for model_name, model in models.items():
        for feature in EXPLAIN_FEATURES:
            grid = grids[feature]
            newdata = pd.concat(
                [reference.copy() for _ in grid], ignore_index=True
            )
            newdata[feature] = np.repeat(grid, len(reference))
            prediction = _predict_benefit(model, newdata)
            frames.append(
                pd.DataFrame(
                    {
                        "learner": model_name,
                        "feature": feature,
                        "value": grid,
                        "hte": prediction.reshape(
                            len(grid), len(reference)
                        ).mean(axis=1),
                    }
                )
            )

    return pd.concat(frames, ignore_index=True)


def _kernel_shap_values(models, features):
    # background は比較の基準集団、explained は寄与を説明する対象個体。
    background = features.sample(n=min(30, len(features)), random_state=42)
    explained = features.sample(n=min(40, len(features)), random_state=123)
    results = {}
    for model_name, model in models.items():
        explainer = shap.KernelExplainer(
            lambda values: _predict_benefit(model, values),
            background,
            feature_names=FEATURE_NAMES,
        )
        values = explainer.shap_values(
            explained,
            nsamples=2 ** len(FEATURE_NAMES),
            silent=True,
        )
        results[model_name] = np.asarray(values).reshape(
            len(explained), len(FEATURE_NAMES)
        )
    return explained, results


def _tidy_shap_values(explained, shap_values):
    frames = []
    for model_name, values in shap_values.items():
        for feature_index, feature in enumerate(FEATURE_NAMES):
            feature_values = explained.iloc[:, feature_index].to_numpy()
            spread = np.ptp(feature_values)
            if spread == 0:
                scaled_values = np.full(len(feature_values), 0.5)
            else:
                scaled_values = (
                    feature_values - feature_values.min()
                ) / spread
            frames.append(
                pd.DataFrame(
                    {
                        "learner": model_name,
                        "id": np.arange(1, len(explained) + 1),
                        "feature": feature,
                        "feature_value": feature_values,
                        "feature_value_scaled": scaled_values,
                        "shap_value": values[:, feature_index],
                    }
                )
            )
    return pd.concat(frames, ignore_index=True)


def write_meta_explanation_data(
    s_model_path,
    t_model_path,
    x_model_path,
    r_model_path,
    dr_model_path,
    toy_data,
    pdp_table_path,
    shap_table_path,
):
    """Compute reusable PDP and SHAP values and persist tidy CSV tables."""
    models = _load_models(
        [
            s_model_path,
            t_model_path,
            x_model_path,
            r_model_path,
            dr_model_path,
        ]
    )
    features = _features(toy_data)

    pdp = _pdp_curves(models, features)
    explained, shap_values = _kernel_shap_values(models, features)
    shap_table = _tidy_shap_values(explained, shap_values)

    pdp_output = _output_path(pdp_table_path)
    shap_output = _output_path(shap_table_path)
    pdp.to_csv(pdp_output, index=False)
    shap_table.to_csv(shap_output, index=False)

    return [str(pdp_output), str(shap_output)]
