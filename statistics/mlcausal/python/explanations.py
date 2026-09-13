"""Python-only PDP, ICE, and Kernel SHAP artifacts for EconML models."""

from pathlib import Path
import warnings

import joblib
import matplotlib.pyplot as plt
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
FEATURE_LABELS = {
    "age": "Age",
    "sexm1": "Sex (male)",
    "bmi": "BMI",
    "hf": "Heart failure",
    "bnp": "BNP",
    "lvef": "LVEF (%)",
}
MODEL_NAMES = [
    "S-learner",
    "T-learner",
    "X-learner",
    "R-learner",
    "DR-learner",
]
MODEL_COLORS = {
    "S-learner": "#59A14F",
    "T-learner": "#EDC948",
    "X-learner": "#F28E2B",
    "R-learner": "#B07AA1",
    "DR-learner": "#E15759",
}


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
    # イベントリスク差の符号を反転し、図では正の値ほど利益（リスク低下）が大きい。
    return -np.asarray(model.effect(frame, T0=0, T1=1)).reshape(-1)


def _effect_curves(models, features):
    # 全モデルで参照集団とグリッドを共有し、モデル差と抽出対象の差を切り分ける。
    reference = features.sample(n=min(500, len(features)), random_state=42)
    ice = reference.sample(n=min(20, len(reference)), random_state=42)
    # 極端な値への外挿を抑えるため、参照集団の 5〜95 パーセンタイルを動かす。
    grids = {
        feature: np.linspace(
            reference[feature].quantile(0.05),
            reference[feature].quantile(0.95),
            25,
        )
        for feature in EXPLAIN_FEATURES
    }

    pdp_frames = []
    ice_frames = []
    for model_name, model in models.items():
        for feature in EXPLAIN_FEATURES:
            grid = grids[feature]
            # グリッド値ごとに全個体を複製し、一つの特徴量だけを置き換えて一括予測する。
            pdp_newdata = pd.concat(
                [reference.copy() for _ in grid], ignore_index=True
            )
            pdp_newdata[feature] = np.repeat(grid, len(reference))
            pdp_prediction = _predict_benefit(model, pdp_newdata)
            pdp_frames.append(
                pd.DataFrame(
                    {
                        "learner": model_name,
                        "feature": feature,
                        "value": grid,
                        # 個体方向の平均が PDP。配列は「グリッド × 個体」の順で並んでいる。
                        "hte": pdp_prediction.reshape(
                            len(grid), len(reference)
                        ).mean(axis=1),
                    }
                )
            )

            # ICE は平均せず同じ個体の予測を追い、個体間の反応の違いを残す。
            ice_newdata = pd.concat(
                [ice.copy() for _ in grid], ignore_index=True
            )
            ice_newdata[feature] = np.repeat(grid, len(ice))
            ice_frames.append(
                pd.DataFrame(
                    {
                        "learner": model_name,
                        "feature": feature,
                        "id": np.tile(np.arange(len(ice)), len(grid)),
                        "value": np.repeat(grid, len(ice)),
                        "hte": _predict_benefit(model, ice_newdata),
                    }
                )
            )

    return pd.concat(pdp_frames), pd.concat(ice_frames)


def _plot_pdp(pdp, output):
    figure, axes = plt.subplots(1, len(EXPLAIN_FEATURES), figsize=(12, 3.7))
    for axis, feature in zip(axes, EXPLAIN_FEATURES):
        axis.axhline(0, color="0.55", linestyle="--", linewidth=1)
        for model_name in MODEL_NAMES:
            selected = pdp.loc[
                (pdp["feature"] == feature)
                & (pdp["learner"] == model_name)
            ]
            axis.plot(
                selected["value"],
                selected["hte"],
                color=MODEL_COLORS[model_name],
                label=model_name,
                linewidth=1.5,
            )
        axis.set_xlabel(FEATURE_LABELS[feature])
        axis.grid(alpha=0.2)
    axes[0].set_ylabel("Estimated HTE: risk reduction")
    handles, labels = axes[-1].get_legend_handles_labels()
    figure.legend(handles, labels, loc="lower center", ncol=5, frameon=False)
    figure.suptitle("EconML meta-learners: partial dependence of CATE")
    figure.tight_layout(rect=(0, 0.12, 1, 1))
    figure.savefig(output, format=output.suffix.lstrip("."), bbox_inches="tight")
    plt.close(figure)


def _plot_ice(pdp, ice, output):
    figure, axes = plt.subplots(1, len(EXPLAIN_FEATURES), figsize=(12, 3.9))
    for axis, feature in zip(axes, EXPLAIN_FEATURES):
        axis.axhline(0, color="0.55", linestyle="--", linewidth=1)
        for model_name in MODEL_NAMES:
            selected = ice.loc[
                (ice["feature"] == feature)
                & (ice["learner"] == model_name)
            ]
            for _, subject in selected.groupby("id"):
                axis.plot(
                    subject["value"],
                    subject["hte"],
                    color=MODEL_COLORS[model_name],
                    alpha=0.08,
                    linewidth=0.35,
                )
            model_pdp = pdp.loc[
                (pdp["feature"] == feature)
                & (pdp["learner"] == model_name)
            ]
            axis.plot(
                model_pdp["value"],
                model_pdp["hte"],
                color=MODEL_COLORS[model_name],
                label=model_name,
                linewidth=1.4,
            )
        axis.set_xlabel(FEATURE_LABELS[feature])
        axis.grid(alpha=0.2)
    axes[0].set_ylabel("Estimated HTE: risk reduction")
    handles, labels = axes[-1].get_legend_handles_labels()
    figure.legend(handles, labels, loc="lower center", ncol=5, frameon=False)
    figure.suptitle("EconML meta-learners: individual conditional expectation")
    figure.tight_layout(rect=(0, 0.12, 1, 1))
    figure.savefig(output, format=output.suffix.lstrip("."), bbox_inches="tight")
    plt.close(figure)


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
            # 6 特徴量の部分集合を網羅できる予算を確保する。
            nsamples=2 ** len(FEATURE_NAMES),
            silent=True,
        )
        results[model_name] = np.asarray(values).reshape(
            len(explained), len(FEATURE_NAMES)
        )
    return explained, results


def _plot_shap(explained, shap_values, output):
    figure, axes = plt.subplots(2, 3, figsize=(12, 7.4), sharex=False)
    rng = np.random.default_rng(123)

    for axis, model_name in zip(axes.flat, MODEL_NAMES):
        values = shap_values[model_name]
        # 平均絶対 SHAP 値で寄与の大きさを並べ、色で元の特徴量の大小を示す。
        order = np.argsort(np.mean(np.abs(values), axis=0))[-6:]
        for position, feature_index in enumerate(order):
            feature_values = explained.iloc[:, feature_index].to_numpy()
            spread = np.ptp(feature_values)
            if spread == 0:
                color_values = np.full(len(feature_values), 0.5)
            else:
                color_values = (feature_values - feature_values.min()) / spread
            jitter = rng.normal(0, 0.07, size=len(feature_values))
            axis.scatter(
                values[:, feature_index],
                position + jitter,
                c=color_values,
                cmap="coolwarm",
                vmin=0,
                vmax=1,
                s=9,
                alpha=0.75,
                linewidths=0,
            )
        axis.axvline(0, color="0.55", linewidth=0.8)
        axis.set_yticks(
            np.arange(len(order)),
            [FEATURE_LABELS[FEATURE_NAMES[index]] for index in order],
            fontsize=8,
        )
        axis.set_title(model_name)
        axis.set_xlabel("SHAP value for risk reduction")
        axis.grid(axis="x", alpha=0.2)

    axes.flat[-1].axis("off")
    figure.suptitle("EconML meta-learners: exact Kernel SHAP")
    figure.tight_layout()
    figure.savefig(output, format=output.suffix.lstrip("."), bbox_inches="tight")
    plt.close(figure)


def make_meta_explanation_figures(
    s_model_path,
    t_model_path,
    x_model_path,
    r_model_path,
    dr_model_path,
    toy_data,
    pdp_figure_path,
    ice_figure_path,
    shap_figure_path,
):
    """Compute and draw all meta-learner explanations within Python."""
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
    pdp, ice = _effect_curves(models, features)

    pdp_output = _output_path(pdp_figure_path)
    ice_output = _output_path(ice_figure_path)
    shap_output = _output_path(shap_figure_path)
    _plot_pdp(pdp, pdp_output)
    _plot_ice(pdp, ice, ice_output)

    explained, shap_values = _kernel_shap_values(models, features)
    _plot_shap(explained, shap_values, shap_output)

    return [str(pdp_output), str(ice_output), str(shap_output)]
