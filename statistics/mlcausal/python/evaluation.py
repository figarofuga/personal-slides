"""Python-only validation artifacts for the cached EconML estimators."""

from pathlib import Path
import warnings

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from econml.validate import DRTester
from sklearn.ensemble import HistGradientBoostingClassifier


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
MODEL_COLORS = {
    "S-learner": "#59A14F",
    "T-learner": "#EDC948",
    "X-learner": "#F28E2B",
    "R-learner": "#B07AA1",
    "DR-learner": "#E15759",
}


class _FlatEffectAdapter:
    """Give DRTester the one-dimensional binary-treatment effect it expects."""

    def __init__(self, estimator):
        self.estimator = estimator

    def effect(self, X, T0=0, T1=1):
        return np.asarray(
            self.estimator.effect(X, T0=T0, T1=T1)
        ).reshape(-1)


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
    return HistGradientBoostingClassifier(
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


def _evaluate_models(models, toy_data, split, n_groups, n_bootstrap):
    """Use one common DR outcome construction for every CATE estimator."""
    (_, X_train, Y_train, T_train), (_, X_test, Y_test, T_test) = _partitions(
        toy_data, split
    )
    X_train_array = X_train.to_numpy(dtype=float)
    X_test_array = X_test.to_numpy(dtype=float)

    first_model = _FlatEffectAdapter(next(iter(models.values())))
    tester = DRTester(
        model_regression=_nuisance_regression_model(),
        model_propensity=_nuisance_propensity_model(),
        cate=first_model,
        cv=5,
    )
    tester.fit_nuisance(
        Xval=X_test_array,
        Dval=T_test,
        yval=Y_test,
        Xtrain=X_train_array,
        Dtrain=T_train,
        ytrain=Y_train,
    )

    summaries = []
    gate_frames = []
    curve_frames = []

    for model_name, model in models.items():
        # Nuisance predictions and DR outcomes stay fixed; only CATE predictions
        # change. This makes the five-model comparison directly comparable.
        tester.cate = _FlatEffectAdapter(model)
        tester.get_cate_preds(Xval=X_test_array, Xtrain=X_train_array)

        blp = tester.evaluate_blp()
        calibration = tester.evaluate_cal(n_groups=n_groups)

        np.random.seed(123)
        autoc = tester.evaluate_uplift(
            metric="toc", n_bootstrap=n_bootstrap
        )
        np.random.seed(123)
        qini = tester.evaluate_uplift(
            metric="qini", n_bootstrap=n_bootstrap
        )

        cate_test = np.asarray(tester.cate_preds_val_).reshape(-1)
        summaries.append(
            {
                "learner": model_name,
                "ate_rd": cate_test.mean(),
                "cate_sd": cate_test.std(ddof=1),
                "calibration_r2": float(calibration.cal_r_squared[0]),
                "blp_slope": float(blp.params[0]),
                "blp_se": float(blp.errs[0]),
                "blp_p_value": float(blp.pvals[0]),
                "autoc": float(autoc.params[0]),
                "autoc_se": float(autoc.errs[0]),
                "autoc_p_value": float(autoc.pvals[0]),
                "qini": float(qini.params[0]),
                "qini_se": float(qini.errs[0]),
                "qini_p_value": float(qini.pvals[0]),
            }
        )

        gate = calibration.plot_data_dict[1].copy()
        gate.insert(0, "learner", model_name)
        gate["group"] = gate["ind"].astype(int) + 1
        gate_frames.append(gate.drop(columns="ind"))

        for metric, result in (("AUTOC", autoc), ("QINI", qini)):
            curve = result.curves[1].copy()
            curve.insert(0, "learner", model_name)
            curve.insert(1, "metric", metric)
            curve_frames.append(curve)

    return (
        pd.DataFrame(summaries),
        pd.concat(gate_frames, ignore_index=True),
        pd.concat(curve_frames, ignore_index=True),
    )


def _plot_gate_comparison(gates, output):
    figure, axes = plt.subplots(1, len(MODEL_NAMES), figsize=(15, 3.4))

    limits = [
        gates["gate"].min() - 1.96 * gates["se_gate"].max(),
        gates["gate"].max() + 1.96 * gates["se_gate"].max(),
        gates["g_cate"].min(),
        gates["g_cate"].max(),
    ]
    low, high = min(limits), max(limits)
    padding = max((high - low) * 0.06, 0.002)
    low, high = low - padding, high + padding

    for axis, model_name in zip(axes, MODEL_NAMES):
        model_gates = gates.loc[gates["learner"] == model_name]
        axis.axline((0, 0), slope=1, color="0.55", linestyle="--", linewidth=1)
        axis.errorbar(
            model_gates["g_cate"],
            model_gates["gate"],
            yerr=1.96 * model_gates["se_gate"],
            fmt="o-",
            color=MODEL_COLORS[model_name],
            capsize=3,
            linewidth=1.2,
        )
        for _, row in model_gates.iterrows():
            axis.annotate(
                f"G{int(row['group'])}",
                (row["g_cate"], row["gate"]),
                xytext=(3, 3),
                textcoords="offset points",
                fontsize=7,
            )
        axis.set(xlim=(low, high), ylim=(low, high), title=model_name)
        axis.grid(alpha=0.2)

    axes[0].set_ylabel("Observed DR GATE (event risk difference)")
    for axis in axes:
        axis.set_xlabel("Mean predicted CATE")

    figure.suptitle(
        "EconML DRTester: GATE calibration on the held-out test set",
        fontsize=12,
    )
    figure.tight_layout()
    figure.savefig(output, format=output.suffix.lstrip("."), bbox_inches="tight")
    plt.close(figure)


def _plot_validation_summary(summary, output):
    figure, axes = plt.subplots(1, 2, figsize=(12, 4.2))
    positions = np.arange(len(MODEL_NAMES))
    ordered = summary.set_index("learner").loc[MODEL_NAMES].reset_index()

    axes[0].barh(
        positions,
        ordered["calibration_r2"],
        color=[MODEL_COLORS[name] for name in MODEL_NAMES],
    )
    axes[0].axvline(0, color="0.35", linewidth=0.8)
    axes[0].axvline(1, color="0.55", linestyle="--", linewidth=1)
    axes[0].set(
        yticks=positions,
        yticklabels=MODEL_NAMES,
        xlabel="Calibration R² (closer to 1 is better)",
        title="Calibration",
    )
    axes[0].invert_yaxis()
    axes[0].grid(axis="x", alpha=0.2)

    offsets = {"AUTOC": -0.12, "QINI": 0.12}
    markers = {"AUTOC": "o", "QINI": "s"}
    for metric, column, se_column in (
        ("AUTOC", "autoc", "autoc_se"),
        ("QINI", "qini", "qini_se"),
    ):
        y = positions + offsets[metric]
        axes[1].errorbar(
            ordered[column],
            y,
            xerr=1.96 * ordered[se_column],
            fmt=markers[metric],
            capsize=3,
            label=metric,
        )
    axes[1].axvline(0, color="0.55", linestyle="--", linewidth=1)
    axes[1].set(
        yticks=positions,
        yticklabels=MODEL_NAMES,
        xlabel="Discrimination coefficient (95% CI)",
        title="Discrimination",
    )
    axes[1].invert_yaxis()
    axes[1].legend(frameon=False)
    axes[1].grid(axis="x", alpha=0.2)

    figure.suptitle("Meta-learner validation with a common EconML DRTester")
    figure.tight_layout()
    figure.savefig(output, format=output.suffix.lstrip("."), bbox_inches="tight")
    plt.close(figure)


def evaluate_meta_learners(
    s_model_path,
    t_model_path,
    x_model_path,
    r_model_path,
    dr_model_path,
    toy_data,
    split,
    summary_path,
    gates_path,
    curves_path,
    gate_figure_path,
    summary_figure_path,
    n_groups=5,
    n_bootstrap=1000,
):
    """Persist all EconML comparison results as CSV/SVG artifacts."""
    models = _load_models(
        [
            s_model_path,
            t_model_path,
            x_model_path,
            r_model_path,
            dr_model_path,
        ]
    )
    summary, gates, curves = _evaluate_models(
        models,
        toy_data,
        split,
        n_groups=int(n_groups),
        n_bootstrap=int(n_bootstrap),
    )

    summary_output = _output_path(summary_path)
    gates_output = _output_path(gates_path)
    curves_output = _output_path(curves_path)
    gate_figure_output = _output_path(gate_figure_path)
    summary_figure_output = _output_path(summary_figure_path)

    summary.to_csv(summary_output, index=False)
    gates.to_csv(gates_output, index=False)
    curves.to_csv(curves_output, index=False)
    _plot_gate_comparison(gates, gate_figure_output)
    _plot_validation_summary(summary, summary_figure_output)

    return [
        str(summary_output),
        str(gates_output),
        str(curves_output),
        str(gate_figure_output),
        str(summary_figure_output),
    ]
