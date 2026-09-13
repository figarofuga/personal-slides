"""Cached EconML SingleTreeCateInterpreter figures for the slide deck."""

from pathlib import Path

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from econml.cate_interpreter import SingleTreeCateInterpreter


FEATURE_NAMES = ["age", "sexm1", "bmi", "hf", "bnp", "lvef"]


class BenefitCateWrapper:
    """Expose CATE as risk reduction so larger values mean more benefit."""

    def __init__(self, estimator):
        self.estimator = estimator

    def const_marginal_effect(self, X):
        # イベントリスク差をリスク低下量に変換し、木の葉で正の値が利益を表す。
        return -np.asarray(self.estimator.const_marginal_effect(X))


def _test_features(toy_data, split):
    data = pd.DataFrame(toy_data).copy()
    data["id"] = data["id"].astype(int)
    data = data.set_index("id", drop=False)

    if isinstance(split, dict):
        test_ids = split["test_ids"]
    else:
        test_ids = getattr(split, "test_ids")

    test_ids = np.asarray(test_ids, dtype=int).reshape(-1)
    # 効果モデルの学習に使わなかった個体で、予測された効果の構造を説明する。
    return data.loc[test_ids, FEATURE_NAMES].astype(float).copy()


def plot_single_tree_interpreter(
    model_path,
    toy_data,
    split,
    model_name,
    figure_path,
):
    """Build one surrogate interpreter and persist only its rendered figure."""
    estimator = joblib.load(Path(model_path))
    tree_X = _test_features(toy_data, split)
    # 葉に少数例だけが残る細かすぎるルールを避け、深さ 3 の読みやすい木にする。
    minimum_leaf_size = max(10, int(np.ceil(0.05 * len(tree_X))))

    benefit_model = BenefitCateWrapper(estimator)
    interpreter = SingleTreeCateInterpreter(
        include_model_uncertainty=False,
        max_depth=3,
        min_samples_leaf=minimum_leaf_size,
        random_state=123,
    )
    interpreter.interpret(benefit_model, tree_X)

    benefit_cate = benefit_model.const_marginal_effect(tree_X)
    benefit_cate = benefit_cate.reshape((len(tree_X), -1))
    # この R² は同じ説明対象上での元モデルへの近似度であり、真の因果効果への精度ではない。
    fidelity = interpreter.tree_model_.score(tree_X, benefit_cate)

    output = Path(figure_path).expanduser().resolve()
    output.parent.mkdir(parents=True, exist_ok=True)

    figure, axis = plt.subplots(figsize=(12, 5.8))
    interpreter.plot(
        ax=axis,
        title=(
            f"{model_name}: depth-3 CATE surrogate "
            f"(R² = {fidelity:.2f})"
        ),
        feature_names=FEATURE_NAMES,
        treatment_names=["CA risk reduction"],
        max_depth=3,
        filled=True,
        rounded=True,
        precision=3,
        fontsize=7,
    )
    figure.tight_layout()
    figure.savefig(output, format=output.suffix.lstrip("."), bbox_inches="tight")
    plt.close(figure)

    # 木そのものは保存せず、targets が追跡できる図のパスを返す。
    return str(output)
