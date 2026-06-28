# %%
import pandas as pd
from pathlib import Path

try:
    # python test.py として通常実行した場合
    here = Path(__file__).resolve().parent
except NameError:
    # VS Code Interactive / Jupyter cell の場合
    here = Path.cwd()

csv_path = here /"statistics"/"mlcausal"/"toy_data.csv"

toy_data = pd.read_csv(csv_path)



# Main imports
# Helper imports
import numpy as np
import pandas as pd
from econml.metalearners import XLearner
from numpy.random import binomial, multivariate_normal, normal, uniform
from sklearn.ensemble import GradientBoostingRegressor, RandomForestClassifier
from sklearn.model_selection import train_test_split


X = toy_data.loc[:, ["age", "sex", "bmi", "hf", "bnp", "lvef"]]
y = toy_data["afeqt_os"]
T = toy_data["ca"]
n = toy_data.shape[0]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=T
)

T_train = T.loc[y_train.index]
T_test = T.loc[y_test.index]

# モデルの構築
models = GradientBoostingRegressor(max_depth=3, random_state=0)
propensity_model = RandomForestClassifier()
X_learner = XLearner(models=models, propensity_model=propensity_model)
X_learner.fit(y_train, T_train, X=X_train)


# %%

from econml.cate_interpreter import SingleTreeCateInterpreter

intrp = SingleTreeCateInterpreter(
    include_model_uncertainty=False, max_depth=2, min_samples_leaf=10
)
# We interpret the CATE model's behavior based on the features used for heterogeneity
intrp.interpret(DR_learner, X)
# Plot the tree
intrp.plot(feature_names=["age", "sex", "bmi", "hf", "bnp", "lvef"], fontsize=12)

# %%
