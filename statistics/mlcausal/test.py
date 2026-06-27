# %%
import pandas as pd

toy_data = pd.read_csv("statistics/mlcausal/toy_data.csv")


# Main imports
# Helper imports
import numpy as np
import pandas as pd
from econml.dr import DRLearner
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

# Instantiate Doubly Robust Learner

outcome_model = GradientBoostingRegressor(
    n_estimators=100, max_depth=6, min_samples_leaf=int(n / 100)
)
pseudo_treatment_model = GradientBoostingRegressor(
    n_estimators=100, max_depth=6, min_samples_leaf=int(n / 100)
)
propensity_model = RandomForestClassifier(
    n_estimators=100, max_depth=6, min_samples_leaf=int(n / 100)
)

DR_learner = DRLearner(
    model_regression=outcome_model,
    model_propensity=propensity_model,
    model_final=pseudo_treatment_model,
    cv=5,
)
# Train DR_learner
DR_learner.fit(y, T, X=X)
# Estimate treatment effects on test data
DR_te = DR_learner.effect(X_test)


# %%

from econml.cate_interpreter import SingleTreeCateInterpreter

intrp = SingleTreeCateInterpreter(
    include_model_uncertainty=False, max_depth=2, min_samples_leaf=10
)
# We interpret the CATE model's behavior based on the features used for heterogeneity
intrp.interpret(DR_learner, X)
# Plot the tree
intrp.plot(feature_names=["age", "sex", "bmi", "hf", "bnp", "lvef"], fontsize=12)
