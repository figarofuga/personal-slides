# %%
from pathlib import Path
import joblib
import pandas as pd

mlcausal_dir = Path(__file__).resolve().parent
model_dir = mlcausal_dir / "cache" / "models"

dr_learner = joblib.load(
    model_dir / "external-dr-learner.joblib"
)

train_toy_data = pd.read_csv(mlcausal_dir / "toy_data.csv")
test_toy_data = pd.read_csv(mlcausal_dir / "test_toy_data.csv")

FEATURE_NAMES = ["age", "sexm1", "bmi", "hf", "bnp", "lvef"]

Xtrain = train_toy_data[FEATURE_NAMES].to_numpy()
Dtrain = train_toy_data["ca"]
Ytrain = train_toy_data["bin_outcome"]

Xval = test_toy_data[FEATURE_NAMES].to_numpy()
Dval = test_toy_data["ca"]
Yval = test_toy_data["bin_outcome"]

print(type(dr_learner))

class BenefitEffectAdapter:
    def __init__(self, estimator):
        self.estimator = estimator

    def effect(self, X, T0=0, T1=1):
        return -np.asarray(
            self.estimator.effect(X, T0=T0, T1=T1)
        ).reshape(-1)
    
# %%
import numpy as np
import pandas as pd
import scipy.stats as st
from sklearn.ensemble import RandomForestClassifier, GradientBoostingRegressor

from econml.dr import DRLearner

from econml.validate.drtester import DRTester

model_regression = GradientBoostingRegressor(random_state=0)
model_propensity = RandomForestClassifier(random_state=0)

# Initialize DRTester and fit/predict nuisance models
dr_tester = DRTester(
    model_regression=model_regression,
    model_propensity=model_propensity,
    cate=BenefitEffectAdapter(dr_learner),
).fit_nuisance(
    Xval=Xval,
    Dval=Dval.to_numpy(),
    yval=1 - Yval.to_numpy(),
    Xtrain=Xtrain,
    Dtrain=Dtrain.to_numpy(),
    ytrain=1 - Ytrain.to_numpy(),
)
res_dr = dr_tester.evaluate_all(
    Xval, Xtrain,
    n_groups=5,
    n_bootstrap=1000)
res_dr.summary()

gate_df = res_dr.cal.plot_data_dict[1].copy()

print(gate_df)

res_dr.plot_cal(tmt=1)
res_dr.plot_qini(1)

