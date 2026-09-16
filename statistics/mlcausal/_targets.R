# R は推定結果を targets に保存し、Python はモデル・表・図のファイルパスを返す。
# 各ターゲットの引数が依存関係となり、入力が更新された処理から再計算される。
library(targets)
library(reticulate)
library(here)

mlcausal_dir <- here::here("statistics", "mlcausal")
mlcausal_path <- function(...) file.path(mlcausal_dir, ...)

# Respect the pixi activation variables. This fallback only covers direct R
# invocations from a nested working directory where the root .Rprofile was not
# discovered.
pixi_python <- here::here(".pixi", "envs", "default", "bin", "python")
if (!nzchar(Sys.getenv("RETICULATE_PYTHON")) && file.exists(pixi_python)) {
  reticulate::use_python(pixi_python, required = TRUE)
}

source(mlcausal_path("R", "make_toy_data.R"))
source(mlcausal_path("R", "pipeline.R"))

# Load each Python module once while the target script is initialized. File
# targets below provide explicit source dependency tracking for Python code.
reticulate::source_python(
  mlcausal_path("python", "models.py"),
  envir = globalenv(),
  convert = TRUE
)
reticulate::source_python(
  mlcausal_path("python", "interpretation.py"),
  envir = globalenv(),
  convert = TRUE
)
reticulate::source_python(
  mlcausal_path("python", "evaluation.py"),
  envir = globalenv(),
  convert = TRUE
)
reticulate::source_python(
  mlcausal_path("python", "explanations.py"),
  envir = globalenv(),
  convert = TRUE
)

tar_option_set(
  packages = c(
    "AIPW",
    "dplyr",
    "grf",
    "kernelshap",
    "marginaleffects",
    "MatchIt",
    "partykit",
    "policytree",
    "purrr",
    "reticulate",
    "sandwich",
    "shapviz",
    "simsurv",
    "SuperLearner",
    "tibble",
    "tmle",
    "WeightIt"
  ),
  seed = 123L,
  memory = "transient",
  garbage_collection = TRUE
)

# Python ファイルを明示的に参照することで、ソース変更も再計算の契機にする。
list(
  tar_target(
    python_models_file,
    mlcausal_path("python", "models.py"),
    format = "file"
  ),
  tar_target(
    python_interpretation_file,
    mlcausal_path("python", "interpretation.py"),
    format = "file"
  ),
  tar_target(
    python_evaluation_file,
    mlcausal_path("python", "evaluation.py"),
    format = "file"
  ),
  tar_target(
    python_explanations_file,
    mlcausal_path("python", "explanations.py"),
    format = "file"
  ),

  # Toy data ---------------------------------------------------------------
  tar_target(
    toy_objects,
    make_toy_data(
      n = 3000L,
      seed = 123L,
      base_age = 60,
      base_log_bmi = log(22) - 0.20^2 / 2,
      female_frac = 0.40
    )
  ),
  tar_target(toy_data, toy_objects$toy_data),
  tar_target(full_toy_data, toy_objects$full_toy_data),
  tar_target(
    test_toy_objects,
    make_toy_data(
      n = 1000L,
      seed = 135L,
      base_age = 61.44,
      base_log_bmi = log(19.88) - 0.20^2 / 2,
      female_frac = 0.45
    )
  ),
  tar_target(test_toy_data, test_toy_objects$toy_data),
  tar_target(full_test_toy_data, test_toy_objects$full_toy_data),

  # Shared preprocessing for all R and Python learners --------------------
  tar_target(policy_features, make_policy_features(toy_data)),
  tar_target(test_policy_features, make_policy_features(test_toy_data)),
  tar_target(
    ml_split,
    {
      python_models_file
      make_ml_split(toy_data, test_size = 0.2, random_state = 123L)
    }
  ),
  tar_target(
    full_training_split,
    {
      python_models_file
      make_full_training_split(toy_data)
    }
  ),

  # Python model fits are persisted with joblib and tracked as files. ------
  tar_target(
    s_learner_model_file,
    {
      python_models_file
      fit_s_learner(
        toy_data,
        ml_split,
        mlcausal_path("cache", "models", "s_learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    t_learner_model_file,
    {
      python_models_file
      fit_t_learner(
        toy_data,
        ml_split,
        mlcausal_path("cache", "models", "t_learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    x_learner_model_file,
    {
      python_models_file
      fit_x_learner(
        toy_data,
        ml_split,
        mlcausal_path("cache", "models", "x_learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    r_learner_model_file,
    {
      python_models_file
      fit_r_learner(
        toy_data,
        ml_split,
        mlcausal_path("cache", "models", "r_learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    dr_learner_model_file,
    {
      python_models_file
      fit_dr_learner(
        toy_data,
        ml_split,
        mlcausal_path("cache", "models", "dr_learner.joblib")
      )
    },
    format = "file"
  ),

  # Refit on the complete development cohort before external validation. --
  tar_target(
    external_s_learner_model_file,
    {
      python_models_file
      fit_s_learner(
        toy_data,
        full_training_split,
        mlcausal_path("cache", "models", "external-s-learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    external_t_learner_model_file,
    {
      python_models_file
      fit_t_learner(
        toy_data,
        full_training_split,
        mlcausal_path("cache", "models", "external-t-learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    external_x_learner_model_file,
    {
      python_models_file
      fit_x_learner(
        toy_data,
        full_training_split,
        mlcausal_path("cache", "models", "external-x-learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    external_r_learner_model_file,
    {
      python_models_file
      fit_r_learner(
        toy_data,
        full_training_split,
        mlcausal_path("cache", "models", "external-r-learner.joblib")
      )
    },
    format = "file"
  ),
  tar_target(
    external_dr_learner_model_file,
    {
      python_models_file
      fit_dr_learner(
        toy_data,
        full_training_split,
        mlcausal_path("cache", "models", "external-dr-learner.joblib")
      )
    },
    format = "file"
  ),

  # Python predictions and validation remain Python-owned file artifacts. --
  tar_target(
    meta_learner_effects_file,
    {
      python_evaluation_file
      write_meta_learner_predictions(
        s_learner_model_file,
        t_learner_model_file,
        x_learner_model_file,
        r_learner_model_file,
        dr_learner_model_file,
        toy_data,
        ml_split,
        mlcausal_path("cache", "tables", "meta-learner-effects.csv")
      )
    },
    format = "file"
  ),
  tar_target(
    meta_learner_evaluation_files,
    {
      python_evaluation_file
      unlist(
        evaluate_meta_learners(
          s_learner_model_file,
          t_learner_model_file,
          x_learner_model_file,
          r_learner_model_file,
          dr_learner_model_file,
          toy_data,
          ml_split,
          mlcausal_path("cache", "tables", "meta-validation.csv"),
          mlcausal_path("cache", "tables", "meta-gates.csv"),
          mlcausal_path("cache", "tables", "meta-uplift-curves.csv"),
          mlcausal_path("cache", "figures", "meta-gates.svg"),
          mlcausal_path("cache", "figures", "meta-validation.svg"),
          n_groups = 5L,
          n_bootstrap = 1000L
        ),
        use.names = FALSE
      )
    },
    format = "file"
  ),
  tar_target(
    external_meta_learner_effects_file,
    {
      python_evaluation_file
      write_external_meta_learner_predictions(
        external_s_learner_model_file,
        external_t_learner_model_file,
        external_x_learner_model_file,
        external_r_learner_model_file,
        external_dr_learner_model_file,
        test_toy_data,
        mlcausal_path(
          "cache", "tables", "external-meta-learner-effects.csv"
        )
      )
    },
    format = "file"
  ),
  tar_target(
    external_meta_learner_evaluation_files,
    {
      python_evaluation_file
      unlist(
        evaluate_external_meta_learners(
          external_s_learner_model_file,
          external_t_learner_model_file,
          external_x_learner_model_file,
          external_r_learner_model_file,
          external_dr_learner_model_file,
          toy_data,
          test_toy_data,
          mlcausal_path(
            "cache", "tables", "external-meta-validation.csv"
          ),
          mlcausal_path(
            "cache", "tables", "external-meta-gates.csv"
          ),
          mlcausal_path(
            "cache", "tables", "external-meta-uplift-curves.csv"
          ),
          mlcausal_path(
            "cache", "figures", "external-meta-gates.svg"
          ),
          mlcausal_path(
            "cache", "figures", "external-meta-validation.svg"
          ),
          n_groups = 5L,
          n_bootstrap = 1000L
        ),
        use.names = FALSE
      )
    },
    format = "file"
  ),

  # Native R causal forest, interpretation, policy, and evaluation. -------
  tar_target(
    causal_forest_bin,
    fit_causal_forest_bin(policy_features, toy_data)
  ),
  tar_target(
    causal_forest_shap,
    make_causal_forest_shap(causal_forest_bin, policy_features)
  ),
  tar_target(
    causal_forest_policy,
    fit_causal_forest_policy(policy_features, toy_data)
  ),
  tar_target(
    external_causal_forest_predictions,
    make_external_causal_forest_predictions(
      causal_forest_bin,
      test_policy_features,
      test_toy_data
    )
  ),
  tar_target(
    external_evaluation_forest,
    fit_external_evaluation_forest(
      test_policy_features,
      test_toy_data
    )
  ),
  tar_target(
    external_rate_results,
    make_external_rate_results(
      external_evaluation_forest,
      external_causal_forest_predictions
    )
  ),
  tar_target(
    external_grf_validation,
    make_external_grf_validation(
      external_causal_forest_predictions,
      external_evaluation_forest,
      external_rate_results,
      n_groups = 5L
    )
  ),
  tar_target(
    external_c_for_benefit,
    make_external_c_for_benefit(
      make_external_benefit_pairs(test_toy_data),
      external_meta_learner_effects_file,
      external_causal_forest_predictions,
      n_bootstrap = 500L
    )
  ),

  # GRF explanations are computed only with native R functions. -----------
  tar_target(explanation_samples, make_explanation_samples(policy_features)),
  tar_target(
    grf_pdp_data,
    make_grf_pdp_data(causal_forest_policy, explanation_samples)
  ),
  tar_target(
    grf_cate_shap,
    make_grf_cate_shap(causal_forest_policy, explanation_samples)
  ),

  # Expensive meta-learner PDP and SHAP values are cached as tidy tables.
  tar_target(
    meta_explanation_data_files,
    {
      python_explanations_file
      unlist(
        write_meta_explanation_data(
          s_learner_model_file,
          t_learner_model_file,
          x_learner_model_file,
          r_learner_model_file,
          dr_learner_model_file,
          toy_data,
          mlcausal_path("cache", "tables", "meta-pdp.csv"),
          mlcausal_path("cache", "tables", "meta-shap.csv")
        ),
        use.names = FALSE
      )
    },
    format = "file"
  ),

  # SingleTree interpreters are rendered in Python; only SVG files persist. -
  tar_target(
    s_learner_tree_figure,
    {
      python_interpretation_file
      plot_single_tree_interpreter(
        s_learner_model_file,
        toy_data,
        ml_split,
        "S-learner",
        mlcausal_path("cache", "figures", "single-tree-s.svg")
      )
    },
    format = "file"
  ),
  tar_target(
    t_learner_tree_figure,
    {
      python_interpretation_file
      plot_single_tree_interpreter(
        t_learner_model_file,
        toy_data,
        ml_split,
        "T-learner",
        mlcausal_path("cache", "figures", "single-tree-t.svg")
      )
    },
    format = "file"
  ),
  tar_target(
    x_learner_tree_figure,
    {
      python_interpretation_file
      plot_single_tree_interpreter(
        x_learner_model_file,
        toy_data,
        ml_split,
        "X-learner",
        mlcausal_path("cache", "figures", "single-tree-x.svg")
      )
    },
    format = "file"
  ),
  tar_target(
    r_learner_tree_figure,
    {
      python_interpretation_file
      plot_single_tree_interpreter(
        r_learner_model_file,
        toy_data,
        ml_split,
        "R-learner",
        mlcausal_path("cache", "figures", "single-tree-r.svg")
      )
    },
    format = "file"
  ),
  tar_target(
    dr_learner_tree_figure,
    {
      python_interpretation_file
      plot_single_tree_interpreter(
        dr_learner_model_file,
        toy_data,
        ml_split,
        "DR-learner",
        mlcausal_path("cache", "figures", "single-tree-dr.svg")
      )
    },
    format = "file"
  )
)
