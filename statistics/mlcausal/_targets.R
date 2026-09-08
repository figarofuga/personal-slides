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
    "reticulate",
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
    make_toy_data(n = 3000L, seed = 123L)
  ),
  tar_target(toy_data, toy_objects$toy_data),
  tar_target(full_toy_data, toy_objects$full_toy_data),
  tar_target(ate_rd_summary, make_ate_rd_summary(full_toy_data)),

  # Small teaching examples: precompute the numerical output shown in slides.
  tar_target(lalonde_data, MatchIt::lalonde),
  tar_target(
    subclassification_fit,
    make_subclassification_fit(lalonde_data)
  ),
  tar_target(
    subclassification_result,
    make_subclassification_result(subclassification_fit)
  ),
  tar_target(
    nearest_matching_fit,
    make_nearest_matching_fit(lalonde_data)
  ),
  tar_target(
    matching_result,
    make_matching_result(nearest_matching_fit)
  ),
  tar_target(weighting_result, make_weighting_result(lalonde_data)),
  tar_target(aipw_result, make_aipw_result(lalonde_data)),
  tar_target(tmle_result, make_tmle_result(lalonde_data)),
  tar_target(
    orthogonalization_result,
    make_orthogonalization_result(lalonde_data)
  ),

  # Shared preprocessing for all R and Python learners --------------------
  tar_target(policy_features, make_policy_features(toy_data)),
  tar_target(
    ml_split,
    {
      python_models_file
      make_ml_split(toy_data, test_size = 0.2, random_state = 123L)
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

  # Native R causal forest, interpretation, policy, and evaluation. -------
  tar_target(
    causal_forest_bin,
    fit_causal_forest_bin(policy_features, toy_data)
  ),
  tar_target(
    causal_forest_predictions,
    make_causal_forest_predictions(causal_forest_bin, toy_data)
  ),
  tar_target(
    causal_forest_dr_scores,
    make_causal_forest_dr_scores(causal_forest_bin)
  ),
  tar_target(
    causal_forest_shap,
    make_causal_forest_shap(causal_forest_bin, policy_features)
  ),
  tar_target(
    causal_forest_surrogate_tree,
    make_causal_forest_surrogate_tree(causal_forest_bin, toy_data)
  ),
  tar_target(
    causal_forest_policy,
    fit_causal_forest_policy(policy_features, toy_data)
  ),
  tar_target(policy_scores, make_policy_scores(causal_forest_policy)),
  tar_target(policy_tree, fit_policy_tree(policy_features, policy_scores)),
  tar_target(rate_results, make_rate_results(causal_forest_bin)),
  tar_target(
    grf_validation,
    make_grf_validation(causal_forest_bin, rate_results, n_groups = 5L)
  ),
  tar_target(grf_gate_data, grf_validation$gates),
  tar_target(grf_validation_summary, grf_validation$summary),

  # GRF explanations are computed only with native R functions. -----------
  tar_target(explanation_samples, make_explanation_samples(policy_features)),
  tar_target(
    grf_effect_curves,
    make_grf_effect_curves(causal_forest_policy, explanation_samples)
  ),
  tar_target(grf_pdp_data, grf_effect_curves$pdp),
  tar_target(grf_ice_data, grf_effect_curves$ice),
  tar_target(
    grf_cate_shap,
    make_grf_cate_shap(causal_forest_policy, explanation_samples)
  ),

  # Meta-learner explanations are computed and rendered entirely in Python.
  tar_target(
    meta_explanation_figures,
    {
      python_explanations_file
      unlist(
        make_meta_explanation_figures(
          s_learner_model_file,
          t_learner_model_file,
          x_learner_model_file,
          r_learner_model_file,
          dr_learner_model_file,
          toy_data,
          mlcausal_path("cache", "figures", "meta-pdp.svg"),
          mlcausal_path("cache", "figures", "meta-ice.svg"),
          mlcausal_path("cache", "figures", "meta-shap.svg")
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
