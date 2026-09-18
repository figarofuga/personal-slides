# 学習・評価の実行範囲

## targetsで保存するもの

- EconMLのS / T / X / R / DR-learnerの学習。前半のinternal split用と、最後のtest datasetに適用する開発コホート全例のモデルは別々に保存する。
- Rのcausal forest、方策用forest、および外部評価用のnuisance推定forestの学習。
- EconML `DRTester.evaluate_all()` による評価。5モデルで共通のDR outcomeを使用し、各モデルの評価済み`DRTester`を個別のjoblib、GATE・TOC・Qini・summaryをモデル別CSVに保存する。
- PDPの数値、Python Kernel SHAP、R SHAP。PythonのPDPとSHAPは別の関数・targets。RのSHAPはR側の関数で独立に計算する。

`meta_pdp_data_file`と`meta_shap_data_file`は、開発コホート全例で学習したモデルとtestデータを使用する。PDPはAge・BMI・BNP・LVEFの4変数。Single Treeの教材例は前半のinternal splitのモデルを使用する。

## index.qmdで計算するもの

- GRFのtestデータへの予測、GATE、RATE（AUTOC / QINI）、TOC・Qini曲線、MatchItによるmatchingとスクラッチ実装のC-for-benefit。
- Meta-learnerのGATE・TOC・Qini・AUTOC比較図、PDP・SHAPの表示。
- 前半の軽い教材例、学習データでのGRF診断。

GRFの評価用forestの効果予測は順位付けに使用しない。開発データで学習した固定モデルで順位付けし、testデータの評価用forestから得るDR scoreで評価する。

## 符号と曲線

- モデル学習のoutcomeはイベント発生。`tau = E[Y(1) - Y(0)]`が負なら利益。
- TOC / Qini / GATEの表示は利益の尺度。EconMLでは評価outcomeを`1 - Y`、モデル予測を`-tau`に変換する。GRFの評価用forestも`1 - Y`で学習する。
- C-for-benefitは観測benefit（対照のイベント−治療のイベント）と予測benefit（ペアの平均`-tau`）の順位一致率をスクラッチで求める。観測同値は除外し、予測同値は0.5点。95% CIは固定したmatched pairを単位とする500回のpercentile bootstrap。
- `plot(grf_qini)`はQini曲線ではなくTOCを表示する。QmdではQiniを`q * TOC(q)`で描く。
- EconMLとGRFは既定の分位点・積分法が異なる。ここでは各パッケージの推定値を別々のスライドで示す。

## 実行と確認

プロジェクトルートから、PixiのR/Python環境を有効にして実行する。

```r
targets::tar_make(
  script = "statistics/mlcausal/_targets.R",
  store = "statistics/mlcausal/_targets"
)
```

```bash
quarto render statistics/mlcausal/index.qmd
```

保存済みのEconML評価をPythonから確認する場合（プロジェクトルート）：

```python
import joblib

tester = joblib.load(
    "statistics/mlcausal/cache/validation/external/s_learner-drtester.joblib"
)
result = tester.res  # econml.validate.EvaluationResults
print(result.summary())
result.plot_cal(tmt=1)
result.plot_toc(tmt=1)
result.plot_qini(tmt=1)
```

Rでは、各DRTesterからtargetsが書き出したraw tableを読み、モデル間で結合してから描画する。

```r
gate_files <- external_meta_learner_evaluation_files[
  endsWith(basename(external_meta_learner_evaluation_files), "-gate.csv")
]

meta_gates <- gate_files |>
  lapply(read.csv) |>
  dplyr::bind_rows()

ggplot2::ggplot(meta_gates, ggplot2::aes(g_cate, gate, color = learner)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = gate - 1.96 * se_gate, ymax = gate + 1.96 * se_gate),
    width = 0
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~learner)
```

Rのチャンクだけを対話的に試す場合は、先にQmdのsetupを実行し、GRFのtest予測から順に実行する。`tar_read()`でもモデル単体を読み込める。
