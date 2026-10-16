# 偏った分布の可視化練習用データ

`synthetic_ck.csv` は、提示された男女別のCKの図に似せた完全な架空データです。
元画像のデータを正確に復元したものでも、実患者データや臨床分布を推定したものでもありません。

- 男性30例：低値から約6,300まで広く分布。
- 女性11例：低値が多く、約8,900の高値を1例含む。
- 人数差、右に裾の長い分布、外れた高値による平均と中央値の違いを練習できます。

列は `synthetic_id`（架空ID）、`sex`（Male/Female）、
`ck_post_pci_u_l`（PCI後CKを想定した値、単位U/L）です。
測定時点・ピーク値などの臨床条件は設定していません。

`synthetic_ck_summary.csv` は答え合わせ用の群別要約、
`synthetic_ck_preview.png` は分布の確認図です。横線は中央値を示します。
図の点は250 U/L幅でビン分けされます。正確な個々の値はCSVを参照してください。

再生成は、プロジェクトルートで次を実行します（乱数seed固定）。

```bash
pixi run --as-is Rscript statistics/visualization_mi/scripts/generate_skewed_ck.R
```

Rでの読み込み例：

```r
ck <- read.csv("statistics/visualization_mi/data/synthetic_ck.csv")
```

練習例：同じデータを平均の棒グラフ、個々の点、箱ひげ図で表示して比較する。
さらに女性の最大値を除いた場合に平均と中央値がどう変わるか確認する。
