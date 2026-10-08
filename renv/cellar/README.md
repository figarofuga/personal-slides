# rmsb のローカル修正版

`rmsb_1.1-2.9000.tar.gz`はCRANのrmsb 1.1-2に、旧`vendor/rmsb`の修正を引き継いだソースです。
バイナリは含みません。通常のCRAN版とキャッシュを区別するため、ローカル版を1.1-2.9000としています。

- 上流：<https://cran.r-project.org/src/contrib/rmsb_1.1-2.tar.gz>
- 上流SHA-256：`a6db5b99b5a54761e8787a2c087e0734573148210fe07119dd4055e6dca2bf00`
- 修正：`scripts/patches/rmsb-1.1-2.patch`
- このアーカイブのSHA-256：`a758de667ca74519b27ed2d6261cb7c370719c47a8375eff5c55c92d70241668`

修正は生成済みStan C++コードのBoost RNG名と、`lrmconppot`の`C2`列数（`p`から`q`）です。
`DESCRIPTION`のVersionをローカル版に変更し、Repositoryを除き、`RemoteType: cellar`を追加しています。
変更前のMD5一覧は除去しました。それ以外はCRANのソースです。

再生成する場合は上流tarballのハッシュを照合し、展開した`rmsb/`で
`patch -p1 < /path/to/scripts/patches/rmsb-1.1-2.patch`を適用します。
上記DESCRIPTIONの変更とMD5の除去を行い、`rmsb/`をtar.gzにまとめてください。
将来、修正を含む上流版へ更新できた場合は、このcellarとpatchを削除できます。
