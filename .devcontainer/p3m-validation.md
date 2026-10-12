# P3Mバイナリ移行の検証

検証日：2026-10-12（JST）。Ubuntu 24.04.4 / WSL2、Linux x86_64、
Pixi 0.81.0、R 4.5.3、conda-forge GCC 14.3.0で検証しました。
454個のパッケージのバージョンは変更していません。

## 取得先

| 対象 | 取得先・形式 |
| --- | --- |
| 通常のCRANパッケージ | P3M `manylinux_2_28/latest`のportableバイナリを優先 |
| `rstan 2.32.7`・`rstanarm 2.32.2` | 2026-07-01のP3Mバイナリ。`renv.lock`のURLで固定 |
| `rmsb 1.1-2` | P3Mのソース版。現在の固定版にはバイナリがない |
| `renv 1.3.1` | 起動用のbootstrapでソース版を導入 |
| `cmdstanr`・`sess` | 従来のGitHub commitを維持 |

最新のP3Mの`rstan`バイナリは、固定中の`RcppParallel 5.1.11-2`と
TBBのシンボルが一致せず、読み込みに失敗しました。
2026-07-01の同じバージョンのバイナリで読み込みと実行が通ったため、
`rstan`・`rstanarm`の配布URLを固定しています。
これらの更新時には、`RcppParallel`・`StanHeaders`との組み合わせを再検証してください。

## 結果

| 確認 | 結果 |
| --- | --- |
| 空のライブラリからrenvをbootstrap | 成功 |
| `pixi install --locked` | 成功。Pixiのロック更新は不要 |
| 全454個の固定版との一致・renvの同期・名前空間の読み込み | 成功 |
| バイナリの内訳 | 450個。ソース由来は上記の4個 |
| Python連携 | NumPy・pandas・SciPy・scikit-learnの読み込み成功 |
| CmdStan | コンパイル・サンプリング成功 |
| RStan | P3MのヘッダーとPixiのコンパイラでコンパイル・サンプリング成功 |
| rstanarm・rmsb | サンプリング成功。rmsbはpartial PPOも確認 |
| Quarto | R・ggplot2・Pythonを実行してHTML出力成功 |
| rmsbの新規ソースビルド | P3Mからソースを取得し、ビルド済みキャッシュを使わず成功。230秒 |
| 再ビルド後のrmsb | renvの同期・partial PPOのサンプリング成功 |
| 通常のプロジェクト環境への反映 | 復元・Rカーネル登録・rmsbサンプリング・Quarto出力成功 |

検証は別ライブラリで行い、通常の環境に反映した後は検証用キャッシュを
一時的に利用できなくしてサンプリングも確認しました。
P3M用ライブラリとキャッシュは`.pixi/renv-library-p3m/`と
`.pixi/renv-cache-p3m/`にあります。以前のCRANソース版の環境は残しています。

`cmdstanr`・`sess`は、ロック中のcommitと一致する既存のソースビルドを再利用しました。
短いサンプリングは実行確認用で、推定の収束・精度の検証ではありません。
Codespaces上でのコンテナ作成自体は、このローカル検証では実行していません。
ビルド時間はローカルの測定値で、環境によって変わります。

実行ログとパッケージ一覧はGit管理対象外の`.pixi/p3m-validation/`に保存しています。
`audit.log`、`package-audit.csv`、`integration.log`、`rmsb-build.log`、
`rmsb-sample.log`、`main-restore.log`、`main-smoke.log`、`main-quarto.log`を参照してください。

## Codespacesでの実行

変更をcommit・pushしてDev Containerを再構築します。
Pixiの基盤導入後、ターミナルで次を実行してください。

```bash
bash .devcontainer/setup.sh --check
```

進捗と失敗時の出力は`.devcontainer/setup.log`に保存されます。

仕様：[Positのportable Linux binaries](https://docs.posit.co/rspm/admin/serving-binaries.html)、
[renvのパッケージ取得元](https://pkgs.rstudio.com/renv/articles/package-sources.html)。
