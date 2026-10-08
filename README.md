# personal-slides

医学・統計学の学習ノートをQuartoでまとめた個人サイトです。

公開サイト：[personal-slides](https://figarofuga.github.io/personal-slides/)

## 環境の管理

- **Pixi 0.81.0**：R本体、Pythonとそのパッケージ、Quarto、CmdStan、コンパイラ、make、外部ライブラリ。
- **renv**：Rの追加パッケージとrecommendedパッケージ。エディター用のパッケージも含みます。
- **Dev Container**：Linux x86_64、フォント、ロケール。

`pixi.toml`・`pixi.lock`・`renv.lock`・`.Rprofile`・`renv/activate.R`・
`renv/settings.json`・`renv/cellar/`・`scripts/`・`.devcontainer/`を同じGitリビジョンで保存します。
インストール済みの`.pixi/`と`renv/library/`はGitに含めません。

## 新しいPC・GitHub Codespaces

ローカルではDocker、VS Code、Dev Containers拡張機能、Gitを用意し、
このリポジトリをcloneして **Dev Containers: Reopen in Container** を実行します。
WindowsではDocker DesktopのWSL統合を有効にし、WSLのLinuxファイルシステム内にcloneしてください。
ホストにR・Python・Quartoを個別に入れる必要はありません。

Codespacesでは、環境定義をcommit・pushした後、GitHubの
**Code → Codespaces → Create codespace** を選びます。
ローカルと同じ`.devcontainer/setup.sh`が次を自動実行します。

1. `pixi install --locked`で固定された基盤を復元。
2. `renv::restore()`で固定されたRパッケージをソースから復元。
3. Jupyterにrenv環境のRカーネルを登録。
4. Rパッケージ、Python連携、CmdStanのコンパイルとサンプリングを確認。

初回は多数のRパッケージをコンパイルするため、環境によって数時間かかります。
復元全体が途中で打ち切られないよう、renvのインストール制限時間を24時間に設定しています。
Stan関連のコンパイル時のメモリ使用量を抑えるため、パッケージとC++ファイルはそれぞれ1つずつビルドします。
CPU 4コア、メモリ16 GB、空き容量64 GBを目安にしてください。
`.pixi`の永続ボリュームにrenvのビルド済みキャッシュも保存し、再構築時に再利用します。
新しいPCや新しいCodespaceにはそのキャッシュがない前提です。
復元にはネットワークと、ロックに記録された配布元へのアクセスが必要です。

対応環境は`linux-64`です。Apple SiliconではLinux amd64のエミュレーションとなり、
特にコンパイルに時間がかかります。Windows/macOSのネイティブRはこの構成の対象外です。

## 初回復元・環境定義をpullした後

Dev Container内では次を実行できます。

```bash
bash .devcontainer/setup.sh
```

Dev Containerを使わないLinux x86_64環境では、Pixi 0.81.0を用意し、
プロジェクト直下で次を実行します。

```bash
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install --locked
pixi run --as-is restore
pixi run --as-is check
```

復元時には`renv::snapshot()`や`renv::update()`を実行しません。
`--locked`は設定とロックが一致しない場合に停止します。
復元後、起動中のRセッションを再起動します。direnvを使う場合は`direnv reload`も実行します。

## 日常の操作

```bash
pixi run --as-is preview
pixi run --as-is render
pixi run --as-is quarto render statistics/model_performance/index.qmd --no-clean
pixi run --as-is R
pixi shell --as-is
```

`--as-is`はPixi環境の再インストールやロック更新を行いません。
Rは`.Rprofile`からrenvを有効にします。`R --vanilla`はこの処理を飛ばすので、通常の分析では使いません。
RパッケージのビルドやQuarto実行は、必ずPixiで有効化した環境から行います。

## Rパッケージの追加・更新・削除

`pixi run --as-is R`で起動したRから操作します。

```r
renv::install("パッケージ名")
# バージョン指定：renv::install("パッケージ名@バージョン")
# GitHub：renv::install("owner/repository@commit-SHA")
renv::snapshot()
renv::status()
```

更新は`renv::update("パッケージ名")`、削除は`renv::remove("パッケージ名")`の後に
`renv::snapshot()`を実行します。`snapshot.type = "all"`なので、対話的に使うパッケージや
エディター用パッケージも、プロジェクトライブラリに入っているものはすべて記録します。

RパッケージをPixiへ追加したり、PixiのR本体のライブラリへ直接インストールしたりしないでください。
新しい外部ライブラリが必要になった場合のみ、それをPixi側に追加します。

## Python・ツール・外部ライブラリの追加

```bash
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi add パッケージ名
```

R本体やコンパイラ、外部ライブラリを更新すると、既存Rパッケージの再ビルドが必要になる場合があります。
更新時はRから`renv::rebuild()`を行い、`pixi run --as-is check`と必要な記事のrenderで確認してください。
R本体のバージョンは`pixi.lock`と`renv.lock`で一致させます。

## CmdStan・Python・Jupyter・エディター

- CmdStan本体はPixiのものを使用します。`cmdstanr::install_cmdstan()`は実行しません。
- `reticulate`は`RETICULATE_PYTHON`でPixiのPythonを指定し、自動venv作成を無効にしています。
- JupyterのRカーネルはrenv管理の`IRkernel`です。`pixi run --as-is restore`で登録できます。
- VS Codeでは**R: Create R terminal**からarfを起動します。`sess`・`httpgd`・`languageserver`はrenvで復元します。
- Positronではプロジェクトの`.pixi/envs/default/bin/R`を選択してください。

## ソースの保存

通常のCRANパッケージは`renv.lock`に記録したバージョンから復元します。
GitHub由来の`cmdstanr`と`sess`はcommitを固定しています。
独自修正を維持するrmsbは`renv/cellar/`にソースを保存しています。
取得元・生成方法は`renv/cellar/README.md`を参照してください。

## 変更の保存・サイト公開

パッケージ変更時には、変更された環境定義と必要なソースを一緒にcommitします。

```bash
git add pixi.toml pixi.lock renv.lock .Rprofile renv/ scripts/ .devcontainer/
git commit -m "Update analysis environment"
```

記事を変更した場合はサイトを生成し、`docs/`を含めて保存・送信します。

```bash
pixi run --as-is render
git add .
git commit -m "Update all components"
git push
```
