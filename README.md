# personal-slides

医学・統計学の学習ノートをQuartoでまとめた個人サイトです。

公開サイト：[personal-slides](https://figarofuga.github.io/personal-slides/)

## 環境の管理

- **Pixi 0.81.0**：R本体、Pythonとそのパッケージ、Quarto、CmdStan、コンパイラ、make、外部ライブラリ。
- **renv**：Rの追加パッケージとrecommendedパッケージ。エディター用のパッケージも含みます。
- **Dev Container**：Linux x86_64、フォント、ロケール。

`pixi.toml`・`pixi.lock`・`renv.lock`・`.Rprofile`・`renv/activate.R`・
`renv/settings.json`・`scripts/`・`.devcontainer/`を同じGitリビジョンで保存します。
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

操作はプロジェクト直下で、PixiのRを起動して行います。

```bash
pixi run --as-is R
```

VS Codeの**R: Create R terminal**から起動したRでも同じ操作ができます。
このプロジェクトはrenvを初期化済みなので、`renv::init()`を再実行する必要はありません。

### 新しいRパッケージを入れる

通常どおり`renv::install()`で大丈夫です。例えば次のように指定します。

```r
renv::install("janitor")
```

複数なら`renv::install(c("janitor", "gtsummary"))`と指定できます。
必要な依存パッケージも一緒に導入されます。
インストールしただけでは、新しい構成は`renv.lock`に保存されません。
Rを再起動し、追加したパッケージを使う処理が動くことを確認してから、次を実行します。

```r
renv::snapshot()
renv::status()
```

`snapshot()`は現在のパッケージ構成を保存し、`status()`はその構成とロックの一致を確認します。
`snapshot.type = "all"`なので、対話的に使うものやエディター用のものも含めて記録されます。
確認後は変更された`renv.lock`をcommit・pushすれば、ほかのPCでも同じ構成を復元できます。

特定の版が必要な場合は`renv::install("パッケージ名@バージョン")`、
GitHubから入れる場合は`renv::install("owner/repository@commit-SHA")`を使います。
通常のCRANパッケージにはバージョン指定は不要です。

### 既存のRパッケージを更新する

まず更新候補だけを確認できます。これはインストールやロックの変更を行いません。

```r
renv::update(check = TRUE)
```

更新範囲に応じて、次のいずれかを実行します。

```r
renv::update("rmsb")                    # 1つだけ更新
renv::update(c("dplyr", "ggplot2"))     # 指定したものを更新
renv::update()                          # プロジェクト内を一括更新
```

更新後はRを再起動し、関係する分析や記事のコードを実行します。
問題なければ`renv::snapshot()`、`renv::status()`で新しい構成を記録・確認します。
さらにターミナルで次を実行してください。

```bash
pixi run --as-is check
```

このプロジェクトではStan関連に動作確認済みのバージョンの組み合わせを使っています。
一括更新ではその組み合わせも変わり得るので、このチェックでStanの実行まで確認します。
記事への影響は、該当記事の`quarto render`でも確認してください。

更新前のロックをまだ変更していなければ、問題が起きたときはRから
`renv::restore()`で記録済みのバージョンへ戻せます。
すでに`snapshot()`した後なら、Gitに保存した更新前の`renv.lock`に戻してから復元します。
renv自体も更新した場合は、`renv/activate.R`も更新前のものに戻します。
更新前に動作する環境定義をcommitしておくと、この復帰が簡単です。

renv自体だけを更新する場合は`renv::upgrade()`を使います。
これはrenvパッケージと起動用の`renv/activate.R`を更新する操作で、R本体の更新とは別です。
変更された`renv.lock`と`renv/activate.R`も保存してください。

### Rパッケージを削除する

```r
renv::remove("パッケージ名")
renv::snapshot()
renv::status()
```

RパッケージをPixiへ追加したり、PixiのR本体のライブラリへ直接インストールしたりしないでください。
新しい外部ライブラリが必要になった場合のみ、それをPixi側に追加します。

操作の詳細：[renv install](https://pkgs.rstudio.com/renv/reference/install.html)、
[update](https://pkgs.rstudio.com/renv/reference/update.html)、
[snapshot](https://pkgs.rstudio.com/renv/reference/snapshot.html)。

## R本体のバージョンをPixiで更新する

R本体はPixiで、Rパッケージはrenvで更新します。
現在は`pixi.toml`で`r-base = "==4.5.3"`と固定しているため、
`pixi update r-base`だけでは、この指定を超えるバージョンには上がりません。
更新先を指定して`pixi add`を使います。

### 1. 更新前の構成を保存する

現在のRで`renv::snapshot()`、`renv::status()`を実行し、
動作する`pixi.toml`・`pixi.lock`・`renv.lock`をGitに保存します。
その後、起動中のRセッションとQuartoプレビューを終了します。

### 2. PixiのRを更新する

ターミナルで配布されているRを確認し、`X.Y.Z`を更新したい実際のバージョンに置き換えます。

```bash
pixi search r-base
r_target_version="X.Y.Z"
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi add "r-base==${r_target_version}"
pixi run --as-is Rscript -e 'cat(as.character(getRversion()), "\n")'
```

`pixi add`が`pixi.toml`と`pixi.lock`を更新し、R本体と必要な依存関係をインストールします。
依存関係を解決できない場合は、更新先のRとコンパイラなどの指定が両立するか確認します。
この段階では`renv.lock`に旧Rのバージョンが残っているため、renvの不一致の通知は想定内です。

### 3. 新しいRでパッケージを復元する

ターミナルから新しいRを起動します。

```bash
pixi run --as-is R
```

そのRの中で次を実行します。rmsbのビルドに必要な`rstantools`を先に復元します。

```r
renv::restore(packages = "rstantools")
renv::restore()
```

**この段階では`pixi run --as-is restore`を使いません。**
このプロジェクトの復元タスクは、R本体と`renv.lock`のRバージョンが異なると停止するためです。
ここではRから直接`renv::restore()`を実行します。

例えば4.5系から4.6系へ変更すると、renvは新しいR用のライブラリとキャッシュを使います。
この構成ではソースから必要なパッケージをビルドするため、復元に数時間かかることがあります。
4.5.3から別の4.5.xへ変更する場合は同じライブラリを使うので、
新しい環境で作り直すため、続けて次を実行してください。

```r
renv::rebuild()
```

旧バージョンのパッケージが新しいRでビルドできない場合は、
`renv::restore(retry = TRUE)`で、そのパッケージの新しいバージョンによる再試行ができます。
`rstantools`の復元で止まった場合は、先に
`renv::restore(packages = "rstantools", retry = TRUE)`を実行します。
復元が完了する前に`snapshot()`すると、未復元のパッケージをロックから落としてしまうので、
すべての必要なパッケージが揃ってから次に進みます。

### 4. 新しいRの構成を保存して確認する

パッケージが揃ったら、Rで次を実行します。

```r
renv::snapshot()
renv::status()
```

これで`renv.lock`のRバージョンも新しいRに更新されます。
Rを終了し、ターミナルで次を実行します。

```bash
pixi run --as-is restore
pixi run --as-is check
```

`restore`はJupyterのRカーネルも登録し直します。
`check`はロックとの一致、Rパッケージの読み込み、Python連携、Stanの実行、Quartoを確認します。
普段使う記事もrenderしてから、変更された`pixi.toml`・`pixi.lock`・`renv.lock`を一緒にcommit・pushします。
direnvを使っている場合は`direnv reload`、エディターのRセッションも再起動します。

うまく動かない場合は、Gitに保存した更新前の環境定義一式へ戻し、
`env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install --locked`、
`pixi run --as-is restore`で旧環境を復元します。

操作の詳細：[Pixi add](https://pixi.prefix.dev/latest/reference/cli/pixi/add/)、
[renv restore](https://pkgs.rstudio.com/renv/reference/restore.html)、
[rebuild](https://pkgs.rstudio.com/renv/reference/rebuild.html)。

## Python・ツール・外部ライブラリの追加

```bash
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi add パッケージ名
```

コンパイラや外部ライブラリを更新した場合は、Rから`renv::rebuild()`を行い、
`pixi run --as-is check`と必要な記事のrenderで確認してください。
R本体の更新は上の「R本体のバージョンをPixiで更新する」に従います。

## CmdStan・Python・Jupyter・エディター

- CmdStan本体はPixiのものを使用します。`cmdstanr::install_cmdstan()`は実行しません。
- `reticulate`は`RETICULATE_PYTHON`でPixiのPythonを指定し、自動venv作成を無効にしています。
- JupyterのRカーネルはrenv管理の`IRkernel`です。`pixi run --as-is restore`で登録できます。
- VS Codeでは**R: Create R terminal**からarfを起動します。`sess`・`httpgd`・`languageserver`はrenvで復元します。
- Positronではプロジェクトの`.pixi/envs/default/bin/R`を選択してください。

## パッケージの取得元

通常のCRANパッケージは`renv.lock`に記録したバージョンから復元します。
GitHub由来の`cmdstanr`と`sess`はcommitを固定しています。
rmsbも通常のCRAN版を使い、独自ソースやパッチは保存しません。
復元スクリプトは先に`rstantools`を復元し、rmsbのインストール時に
現在のStan環境に合うC++コードを自動生成できるようにしています。

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
git commit -m "Update visualization slides"
git push
```
