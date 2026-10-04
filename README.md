# personal-slides

医学・統計学の学習ノートをQuartoでまとめた個人サイトです。

公開サイト：[personal-slides](https://figarofuga.github.io/personal-slides/)

## 基本方針

**普段は既存の環境を使い、パッケージの追加・更新時だけ環境を変更します。**
以下のコマンドは、`pixi.toml`があるプロジェクト直下で実行してください。
`パッケージ名`は、追加したい名前に置き換えます。

| やりたいこと | コマンド |
| --- | --- |
| サイトをプレビュー | `pixi run --as-is preview` |
| サイト全体を生成 | `pixi run --as-is render` |
| 既存環境でコマンドを実行 | `pixi run --as-is コマンド` |
| 既存環境のシェルを開く | `pixi shell --as-is` |

`--as-is`はインストール・ビルド・ロックファイル更新を行いません。初回は先にセットアップが必要です。

## 初回セットアップ

ローカルのVS Code Dev ContainersとGitHub Codespacesで、同じ
`.devcontainer/devcontainer.json`・Dockerfile・セットアップスクリプトを使用します。
PCごとのパスやローカルイメージ名を設定する必要はありません。
再構築に使う`vendor/`・`pixi.toml`・`pixi.lock`は、同じGitリビジョンから取得してください。
対応環境はLinux x86_64（`linux-64`）です。
コンテナのビルドと起動には`linux/amd64`を指定しています。
Apple SiliconなどARMのPCではDockerのエミュレーションを使うため、特に初回ビルドが遅くなります。

### 新しいPCでローカル開発

1. Dockerを用意します。WindowsではDocker DesktopのLinux containersと
   WSL 2を使い、Settings → Resources → WSL Integrationで使用するUbuntuを有効にします。
2. VS Code・Dev Containers拡張機能・Gitを用意します。WindowsのWSLから開く場合は
   WSL拡張機能も入れます。
3. このリポジトリをcloneし、プロジェクト直下をVS Codeで開きます。
   WindowsではWSL内のLinuxファイルシステムにcloneすると、vendorのビルドが速くなります。
4. 「Dev Containers: Reopen in Container」を実行します。
   Dockerfileのビルド、Pixi環境のインストール、動作確認が自動で実行されます。

cloneとVS Code起動の例（WindowsではWSLのターミナルで実行）：

```bash
git clone https://github.com/figarofuga/personal-slides.git
cd personal-slides
code .
```

ホスト側にR・Python・Quarto・Pixiを個別にインストールする必要はありません。
初回はパッケージのダウンロードとvendorのビルドに時間がかかります。
メモリ16 GB・Dockerの空き容量64 GB以上を目安にしてください。

このPCでWSLCから切り替える場合は、VS Codeユーザー設定の
`dev.containers.dockerPath`を`docker`に戻します（今回変更済み）。
Docker Desktopを起動し、WSL統合を有効にしてから、WSL側で
`docker version`のClientとServerの両方が表示されることを確認してください。
WSLCのイメージや環境ボリュームはDockerと共有されないため、Docker側で環境を構築します。
以前のDockerコンテナがある場合は「Dev Containers: Rebuild Container」で
今回のDockerfileと設定を反映してください。

### GitHub Codespacesで開発

1. `.devcontainer/`・`.vscode/settings.json`・`vendor/`・`pixi.toml`・`pixi.lock`・
   `scripts/`・`README.md`の変更を
   同じGitリビジョンにcommit・pushします。`.pixi`やビルド済みバイナリのコピーは不要です。
2. GitHubでリポジトリの「Code → Codespaces → Create codespace」を選びます。
3. 初回セットアップの完了を待ち、ブラウザーまたはVS Codeから開発します。
   ローカルと同じ`pixi run --as-is ...`コマンドを使えます。

別PCから同じCodespaceに接続すれば、そのクラウド環境を継続して使えます。
ローカルとCodespacesのソース変更はGitのcommit・push・pullで同期します。
環境の定義を変更した場合は「Dev Containers: Rebuild Container」または
「Codespaces: Rebuild Container」で再構築できます。

[Dev Containersの公式手順](https://code.visualstudio.com/docs/devcontainers/create-dev-container)・
[Codespacesの公式手順](https://docs.github.com/en/codespaces/setting-up-your-project-for-codespaces/adding-a-dev-container-configuration/introduction-to-dev-containers)

Dev ContainerではPixi **0.81.0**を使います。初回作成時に
`.devcontainer/setup.sh`が`.pixi`の権限を設定し、`pixi install --locked`で
環境をインストールして、vendorのRパッケージ・Python・Quartoの起動を確認します。
`r-gam`が必要とするOpenBLASもPixiの依存関係に含め、ホストのシステムライブラリに
依存せず読み込めるようにしています。
再構築にはネットワーク接続と、ロックに記録された配布パッケージが必要です。
通常のコンテナ起動では自動インストールを行わず、既存の永続ボリュームを使用します。

既存コンテナでセットアップを再実行する場合は、次を使います。

```bash
bash .devcontainer/setup.sh
```

Dev Containerを使わない場合も、Pixi 0.81.0を使って次を実行します。
Gitから取得した変更で`pixi.lock`が更新された場合も、このコマンドを使います。

```bash
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install --locked
```

`--locked`は、ソース・設定とロックが一致しない場合に停止します。
再構築時に`--frozen`で不一致を無視したり、通常の`pixi install`で
新しいバージョンを解決したりしないでください。vendorのビルドツールも
バージョンを固定しています。更新時は設定とロックを一緒に更新・保存します。

direnvを使っている場合は、その後に`direnv reload`を実行します。
使っていなければターミナルを開き直してください。起動中のRセッションも再起動します。

## サイトのプレビュー・生成

```bash
# プレビュー
pixi run --as-is preview

# サイト全体を生成（出力先はdocs/）
pixi run --as-is render

# 1つのファイルだけ生成
pixi run --as-is quarto render statistics/model_performance/index.qmd --no-clean
pixi run --as-is quarto render medicine/antithrombotic_etc/index.qmd --no-clean
```

`render`タスクはサイト全体を生成します。個別のファイルを指定する場合は、`quarto render`を使います。

## Rパッケージの追加・更新

新しいLinux統合ターミナルには、意図しない環境変更を避けるため、
`PIXI_NO_INSTALL=true`と`PIXI_FROZEN=true`を設定しています。
以下の`env -u ...`は、そのコマンドだけ制限を解除します。エディター外でも同じ書き方を使えます。

### conda-forgeにあるパッケージ

通常はRパッケージ名を小文字にして`r-`を付けます。
例えば、`ggplot2`は`r-ggplot2`、`MASS`は`r-mass`です。

```bash
# conda-forgeにあるか確認
pixi search --channel https://prefix.dev/conda-forge r-パッケージ名

# 追加・インストール（複数の名前を並べてもよい）
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi add r-パッケージ名
```

`pixi add`が`pixi.toml`、`pixi.lock`、環境を更新します。追加の`pixi install`は不要です。

### conda-forgeにないパッケージをvendorへ追加

追加スクリプトでCRANのソースを取得し、ローカルパッケージとして登録します。
こちらは、`ExclusionTable`など、**Rの正式な名前の大文字・小文字を保って**指定します。

```bash
# ソースをvendorへ配置し、pixi.tomlに登録
pixi run --as-is Rscript scripts/vendor_pixi_r_packages.R パッケージ名

# ロックファイルを更新し、ビルド・インストール
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install
```

スクリプトは標準ではPure Rパッケージ向けです。コンパイルやシステムライブラリが必要な
パッケージには、個別のビルド設定が必要です。依存パッケージの利用可否もスクリプトが確認します。

### vendorの既存パッケージを更新

```bash
pixi run --as-is Rscript scripts/vendor_pixi_r_packages.R --upgrade パッケージ名
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install
```

取得先に新しいバージョンがある場合だけソースを更新します。
既に同じバージョン、またはvendor側の方が新しければ、そのソースを変更しません。

### パッケージを削除

```bash
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi remove r-パッケージ名
```

vendorで追加したものは、依存関係から削除した後、対応する`vendor/正式なパッケージ名/`も削除します。

### pixi.tomlを直接編集した場合

vendor追加などで`pixi.toml`を変更した場合は、ロックファイルも更新するため、
**`--locked`や`--frozen`を付けずに**インストールします。

```bash
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install
```

追加・更新・削除後はRセッションを再起動してください。direnvを使う場合は`direnv reload`も実行します。

## VS Code・Positronの設定

設定は`.vscode/settings.json`にまとめています。

- **VS Code**：PixiのR、arfコンソール、httpgdのPlot viewerを使用します。
  コマンドパレットの`R: Create R terminal`でRを起動します。
- **Positron**：Pixi自動検出を無効にし、既存のRを直接登録しています。
  自動検出による再インストールを避けるための設定です。
- **負荷対策**：`.pixi`全体をExplorer、ファイル監視、検索、Python解析から除外しています。

現在の`pixi.lock`にはR 4.5.3が記録されています。
arf・radian起動スクリプトとdirenvの`.envrc`は、既存環境を`--as-is`で有効化します。

設定の変更後はエディターを再読み込みし、ターミナルとRセッションを作り直します。
フォルダーを移動した場合やDev Containerで使う場合は、Positronの`customBinaries`と
`interpreters.default`の絶対パスを、その環境の`.pixi/envs/default/bin/R`に合わせて変更してください。

## ビルドキャッシュの扱い

| フォルダー | 内容 |
| --- | --- |
| `.pixi/envs/default` | 普段使うインストール済み環境 |
| `.pixi/bld` | ローカルパッケージのビルド作業用データ・キャッシュ |

通常の起動では`--as-is`を使い、ビルドを行いません。
明示的なインストール・更新時には`.pixi/bld`が生成される場合があります。
`--frozen`だけでは、インストールやビルドは止まりません。

ビルドが終了した後、不要なビルドキャッシュは次で削除できます。

```bash
pixi clean --build
```

キャッシュを削除しても、次にビルドが必要になれば再生成されます。
インストール済み環境を残すため、`.pixi`全体は削除しないでください。

## 変更の保存・公開

パッケージを追加・更新した場合は、環境定義とvendorの変更を保存します。

```bash
git add pixi.toml pixi.lock vendor/
git commit -m "Rパッケージを追加・更新"
```

記事を編集した場合は、サイトを生成してから変更を保存・送信します。
`docs/`はGitHub Pagesで公開するサイトの出力先です。

```bash
pixi run --as-is render
git add .
git commit -m "update model performance slides"
git push
```

## フォルダー構成

```text
personal-slides/
├── .devcontainer/       # Dev Containerの設定
├── .vscode/             # VS Code・Positronの設定
├── pixi.toml            # 依存パッケージ・タスクの定義
├── pixi.lock            # 使用するパッケージのバージョンを固定
├── scripts/             # R起動・vendor追加・サイト生成
├── vendor/              # ローカルでビルドするRパッケージ
├── _quarto.yml          # Quartoの設定
├── index.qmd            # トップページ
├── statistics/          # 統計学のノート
├── medicine/            # 医学のノート
└── docs/                # 公開サイトの生成先
```

## 今後まとめたい内容

- 医学：高齢者の心不全、リケッチア、好酸球増多、低カリウム血症、急性重症低ナトリウム血症の治療、
  Whipple病、VTEの血栓性素因検査、血液ガス、妊娠中の救急診療、敗血症の輸液。
- 統計学：クラスタリング、DAG、モデル性能評価、可視化、時間依存Coxモデルなどの生存時間解析、
  スプライン、傾向スコア解析、多重代入。
- 修正：モデル性能評価のページ。

## Quartoの書き方メモ

文字の色・大きさを指定する例です。

```markdown
[**Calibration plot**]{style="color: red; font-size: 1.1em;"}

::: {style="color: red; font-size: 1.2em;"}
- **目の前の、酸塩基の異常を魔法で消し去ったらどうなる？**
:::
```
