# Docker Desktop・Codespacesでの開発

ローカルではDocker DesktopのWSL2バックエンドでDev Containerを使います。
GitHub Codespacesでも同じDockerfile・Pixi・renvの環境定義を使い、
iPadや他のPCからブラウザで開発できます。
R・Python・Quarto・CmdStan・コンパイラはコンテナ内で復元します。

環境定義は2026-10-17の`3316c2c`を基準とし、R 4.5.3と454個のRパッケージを使います。
`restore-renv.R`などの復元・検証スクリプトはこの時点から使われているため維持し、
参照されていなかった`radian-pixi.sh`は削除しています。

## 新しいWindows PCでの準備

Windows x64、WSL2のLinuxディストリビューション（例：Ubuntu）、Git、Docker Desktop、
Windows版VS Code、WSL拡張、Dev Containers拡張を用意します。

1. Docker Desktopを起動し、**Use WSL 2 based engine**を有効にします。
   既定で有効になっている環境では、この項目は表示されない場合があります。
2. **Settings → Resources → WSL Integration**で使うディストリビューションを有効にします。
3. WSLのターミナルで`docker version`を実行し、ClientとServerの情報が出ることを確認します。

詳細は[Docker DesktopのWSL2バックエンド](https://docs.docker.com/desktop/features/wsl/)を参照してください。

VS Codeの **Preferences: Open User Settings (JSON)** で、
既存の`dev.containers.dockerPath`がある場合は、その値を`docker`に戻します。
既存のほかの設定は残してください。

```json
{
  "dev.containers.dockerPath": "docker"
}
```

これはVS Codeのユーザー設定で、Gitでは配布されません。
新しいPCでも、明示的に設定している場合は値を確認してください。

WSLのLinuxファイルシステム内（`/home/<user>/`以下）でリポジトリを取得します。

```bash
mkdir -p ~/analysis
cd ~/analysis
git clone https://github.com/figarofuga/personal-slides.git
cd personal-slides
code .
```

コマンドパレットから **Dev Containers: Reopen in Container** を実行します。
Dockerfileのビルド後、`pixi install --locked`が自動実行されます。
エディターはPixiの復元完了前に開くため、コンテナ内のターミナルで進捗を確認してください。

```bash
tail -f .devcontainer/setup.log
```

`Pixi base ready.`が出たら`Ctrl+C`でログ表示を終了し、Rパッケージを復元します。

```bash
bash .devcontainer/setup.sh
```

`Setup completed.`まで確認してからRやQuartoを使います。
初回はダウンロードとソースビルドが必要なため、時間がかかります。
Gitで配布されるのは環境定義であり、インストール済みの環境ではありません。

## 既存のPCで環境定義を更新する

自分の作業をcommitなどで保存してから、リポジトリ直下で更新します。

```bash
git pull --ff-only
```

Dockerfileや`devcontainer.json`が変わった場合は、
**Dev Containers: Rebuild Container**でコンテナを再構築します。
コンテナ外から再構築する場合は **Dev Containers: Rebuild and Reopen in Container** を使います。
Pixiの初期化が終わってから、コンテナ内で次を実行してください。

```bash
bash .devcontainer/setup.sh
```

復元後は起動中のRを再起動します。direnvを使っている場合は`direnv reload`も実行します。
`pixi.toml`・`pixi.lock`・`renv.lock`・`.Rprofile`・`renv/`・`scripts/`・
`.devcontainer/`を同じGitリビジョンで取得してください。

## 以前の構成からDocker Desktopへ切り替える場合

VS Codeのユーザー設定を上記の`docker`に戻し、WSL側でプロジェクトを開き直して、
**Dev Containers: Rebuild and Reopen in Container** を実行します。
コンテナ定義はDockerの`--platform=linux/amd64`を使い、Linux x86_64環境を作ります。
UID/GIDはDev Containersの標準の自動調整を使います。

Docker Desktopのイメージ・ボリュームは、以前のコンテナ実行基盤とは別に管理されます。
Docker側に既存の環境ボリュームがなければ、上記の初回復元を行ってください。

## GitHub Codespaces・iPad

使用する環境定義をcommit・pushした後、GitHubの
**Code → Codespaces → Create codespace** から対象ブランチのCodespaceを作成します。
`devcontainer.json`の`hostRequirements`は、最低4コア・メモリ16 GB・ストレージ32 GBです。
作成画面では、この条件を満たすマシンを選びます。
実際の復元には空き容量64 GBを目安にしてください。

ローカルと同じく、初期化ログの`Pixi base ready.`を確認してから、
Codespace内のターミナルでRパッケージを復元します。

```bash
bash .devcontainer/setup.sh
```

iPadや他のPCからは、そのCodespaceをブラウザで開きます。
ビルドと計算はGitHub側で実行されるため、アクセスする端末にDocker Desktopは不要です。
環境定義を更新した既存のCodespaceでは **Codespaces: Rebuild Container** を実行し、
Pixiの初期化後にセットアップを再実行します。

設定の詳細：[Codespacesの最低マシンスペック](https://docs.github.com/en/codespaces/setting-up-your-project-for-codespaces/configuring-dev-containers/setting-a-minimum-specification-for-codespace-machines)、
[Codespaceの再構築](https://docs.github.com/en/codespaces/developing-in-a-codespace/rebuilding-the-container-in-a-codespace)。

## キャッシュ・リソース・復元時の確認

`.pixi`は`${localWorkspaceFolderBasename}-pixi`という名前のボリュームに保存します。
ローカルではフォルダー名が`pixi-test`なら`pixi-test-pixi`です。
ボリュームを残した通常の再構築では、Pixi環境・Rライブラリ・renvキャッシュを再利用します。
新しいPCや新しいCodespaceには、そのキャッシュがない前提で復元してください。
同じ名前のフォルダーを同じDocker環境で複数使うとボリューム名が重なるため、
異なる環境を並行して使う場合はフォルダー名も分けます。
CodespacesのFull Rebuildなどでボリュームが失われた場合も、環境の再インストールが必要です。

リソースの目安は4コア・メモリ16 GB・空き容量64 GBです。
ローカルではDocker DesktopとWSL2側で利用可能なリソースを確保します。
`hostRequirements`だけでDockerに割り当てるCPUやメモリが増えるわけではありません。
対応環境は`linux-64`です。Apple Siliconではamd64のエミュレーションを使います。

Rパッケージの取得先は`.Rprofile`のP3M portableバイナリのURLを使います。
ライブラリ・キャッシュ・ダウンロード先は`.pixi/`内のP3M専用ディレクトリです。
プラットフォームごとのサブディレクトリは、この構成のrenvが自動で選びます。
詳細は[ルートREADMEのパッケージ取得元](../README.md#パッケージの取得元)を参照してください。

R・Python・Stan・Quartoを含む統合検証は、復元後に次を実行します。

```bash
pixi run --as-is check
```

復元から統合検証までまとめて行う場合は`bash .devcontainer/setup.sh --check`を使います。
進捗と失敗時の出力は`.devcontainer/setup.log`に追記されます。
Dockerのビルド段階で失敗した場合は、ローカルではDev Containersのログ、
Codespacesでは作成ログを確認してください。

## 検証の記録

[P3M移行の検証結果](p3m-validation.md)に、2026-10-12のWSL2環境での
R・Python・Stan・Quartoの検証を記録しています。
2026-10-25には、移行前のwslc環境で全458パッケージのバージョン・
ライブラリの参照先・名前空間の読み込みと、SVG/PNG描画・QuartoのR/Python実行を確認しました。
その結果は、Docker Desktopに戻した構成やCodespacesでの動作確認を意味しません。
現在のロックは10月17日の454パッケージに戻しており、上記458パッケージのロックとは異なります。
10月17日の設定へ戻した後、WSL2側の既存ライブラリをロックに揃え、
`renv::status()`の整合性と`jsonlite 2.0.0`の読み込みを確認しました。
VS Code R拡張の関数一覧取得処理（`getAliases.R`）も正常終了しています。
Docker Desktopでの再ビルド、Codespaces実機、iPadでの操作は今回は実施していません。
