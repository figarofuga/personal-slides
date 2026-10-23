# WSL Container（wslc）とCodespaces

ローカルではWindows標準のWSL Container（`wslc.exe`）を使います。
Docker Desktopのインストールや起動は不要です。
CodespacesではGitHub側のコンテナ基盤で、同じDockerfile・Pixi・renvの定義を使います。

## 新しいWindows PCでの準備

Windows x64、WSL2、Git、Windows版VS Code、WSL拡張、Dev Containers拡張を用意します。
検証に使った版はWSL / wslc 3.0.3.0、Dev Containers 0.470.0です。
PowerShellでバージョンを確認できます。

```powershell
wsl --version
wslc version
```

Windows版VS Codeの **Preferences: Open User Settings (JSON)** で次を設定します。
すでに`dev.containers.dockerPath`がある場合は、その値を置き換えます。

```json
{
  "dev.containers.dockerPath": "wslc.exe"
}
```

これはVS Codeのユーザー設定で、ほかのプロジェクトにも適用されます。
`.devcontainer/devcontainer.json`やリポジトリの`.vscode/settings.json`では設定できません。
WSL内からも実行できるよう、拡張子を含む`wslc.exe`を指定します。

WSLのターミナルでリポジトリを取得し、VS Codeで開きます。

```bash
git clone https://github.com/figarofuga/personal-slides.git
cd personal-slides
code .
```

コマンドパレットから **Dev Containers: Reopen in Container** を実行します。
Dockerfileのビルド後、`pixi install --locked`が自動実行されます。
エディターは復元完了前に開くため、ターミナルで進捗を確認してください。

```bash
tail -f .devcontainer/setup.log
```

`Pixi base ready.` が出たら`Ctrl+C`でログ表示を終了し、Rパッケージを復元します。

```bash
bash .devcontainer/setup.sh
```

R・Stan・Quartoの統合検証も行う場合は、上のコマンドに`--check`を付けます。
初回は依存パッケージのダウンロードとインストールが必要です。
Gitで配布されるのは環境定義であり、インストール済みの環境ではありません。

`pixi.toml`・`pixi.lock`・`renv.lock`・`.Rprofile`・`renv/`・`scripts/`も
`.devcontainer/`と同じリビジョンで取得してください。
環境定義を`git pull`した後はセットアップを再実行し、Dockerfileや
`devcontainer.json`が変わった場合は **Dev Containers: Rebuild Container** を実行します。

## wslc用の互換設定

- wslc 3.0.3では`build`・`run`の`--platform`引数が使えません。
  Linux amd64の指定はDockerfileの`FROM --platform=linux/amd64`に置いています。
- WSL側のDev Containers CLIはUID調整用ビルドにも`--platform`を追加します。
  そのため`updateRemoteUserUID: false`とし、イメージの`vscode`ユーザーを使います。
  ホストとコンテナのUID/GIDが1000の環境で、作業フォルダーへの読み書きを確認しています。
  異なるUID/GIDのホストでの利用は今回未検証です。
- `.pixi`は名前付きボリュームに置き、コンテナ再作成時にも環境を再利用します。
  同じフォルダー名の別チェックアウトは同じボリューム名になるため、
  異なる環境を並行して使う場合はチェックアウトのフォルダー名も分けてください。

リソースの目安は4コア・メモリ16 GB・空き容量64 GBです。
wslcのメモリなどは`wslc settings`で開く
`%LOCALAPPDATA%\wslc\settings.yaml`で設定します。
Docker Desktopや`.wslconfig`のリソース設定はwslcには適用されません。

## GitHub Codespaces・iPad

環境定義をcommit・pushした後、GitHubの
**Code → Codespaces → Create codespace** から作成します。
4コア・16 GB以上のマシンを選びます。
初期化ログの`Pixi base ready.`を確認した後、ローカルと同じく
`bash .devcontainer/setup.sh`でRパッケージを復元してください。

iPadや他のPCからは、そのCodespaceをブラウザで開きます。
ビルド・計算はGitHub側で実行されます。
Codespacesには`wslc.exe`やWindowsのVS Codeユーザー設定は不要です。

## 今回の検証範囲

2026-10-22（JST）、元の作業環境とは別の一時コピーと空の名前付きボリュームで検証しました。

| 確認 | 結果 |
| --- | --- |
| wslcでDockerfileをビルド | 成功 |
| Windows側のDev Containers CLIから起動・初期化 | 成功 |
| WSL側のDev Containers CLIから起動 | UID自動調整を無効にして成功 |
| WSL内の作業フォルダーと`.pixi`ボリュームの読み書き | 成功 |
| 空の環境への`pixi install --locked` | 成功。518パッケージ、約11分 |
| R 4.5.3の起動・簡単な計算 | 成功（`--vanilla`で確認） |
| NumPy・pandas・SciPyの読み込み | 成功 |
| Quarto 1.9.38の起動 | 成功 |
| 日本語フォント・ロケール | 成功 |
| R追加パッケージ全体の復元・Stan・Quartoレンダリング | 今回は未実施 |
| Codespaces実機・iPadでの操作 | 今回は未実施 |

所要時間は今回の測定値で、PC・ネットワークによって変わります。
R追加パッケージの既存WSL検証については[p3m-validation.md](p3m-validation.md)を参照してください。
その結果は今回のwslc上での全パッケージ検証を意味しません。

参考：[WSL Container](https://learn.microsoft.com/en-us/windows/wsl/wsl-container)、
[Dev Containersとの連携](https://devblogs.microsoft.com/commandline/wsl-container-is-now-available-for-public-preview/#vs-code-dev-containers)、
[wslcのリソース設定](https://learn.microsoft.com/en-us/windows/wsl/wsl-container-settings)。
