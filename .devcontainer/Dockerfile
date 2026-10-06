FROM mcr.microsoft.com/devcontainers/base:jammy

ARG PIXI_VERSION=v0.81.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        direnv \
        fontconfig \
        fonts-ipaexfont \
        fonts-noto-cjk \
        locales \
    && localedef -i ja_JP -f UTF-8 ja_JP.UTF-8 \
    && fc-cache -f \
    && fc-match -f '%{family}\n' "Noto Sans CJK JP" | grep -F "Noto Sans CJK JP" \
    && fc-match -f '%{family}\n' "IPAexGothic" | grep -F "IPAexGothic" \
    && rm -rf /var/lib/apt/lists/*

RUN test "$(uname -m)" = "x86_64" \
    && curl -fsSL --retry 3 -o /tmp/pixi.tar.gz "https://github.com/prefix-dev/pixi/releases/download/${PIXI_VERSION}/pixi-x86_64-unknown-linux-musl.tar.gz" \
    && curl -fsSL --retry 3 -o /tmp/pixi.tar.gz.sha256 "https://github.com/prefix-dev/pixi/releases/download/${PIXI_VERSION}/pixi-x86_64-unknown-linux-musl.tar.gz.sha256" \
    && echo "$(cut -d ' ' -f 1 /tmp/pixi.tar.gz.sha256)  /tmp/pixi.tar.gz" | sha256sum -c - \
    && tar -xzf /tmp/pixi.tar.gz -C /usr/local/bin pixi \
    && rm /tmp/pixi.tar.gz /tmp/pixi.tar.gz.sha256 \
    && pixi --version

# set some user and workdir settings to work nicely with vscode
USER vscode
WORKDIR /home/vscode

RUN echo 'eval "$(pixi completion -s bash)"' >> /home/vscode/.bashrc \
    && echo 'eval "$(direnv hook bash)"' >> /home/vscode/.bashrc
