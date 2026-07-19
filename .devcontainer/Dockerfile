FROM mcr.microsoft.com/devcontainers/base:jammy

ARG PIXI_VERSION=v0.70.2

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        fontconfig \
        fonts-ipaexfont \
        fonts-noto-cjk \
    && fc-cache -f \
    && fc-match -f '%{family}\n' "Noto Sans CJK JP" | grep -F "Noto Sans CJK JP" \
    && fc-match -f '%{family}\n' "IPAexGothic" | grep -F "IPAexGothic" \
    && rm -rf /var/lib/apt/lists/*

RUN curl -L -o /usr/local/bin/pixi -fsSL --compressed "https://github.com/prefix-dev/pixi/releases/download/${PIXI_VERSION}/pixi-$(uname -m)-unknown-linux-musl" \
    && chmod +x /usr/local/bin/pixi \
    && pixi info

# set some user and workdir settings to work nicely with vscode
USER vscode
WORKDIR /home/vscode

RUN echo 'eval "$(pixi completion -s bash)"' >> /home/vscode/.bashrc
