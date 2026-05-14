FROM ghcr.io/rocker-org/devcontainer/r-ver:4

USER root

ARG QUARTO_VERSION=1.9.37
ARG TYPST_VERSION=0.14.2

# OS packages commonly needed by R packages, Quarto, Typst, reticulate, and Japanese slides
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    wget \
    git \
    make \
    build-essential \
    pkg-config \
    python3 \
    python3-venv \
    python3-dev \
    fontconfig \
    fonts-noto \
    fonts-noto-cjk \
    fonts-noto-color-emoji \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    && rm -rf /var/lib/apt/lists/*

# Quarto CLI
RUN ARCH="$(dpkg --print-architecture)" && \
    case "$ARCH" in \
      amd64) QUARTO_ARCH="amd64" ;; \
      arm64) QUARTO_ARCH="arm64" ;; \
      *) echo "Unsupported architecture: $ARCH" && exit 1 ;; \
    esac && \
    curl -fsSL -o /tmp/quarto.deb \
      "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${QUARTO_ARCH}.deb" && \
    apt-get update && \
    apt-get install -y /tmp/quarto.deb && \
    rm -f /tmp/quarto.deb && \
    rm -rf /var/lib/apt/lists/*

# Typst CLI
RUN ARCH="$(dpkg --print-architecture)" && \
    case "$ARCH" in \
      amd64) TYPST_ARCH="x86_64-unknown-linux-musl" ;; \
      arm64) TYPST_ARCH="aarch64-unknown-linux-musl" ;; \
      *) echo "Unsupported architecture: $ARCH" && exit 1 ;; \
    esac && \
    curl -fsSL -o /tmp/typst.tar.xz \
      "https://github.com/typst/typst/releases/download/v${TYPST_VERSION}/typst-${TYPST_ARCH}.tar.xz" && \
    mkdir -p /tmp/typst && \
    tar -xJf /tmp/typst.tar.xz -C /tmp/typst --strip-components=1 && \
    install -m 0755 /tmp/typst/typst /usr/local/bin/typst && \
    rm -rf /tmp/typst /tmp/typst.tar.xz

# uv for optional Python environments and reticulate
ENV UV_NO_MODIFY_PATH=1
RUN curl -LsSf https://astral.sh/uv/install.sh | sh && \
    install -m 0755 /root/.local/bin/uv /usr/local/bin/uv && \
    install -m 0755 /root/.local/bin/uvx /usr/local/bin/uvx

USER rstudio

RUN quarto --version && typst --version && uv --version && R --version