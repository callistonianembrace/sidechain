# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.187.0/containers/javascript-node/.devcontainer/base.Dockerfile

# [Choice] Node.js version: 16, 14, 12
ARG VARIANT="16-buster"
FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:0-${VARIANT}

# [Optional] Uncomment this section to install additional OS packages.
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends rsync git m4 build-essential patch unzip wget pkg-config libgmp-dev libev-dev libhidapi-dev libffi-dev opam jq zlib1g-dev inotify-tools

# [Optional] Uncomment if you want to install an additional version of node using nvm
# ARG EXTRA_NODE_VERSION=10
# RUN su node -c "source /usr/local/share/nvm/nvm.sh && nvm install ${EXTRA_NODE_VERSION}"

# [Optional] Uncomment if you want to install more global node modules
RUN su node -c "sudo npm install -g --unsafe-perm esy"