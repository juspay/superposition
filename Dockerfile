FROM rust:1.86.0 as builder

WORKDIR /build

# install nodeJS for functions
ENV NVM_DIR /usr/local/nvm
ENV NODE_VERSION 18.19.0

RUN mkdir -p $NVM_DIR
RUN curl "https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh" | bash \
    && . $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

ENV NODE_PATH $NVM_DIR/v$NODE_VERSION/lib/node_modules
ENV PATH="${NVM_DIR}/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version

RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

COPY . .
RUN npm ci --loglevel=info

RUN cd crates/context_aware_config/ && npm ci
RUN mkdir -p target/node_modules
RUN cp -a crates/context_aware_config/node_modules target/
RUN cd crates/frontend \
    && wasm-pack build --target=web --release --no-default-features --features=hydrate

# copying .wasm, .js and .css files to target/site directory
RUN mkdir -p target/site && mkdir -p target/site/pkg
RUN cd crates/frontend \
    && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
RUN mv crates/frontend/pkg target/site/
RUN cp -a crates/frontend/assets/. target/site/
RUN cp .env.example target/.env
# building backend
RUN cargo build --release --features=ssr
RUN pwd
RUN ls -l target

FROM debian:bookworm-slim as runtime

RUN mkdir -p /app/crates/superposition
ENV NODE_VERSION=18.19.0
WORKDIR /app

ARG SOURCE_COMMIT
ARG SUPERPOSITION_VERSION

RUN apt-get update && apt-get install -y libpq5 ca-certificates curl
RUN apt-get install -y postgresql-common
RUN apt-get update
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash

ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version

COPY --from=builder /build/target/release/superposition /app/superposition
COPY --from=builder /build/Cargo.toml /app/Cargo.toml
COPY --from=builder /build/target/site /app/target/site
COPY --from=builder /build/target/node_modules /app/target/node_modules
COPY --from=builder /build/workspace_template.sql /app/workspace_template.sql
# COPY --from=builder /build/superposition/target/.env /app/.env
ENV SUPERPOSITION_VERSION=$SUPERPOSITION_VERSION
ENV SOURCE_COMMIT=$SOURCE_COMMIT
CMD ["/app/superposition"]
