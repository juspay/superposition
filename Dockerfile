FROM public.ecr.aws/docker/library/rust:1.76.0 as builder
WORKDIR /build
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

COPY . .
RUN mkdir -p ~/.ssh && ssh-keyscan ssh.bitbucket.juspay.net >> ~/.ssh/known_hosts
RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
RUN npm ci --loglevel=info
RUN cd crates/context_aware_config/ && npm ci
RUN mkdir -p target/node_modules
RUN cp -a crates/context_aware_config/node_modules target/

# building frontend
RUN --mount=type=ssh cd crates/frontend \
    && wasm-pack build --target=web --no-default-features --features=hydrate

# copying .wasm, .js and .css files to target/site directory
RUN mkdir -p target/site && mkdir -p target/site/pkg
RUN cd crates/frontend \
    && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
RUN mv crates/frontend/pkg target/site/
RUN cp -a crates/frontend/assets/. target/site/
RUN cp .env.example .env
# building backend
RUN --mount=type=ssh cargo build --release

FROM public.ecr.aws/debian/debian:bookworm-slim
WORKDIR /app
ENV NODE_VERSION=18.19.0

ARG SOURCE_COMMIT
ARG CONTEXT_AWARE_CONFIG_VERSION

RUN apt-get update && apt-get install -y libpq5 ca-certificates curl
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version


COPY --from=builder /build/target/release/juspay_superposition /app/juspay_superposition
COPY --from=builder /build/Cargo.toml /app/Cargo.toml
COPY --from=builder /build/target/site /app/target/site
COPY --from=builder /build/target/node_modules /app/target/node_modules
ENV CONTEXT_AWARE_CONFIG_VERSION=$CONTEXT_AWARE_CONFIG_VERSION
ENV SOURCE_COMMIT=$SOURCE_COMMIT
CMD ["/app/juspay_superposition"]