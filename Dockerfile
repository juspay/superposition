FROM rust:1.73 as builder
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

FROM debian:bookworm-slim
WORKDIR /app

ARG SOURCE_COMMIT
ARG CONTEXT_AWARE_CONFIG_VERSION

RUN apt-get update && apt-get install -y libpq5 ca-certificates
COPY --from=builder /build/target/release/context-aware-config /app/context-aware-config
COPY --from=builder /build/Cargo.toml /app/Cargo.toml
COPY --from=builder /build/target/site /app/target/site
ENV CONTEXT_AWARE_CONFIG_VERSION=$CONTEXT_AWARE_CONFIG_VERSION
ENV SOURCE_COMMIT=$SOURCE_COMMIT
CMD ["/app/context-aware-config"]