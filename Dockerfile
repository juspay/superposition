FROM public.ecr.aws/docker/library/node:26.13.0-trixie-slim@sha256:5adad64f9a1fca2d51972caa65983634c8a774eb300d72b20f7f2a0f60368072 as css

WORKDIR /frontend
COPY crates/frontend/package.json crates/frontend/package-lock.json ./
RUN npm ci --ignore-scripts
COPY crates/frontend/tailwind.config.js ./
COPY crates/frontend/styles ./styles
COPY crates/frontend/src ./src
RUN npx --no-install tailwindcss -i ./styles/tailwind.css -o ./style.css

FROM public.ecr.aws/docker/library/rust:1.95.0@sha256:f49565f188ee00bc2a18dd418183f2c5f23ef7d6e691890517ed341a598f67c3 as builder

WORKDIR /build

RUN ARCH=$(dpkg --print-architecture) \
    && if [ "$ARCH" = "arm64" ]; then \
         WP_TARGET="aarch64-unknown-linux-musl"; WP_SHA256="7004853c7925ae1984033b5f25866ec604bbc921ef956d5e6c5595bc5af6d30c"; \
       else \
         WP_TARGET="x86_64-unknown-linux-musl"; WP_SHA256="410101009739b9d01c7d5affaaad6e9c9a24b4cba6791a4805db8e89b9bf7af2"; \
       fi \
    && curl -fsSL -o /tmp/wasm-pack.tar.gz "https://github.com/rustwasm/wasm-pack/releases/download/v0.15.1/wasm-pack-v0.15.1-${WP_TARGET}.tar.gz" \
    && echo "${WP_SHA256}  /tmp/wasm-pack.tar.gz" | sha256sum -c - \
    && tar -xzf /tmp/wasm-pack.tar.gz -C /tmp \
    && install -m 0755 "/tmp/wasm-pack-v0.15.1-${WP_TARGET}/wasm-pack" /usr/local/bin/wasm-pack \
    && rm -rf /tmp/wasm-pack.tar.gz "/tmp/wasm-pack-v0.15.1-${WP_TARGET}"

COPY . .

RUN cd crates/frontend \
    && wasm-pack build --target=web --release --no-default-features --features=hydrate

# copying .wasm, .js and .css files to target/site directory
RUN mkdir -p target/site && mkdir -p target/site/pkg
COPY --from=css /frontend/style.css crates/frontend/pkg/style.css
RUN mv crates/frontend/pkg target/site/
RUN cp -a crates/frontend/assets/. target/site/
RUN cp .env.example target/.env
# building backend
RUN cargo build --release --features=ssr
RUN pwd
RUN ls -l target

FROM public.ecr.aws/docker/library/ubuntu:26.04@sha256:065aa8626841f16057b78b027b464cd59c4fac36c204c72dcbf48d66f5fa8a96 as runtime


RUN mkdir -p /app/crates/superposition
WORKDIR /app

ARG SOURCE_COMMIT
ARG SUPERPOSITION_VERSION

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt \
    libapt-pkg7.0 \
    libpq5 \
    ca-certificates \
    curl \
    postgresql-common && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
# The ubuntu base image ships /usr/bin/pebble (an unused service manager)
# with unpatched golang stdlib CVEs; superposition does not use it.
RUN rm -rf /usr/bin/pebble /var/lib/pebble
    
COPY --from=builder /build/target/release/superposition /app/superposition
COPY --from=builder /build/Cargo.toml /app/Cargo.toml
COPY --from=builder /build/target/site /app/target/site
COPY --from=builder /build/workspace_template.sql /app/workspace_template.sql
ENV SUPERPOSITION_VERSION=$SUPERPOSITION_VERSION
ENV SOURCE_COMMIT=$SOURCE_COMMIT
CMD ["/app/superposition"]
