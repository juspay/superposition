FROM rust:1.78.0 as builder

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

RUN cargo install wasm-pack

COPY . .
RUN npm ci --loglevel=info
RUN cd crates/context_aware_config/ && npm ci
RUN mkdir -p target/node_modules
RUN cp -a crates/context_aware_config/node_modules target/
RUN cd crates/frontend \
    && wasm-pack build --target=web --no-default-features --features=hydrate

# copying .wasm, .js and .css files to target/site directory
RUN mkdir -p target/site && mkdir -p target/site/pkg
RUN cd crates/frontend \
    && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
RUN mv crates/frontend/pkg target/site/
RUN cp -a crates/frontend/assets/. target/site/
RUN cp .env.example target/.env
RUN mkdir -p target/web
RUN cp -a examples/superposition-demo-app/web/. target/web/
# building backend
RUN cargo build --release

FROM debian:bookworm-slim as runtime

ENV NODE_VERSION=18.19.0
WORKDIR /app

RUN apt-get update && apt-get install -y libpq5 ca-certificates curl supervisor
RUN apt-get install -y postgresql-common
RUN /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh -i -v 14
# Update the package lists:
RUN apt-get update
RUN mkdir -p /var/log/supervisor

COPY --from=builder /build/examples/superposition-demo-app/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY --from=builder /build/examples/superposition-demo-app/pg_hba.conf /etc/postgresql/14/main/pg_hba.conf

COPY --from=builder /build/docker-compose/postgres/db_init.sql /app/db_init.sql

COPY --from=builder /build/examples/superposition-demo-app/hba_pg_setup.sql /app/hba_pg_setup.sql

RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash

ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION}
RUN . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
RUN node --version

COPY examples/superposition-demo-app/superposition_demo.sh .
RUN chmod 774 superposition_demo.sh
COPY --from=builder /build/target/release/cac-demo-app /app/cac-demo-app
COPY --from=builder /build/target/web /app/examples/superposition-demo-app/web/
COPY --from=builder /build/target/release/superposition /app/superposition
COPY --from=builder /build/Cargo.toml /app/Cargo.toml
COPY --from=builder /build/target/site /app/target/site
COPY --from=builder /build/target/node_modules /app/target/node_modules
COPY --from=builder /build/target/.env /app/.env
CMD ["./superposition_demo.sh"]
