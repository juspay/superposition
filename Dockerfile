FROM rust:1.67 as builder
WORKDIR /build
COPY . .
RUN mkdir -p ~/.ssh && ssh-keyscan ssh.bitbucket.juspay.net >> ~/.ssh/known_hosts
RUN --mount=type=ssh cargo build --release
RUN cargo build --release

FROM debian:bullseye-slim
WORKDIR /app

ARG SOURCE_COMMIT
ARG CONTEXT_AWARE_CONFIG_VERSION

RUN apt-get update && apt-get install -y libpq5 ca-certificates
COPY --from=builder /build/target/release/context-aware-config /app/context-aware-config
ENV CONTEXT_AWARE_CONFIG_VERSION=$CONTEXT_AWARE_CONFIG_VERSION
ENV SOURCE_COMMIT=$SOURCE_COMMIT
CMD ["/app/context-aware-config"]
