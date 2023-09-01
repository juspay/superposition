FROM rust:1.67 as builder
WORKDIR /build
COPY . .
RUN cargo build --release

FROM debian:bullseye-slim
WORKDIR /app
RUN apt-get update && apt-get install -y libpq5 ca-certificates
COPY --from=builder /build/target/release/context-aware-config /app/context-aware-config
ENV CONTEXT_AWARE_CONFIG_VERSION=$CONTEXT_AWARE_CONFIG_VERSION
ENV SOURCE_COMMIT=$SOURCE_COMMIT
CMD ["/app/context-aware-config"]
