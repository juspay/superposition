# Multi-stage build for Superposition MCP Server
FROM python:3.11-slim as builder

# Set environment variables
ENV PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Create virtual environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Copy requirements and install dependencies
WORKDIR /app
COPY pyproject.toml README.md ./
COPY src/ ./src/

# Install the application and dependencies
RUN pip install --upgrade pip && \
    pip install -e .

# Production stage
FROM python:3.11-slim as production

# Set environment variables
ENV PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PATH="/opt/venv/bin:$PATH"

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user
RUN groupadd -r appuser && useradd -r -g appuser appuser

# Copy virtual environment from builder stage
COPY --from=builder /opt/venv /opt/venv

# Copy application code
WORKDIR /app
COPY --from=builder /app/src ./src
COPY --chown=appuser:appuser scripts/ ./scripts/

# Create directories for logs, cache, and config
RUN mkdir -p /app/logs /app/cache /app/config && \
    chown -R appuser:appuser /app

# Switch to non-root user
USER appuser

# Expose port (if running as HTTP server)
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "import asyncio; from superposition_mcp.health import health_check; asyncio.run(health_check())" || exit 1

# Default command
CMD ["python", "-m", "superposition_mcp.main"]

# Development stage
FROM production as development

# Switch back to root to install dev dependencies
USER root

# Install development dependencies
RUN pip install -e ".[dev]"

# Install additional development tools
RUN apt-get update && apt-get install -y \
    vim \
    less \
    htop \
    && rm -rf /var/lib/apt/lists/*

# Switch back to appuser
USER appuser

# Override command for development
CMD ["python", "-m", "superposition_mcp.main", "--debug"]