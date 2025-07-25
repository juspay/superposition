#!/bin/bash
# Superposition MCP Server Setup Script
# This script sets up the development or production environment

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Configuration
PYTHON_MIN_VERSION="3.8"
NODE_MIN_VERSION="16"

# Functions
log() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

check_command() {
    if command -v "$1" &>/dev/null; then
        return 0
    else
        return 1
    fi
}

version_compare() {
    printf '%s\n%s\n' "$2" "$1" | sort -V -C
}

check_python() {
    log "Checking Python installation..."

    if ! check_command python3; then
        error "Python 3 is required but not installed. Please install Python 3.8 or higher."
    fi

    local python_version=$(python3 -c 'import sys; print(".".join(map(str, sys.version_info[:2])))')
    log "Found Python $python_version"

    if ! version_compare "$python_version" "$PYTHON_MIN_VERSION"; then
        error "Python $PYTHON_MIN_VERSION or higher is required. Found: $python_version"
    fi

    success "Python version is compatible"
}

check_pip() {
    log "Checking pip installation..."

    if ! check_command pip3; then
        warn "pip3 not found. Attempting to install..."
        python3 -m ensurepip --upgrade || {
            error "Failed to install pip. Please install pip3 manually."
        }
    fi

    # Upgrade pip
    python3 -m pip install --upgrade pip
    success "pip is ready"
}

check_git() {
    log "Checking Git installation..."

    if ! check_command git; then
        warn "Git is not installed. Some features may not work correctly."
        return 1
    fi

    success "Git is available"
}

check_docker() {
    log "Checking Docker installation..."

    if ! check_command docker; then
        warn "Docker is not installed. Docker features will be skipped."
        return 1
    fi

    if ! docker info &>/dev/null; then
        warn "Docker daemon is not running. Please start Docker."
        return 1
    fi

    success "Docker is ready"
}

setup_virtual_environment() {
    log "Setting up virtual environment..."

    cd "$PROJECT_DIR"

    if [[ -d "venv" ]]; then
        warn "Virtual environment already exists. Skipping..."
    else
        python3 -m venv venv
        source venv/bin/activate

        # Upgrade pip in virtual environment
        pip install --upgrade pip

        success "Virtual environment created"
    fi

}

install_dependencies() {
    local install_type="$1"

    log "Installing dependencies for $install_type..."

    cd "$PROJECT_DIR"
    source venv/bin/activate

    case "$install_type" in
    "development")
        pip install -e ".[dev,metrics,redis]"
        ;;
    "production")
        pip install -e ".[metrics,redis]"
        ;;
    "minimal")
        pip install -e .
        ;;
    *)
        error "Unknown install type: $install_type"
        ;;
    esac

    success "Dependencies installed"
}

setup_configuration() {
    local env_type="$1"

    log "Setting up configuration for $env_type environment..."

    cd "$PROJECT_DIR"

    # Create directories
    mkdir -p config logs cache

    # Copy configuration template
    local template_file="examples/${env_type}-config.env"
    local config_file=".env"

    if [[ -f "$template_file" ]]; then
        cp "$template_file" "$config_file"
        log "Configuration template copied to $config_file"
    else
        # Create basic configuration
        cat >"$config_file" <<EOF
# Superposition MCP Server Configuration
SUPERPOSITION_API_URL=https://your-superposition-instance.com
SUPERPOSITION_API_TOKEN=your-bearer-token
SUPERPOSITION_DEFAULT_WORKSPACE=dev
SUPERPOSITION_DEFAULT_ORG=juspay
SUPERPOSITION_TIMEOUT=30
SUPERPOSITION_DEBUG=false
SUPERPOSITION_LOG_LEVEL=INFO
EOF
        log "Basic configuration created at $config_file"
    fi

    # Set secure permissions
    chmod 600 "$config_file"

    warn "Please edit $config_file with your actual Superposition instance details"
    success "Configuration setup complete"
}

setup_pre_commit() {
    log "Setting up pre-commit hooks..."

    cd "$PROJECT_DIR"
    source venv/bin/activate

    if check_command pre-commit; then
        pre-commit install
        success "Pre-commit hooks installed"
    else
        warn "pre-commit not available. Install development dependencies to enable."
    fi
}

run_tests() {
    log "Running tests..."

    cd "$PROJECT_DIR"
    source venv/bin/activate

    if check_command pytest; then
        pytest -v --tb=short
        success "Tests completed"
    else
        warn "pytest not available. Install development dependencies to run tests."
    fi
}

setup_docker() {
    log "Setting up Docker environment..."

    cd "$PROJECT_DIR"

    # Copy environment file for Docker
    if [[ -f ".env" ]]; then
        cp .env docker.env
        log "Docker environment file created"
    fi

    # Build Docker image
    if check_docker; then
        docker build -t superposition-mcp:latest .
        success "Docker image built"
    else
        warn "Docker not available. Skipping Docker setup."
    fi
}

setup_monitoring() {
    log "Setting up monitoring configuration..."

    cd "$PROJECT_DIR"
    mkdir -p monitoring/grafana/{dashboards,datasources}

    # Create Prometheus configuration
    cat >monitoring/prometheus.yml <<EOF
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'superposition-mcp'
    static_configs:
      - targets: ['superposition-mcp:9090']
    scrape_interval: 10s
    metrics_path: /metrics
EOF

    # Create Grafana datasource configuration
    cat >monitoring/grafana/datasources/prometheus.yml <<EOF
apiVersion: 1
datasources:
  - name: Prometheus
    type: prometheus
    url: http://prometheus:9090
    access: proxy
    isDefault: true
EOF

    success "Monitoring configuration created"
}

validate_setup() {
    log "Validating setup..."

    cd "$PROJECT_DIR"

    # Check virtual environment
    if [[ ! -d "venv" ]]; then
        error "Virtual environment not found"
    fi

    # Check configuration
    if [[ ! -f ".env" ]]; then
        error "Configuration file not found"
    fi

    # Test Python import
    if ! venv/bin/python -c "import superposition_mcp" 2>/dev/null; then
        error "Package import failed"
    fi

    # Test configuration loading
    if ! venv/bin/python -c "from superposition_mcp.config import Config; Config()" 2>/dev/null; then
        warn "Configuration validation failed. Please check your .env file"
    fi

    success "Setup validation complete"
}

print_next_steps() {
    local setup_type="$1"

    echo
    echo "======================================"
    success "Setup completed successfully!"
    echo "======================================"
    echo
    log "Next steps:"
    echo

    case "$setup_type" in
    "development")
        echo "1. Activate virtual environment:"
        echo "   source venv/bin/activate"
        echo
        echo "2. Edit configuration:"
        echo "   nano .env"
        echo
        echo "3. Run the server:"
        echo "   python -m superposition_mcp.main"
        echo
        echo "4. Run tests:"
        echo "   pytest"
        echo
        echo "5. Start development with auto-reload:"
        echo "   watchfiles python -m superposition_mcp.main src/"
        ;;
    "production")
        echo "1. Edit configuration:"
        echo "   nano .env"
        echo
        echo "2. Install as system service:"
        echo "   sudo ./scripts/install.sh"
        echo
        echo "3. Start the service:"
        echo "   sudo systemctl start superposition-mcp"
        ;;
    "docker")
        echo "1. Edit Docker environment:"
        echo "   nano docker.env"
        echo
        echo "2. Start with Docker Compose:"
        echo "   docker-compose up -d"
        echo
        echo "3. View logs:"
        echo "   docker-compose logs -f superposition-mcp"
        ;;
    esac

    echo
    log "Useful commands:"
    echo "  ./scripts/setup.sh --help    : Show help"
    echo "  ./scripts/install.sh         : Install as system service"
    echo "  ./scripts/health-check.sh    : Check server health"
    echo
    log "Documentation: .ai/docs/mcp-server/README.md"
    log "Configuration: .ai/docs/mcp-server/CONFIGURATION.md"
    log "Troubleshooting: .ai/docs/mcp-server/TROUBLESHOOTING.md"
}

show_help() {
    cat <<EOF
Superposition MCP Server Setup Script

Usage: $0 [OPTIONS]

Options:
  -t, --type TYPE        Setup type: development, production, docker (default: development)
  -s, --skip-deps        Skip dependency installation
  -n, --no-tests         Skip running tests
  -d, --docker           Setup Docker environment
  -m, --monitoring       Setup monitoring stack
  -h, --help             Show this help message

Setup Types:
  development    : Full development environment with dev dependencies
  production     : Production environment with minimal dependencies
  docker         : Docker-based setup

Examples:
  $0                           # Development setup
  $0 -t production             # Production setup
  $0 -d -m                     # Docker setup with monitoring
  $0 -t development -n         # Development setup without tests

EOF
}

# Main function
main() {
    local setup_type="development"
    local skip_deps=false
    local no_tests=false
    local setup_docker=false
    local setup_monitoring=false

    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
        -t | --type)
            setup_type="$2"
            shift 2
            ;;
        -s | --skip-deps)
            skip_deps=true
            shift
            ;;
        -n | --no-tests)
            no_tests=true
            shift
            ;;
        -d | --docker)
            setup_docker=true
            shift
            ;;
        -m | --monitoring)
            setup_monitoring=true
            shift
            ;;
        -h | --help)
            show_help
            exit 0
            ;;
        *)
            error "Unknown option: $1"
            ;;
        esac
    done

    log "Starting Superposition MCP Server setup (type: $setup_type)..."

    # System checks
    check_python
    check_pip
    check_git

    if [[ "$setup_docker" == true ]]; then
        check_docker
    fi

    # Setup virtual environment
    setup_virtual_environment

    # Install dependencies
    if [[ "$skip_deps" != true ]]; then
        install_dependencies "$setup_type"
    fi

    # Setup configuration
    setup_configuration "$setup_type"

    # Setup pre-commit hooks for development
    if [[ "$setup_type" == "development" ]]; then
        setup_pre_commit
    fi

    # Run tests
    if [[ "$no_tests" != true && "$setup_type" == "development" ]]; then
        run_tests
    fi

    # Setup Docker if requested
    if [[ "$setup_docker" == true ]]; then
        setup_docker
    fi

    # Setup monitoring if requested
    if [[ "$setup_monitoring" == true ]]; then
        setup_monitoring
    fi

    # Validate setup
    validate_setup

    # Print next steps
    if [[ "$setup_docker" == true ]]; then
        print_next_steps "docker"
    else
        print_next_steps "$setup_type"
    fi
}

# Handle script interruption
trap 'error "Setup interrupted"' INT TERM

# Run main function
main "$@"
