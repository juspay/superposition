#!/bin/bash
# Superposition MCP Server Installation Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DEFAULT_INSTALL_DIR="/opt/superposition-mcp"
DEFAULT_USER="superposition"
DEFAULT_SERVICE_NAME="superposition-mcp"

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

check_root() {
    if [[ $EUID -eq 0 ]]; then
        error "This script should not be run as root. Please run as a regular user with sudo privileges."
    fi
}

check_dependencies() {
    log "Checking dependencies..."
    
    # Check Python
    if ! command -v python3 &> /dev/null; then
        error "Python 3 is required but not installed."
    fi
    
    local python_version=$(python3 -c 'import sys; print(".".join(map(str, sys.version_info[:2])))')
    local required_version="3.8"
    
    if ! python3 -c "import sys; exit(0 if sys.version_info >= (3,8) else 1)"; then
        error "Python 3.8 or higher is required. Found: $python_version"
    fi
    
    # Check pip
    if ! command -v pip3 &> /dev/null; then
        warn "pip3 not found. Installing..."
        sudo apt-get update && sudo apt-get install -y python3-pip || \
        sudo yum install -y python3-pip || \
        sudo dnf install -y python3-pip || \
        error "Failed to install pip3"
    fi
    
    # Check systemctl (for service installation)
    if ! command -v systemctl &> /dev/null; then
        warn "systemctl not found. Service installation will be skipped."
    fi
    
    success "All dependencies are satisfied"
}

get_install_options() {
    echo
    log "Superposition MCP Server Installation"
    echo "====================================="
    echo
    
    # Installation directory
    read -p "Installation directory [$DEFAULT_INSTALL_DIR]: " INSTALL_DIR
    INSTALL_DIR=${INSTALL_DIR:-$DEFAULT_INSTALL_DIR}
    
    # User for service
    read -p "Service user [$DEFAULT_USER]: " SERVICE_USER
    SERVICE_USER=${SERVICE_USER:-$DEFAULT_USER}
    
    # Service name
    read -p "Service name [$DEFAULT_SERVICE_NAME]: " SERVICE_NAME
    SERVICE_NAME=${SERVICE_NAME:-$DEFAULT_SERVICE_NAME}
    
    # Installation type
    echo
    echo "Installation options:"
    echo "1) Development (local, debug enabled)"
    echo "2) Production (system service, optimized)"
    echo "3) Docker (container-ready)"
    read -p "Choose installation type [2]: " INSTALL_TYPE
    INSTALL_TYPE=${INSTALL_TYPE:-2}
    
    echo
    log "Installation configuration:"
    log "  Directory: $INSTALL_DIR"
    log "  User: $SERVICE_USER"
    log "  Service: $SERVICE_NAME"
    log "  Type: $INSTALL_TYPE"
    echo
    
    read -p "Continue with installation? (y/N): " CONFIRM
    if [[ ! $CONFIRM =~ ^[Yy]$ ]]; then
        log "Installation cancelled"
        exit 0
    fi
}

create_user() {
    if id "$SERVICE_USER" &>/dev/null; then
        log "User $SERVICE_USER already exists"
    else
        log "Creating user $SERVICE_USER..."
        sudo useradd --system --home-dir "$INSTALL_DIR" --shell /bin/false "$SERVICE_USER"
        success "User $SERVICE_USER created"
    fi
}

create_directories() {
    log "Creating directories..."
    
    sudo mkdir -p "$INSTALL_DIR"/{bin,config,logs,cache,data}
    sudo chown -R "$SERVICE_USER:$SERVICE_USER" "$INSTALL_DIR"
    
    # Set permissions
    sudo chmod 755 "$INSTALL_DIR"
    sudo chmod 750 "$INSTALL_DIR"/config
    sudo chmod 750 "$INSTALL_DIR"/logs
    sudo chmod 750 "$INSTALL_DIR"/cache
    sudo chmod 750 "$INSTALL_DIR"/data
    
    success "Directories created"
}

install_application() {
    log "Installing Superposition MCP Server..."
    
    # Create virtual environment
    sudo -u "$SERVICE_USER" python3 -m venv "$INSTALL_DIR/venv"
    
    # Upgrade pip
    sudo -u "$SERVICE_USER" "$INSTALL_DIR/venv/bin/pip" install --upgrade pip
    
    # Install the application
    case $INSTALL_TYPE in
        1) # Development
            sudo -u "$SERVICE_USER" "$INSTALL_DIR/venv/bin/pip" install -e ".[dev]"
            ;;
        2|3) # Production or Docker
            sudo -u "$SERVICE_USER" "$INSTALL_DIR/venv/bin/pip" install superposition-mcp
            ;;
    esac
    
    success "Application installed"
}

setup_configuration() {
    log "Setting up configuration..."
    
    local config_file="$INSTALL_DIR/config/.env"
    
    # Choose configuration template based on installation type
    case $INSTALL_TYPE in
        1) # Development
            local template="../examples/development-config.env"
            ;;
        2) # Production
            local template="../examples/production-config.env"
            ;;
        3) # Docker
            local template="../examples/basic-config.env"
            ;;
    esac
    
    # Copy configuration template
    if [[ -f "$template" ]]; then
        sudo cp "$template" "$config_file"
    else
        # Create basic configuration if template not found
        sudo tee "$config_file" > /dev/null <<EOF
# Superposition MCP Server Configuration
SUPERPOSITION_API_URL=https://your-superposition-instance.com
SUPERPOSITION_API_TOKEN=your-bearer-token
SUPERPOSITION_DEFAULT_WORKSPACE=dev
SUPERPOSITION_DEFAULT_ORG=juspay
SUPERPOSITION_TIMEOUT=30
SUPERPOSITION_DEBUG=false
SUPERPOSITION_LOG_LEVEL=INFO
EOF
    fi
    
    sudo chown "$SERVICE_USER:$SERVICE_USER" "$config_file"
    sudo chmod 600 "$config_file"
    
    success "Configuration template created at $config_file"
    warn "Please edit $config_file with your actual Superposition instance details"
}

install_service() {
    if [[ $INSTALL_TYPE -ne 2 ]] || ! command -v systemctl &> /dev/null; then
        log "Skipping service installation"
        return
    fi
    
    log "Installing systemd service..."
    
    local service_file="/etc/systemd/system/${SERVICE_NAME}.service"
    
    sudo tee "$service_file" > /dev/null <<EOF
[Unit]
Description=Superposition MCP Server
Documentation=https://docs.superposition.io/mcp-server
After=network.target network-online.target
Wants=network-online.target

[Service]
Type=simple
User=$SERVICE_USER
Group=$SERVICE_USER
WorkingDirectory=$INSTALL_DIR
ExecStart=$INSTALL_DIR/venv/bin/python -m superposition_mcp.main
ExecReload=/bin/kill -HUP \$MAINPID
Restart=always
RestartSec=5
TimeoutStopSec=30

# Environment
EnvironmentFile=$INSTALL_DIR/config/.env

# Security
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=$INSTALL_DIR/logs $INSTALL_DIR/cache $INSTALL_DIR/data

# Limits
LimitNOFILE=65536
LimitNPROC=4096

[Install]
WantedBy=multi-user.target
EOF
    
    sudo systemctl daemon-reload
    sudo systemctl enable "$SERVICE_NAME"
    
    success "Systemd service installed and enabled"
}

create_scripts() {
    log "Creating management scripts..."
    
    # Create start script
    sudo tee "$INSTALL_DIR/bin/start.sh" > /dev/null <<EOF
#!/bin/bash
cd "$INSTALL_DIR"
source config/.env
exec venv/bin/python -m superposition_mcp.main
EOF
    
    # Create stop script for development
    sudo tee "$INSTALL_DIR/bin/stop.sh" > /dev/null <<EOF
#!/bin/bash
pkill -f "superposition_mcp.main"
EOF
    
    # Create update script
    sudo tee "$INSTALL_DIR/bin/update.sh" > /dev/null <<EOF
#!/bin/bash
set -e
echo "Updating Superposition MCP Server..."
cd "$INSTALL_DIR"
source venv/bin/activate
pip install --upgrade superposition-mcp
echo "Update completed. Please restart the service."
EOF
    
    # Create backup script
    sudo tee "$INSTALL_DIR/bin/backup.sh" > /dev/null <<EOF
#!/bin/bash
BACKUP_DIR="/var/backups/superposition-mcp"
DATE=\$(date +%Y%m%d_%H%M%S)

mkdir -p "\$BACKUP_DIR"

# Backup configuration
cp "$INSTALL_DIR/config/.env" "\$BACKUP_DIR/config_\$DATE.env"

# Backup logs
tar -czf "\$BACKUP_DIR/logs_\$DATE.tar.gz" "$INSTALL_DIR/logs/"

echo "Backup completed: \$BACKUP_DIR"
EOF
    
    # Make scripts executable
    sudo chmod +x "$INSTALL_DIR/bin"/*.sh
    sudo chown -R "$SERVICE_USER:$SERVICE_USER" "$INSTALL_DIR/bin"
    
    success "Management scripts created"
}

setup_logrotate() {
    if [[ $INSTALL_TYPE -ne 2 ]]; then
        return
    fi
    
    log "Setting up log rotation..."
    
    sudo tee "/etc/logrotate.d/$SERVICE_NAME" > /dev/null <<EOF
$INSTALL_DIR/logs/*.log {
    daily
    rotate 30
    compress
    delaycompress
    missingok
    notifempty
    create 0644 $SERVICE_USER $SERVICE_USER
    postrotate
        systemctl reload $SERVICE_NAME || true
    endscript
}
EOF
    
    success "Log rotation configured"
}

install_claude_integration() {
    echo
    read -p "Would you like to install Claude Desktop integration? (y/N): " INSTALL_CLAUDE
    
    if [[ $INSTALL_CLAUDE =~ ^[Yy]$ ]]; then
        log "Creating Claude Desktop configuration..."
        
        local claude_config="$HOME/.config/claude/claude_desktop_config.json"
        local claude_dir="$(dirname "$claude_config")"
        
        mkdir -p "$claude_dir"
        
        if [[ -f "$claude_config" ]]; then
            # Backup existing config
            cp "$claude_config" "$claude_config.backup.$(date +%s)"
            log "Existing Claude config backed up"
        fi
        
        # Create or update config
        cat > "$claude_config" <<EOF
{
  "mcpServers": {
    "superposition": {
      "command": "python",
      "args": ["$INSTALL_DIR/venv/bin/python", "-m", "superposition_mcp.main"],
      "env": {
        "SUPERPOSITION_CONFIG_FILE": "$INSTALL_DIR/config/.env"
      }
    }
  }
}
EOF
        
        success "Claude Desktop configuration created at $claude_config"
    fi
}

print_completion_message() {
    echo
    echo "======================================"
    success "Installation completed successfully!"
    echo "======================================"
    echo
    log "Next steps:"
    echo
    echo "1. Edit configuration file:"
    echo "   sudo nano $INSTALL_DIR/config/.env"
    echo
    echo "2. Update with your Superposition instance details:"
    echo "   - SUPERPOSITION_API_URL"
    echo "   - SUPERPOSITION_API_TOKEN"
    echo "   - SUPERPOSITION_DEFAULT_WORKSPACE"
    echo
    
    case $INSTALL_TYPE in
        1) # Development
            echo "3. Start the server (development):"
            echo "   $INSTALL_DIR/bin/start.sh"
            ;;
        2) # Production
            echo "3. Start the service:"
            echo "   sudo systemctl start $SERVICE_NAME"
            echo
            echo "4. Check service status:"
            echo "   sudo systemctl status $SERVICE_NAME"
            echo
            echo "5. View logs:"
            echo "   sudo journalctl -u $SERVICE_NAME -f"
            ;;
        3) # Docker
            echo "3. Build Docker image (if not using pre-built):"
            echo "   docker build -t superposition-mcp ."
            echo
            echo "4. Run container:"
            echo "   docker run -d --env-file $INSTALL_DIR/config/.env superposition-mcp"
            ;;
    esac
    
    echo
    log "Management scripts available in $INSTALL_DIR/bin/:"
    echo "  - start.sh    : Start the server"
    echo "  - stop.sh     : Stop the server"
    echo "  - update.sh   : Update to latest version"
    echo "  - backup.sh   : Backup configuration and logs"
    echo
    log "Documentation: https://docs.superposition.io/mcp-server"
    log "Support: https://github.com/juspay/superposition/issues"
}

# Main installation flow
main() {
    log "Starting Superposition MCP Server installation..."
    
    check_root
    check_dependencies
    get_install_options
    
    if [[ $INSTALL_TYPE -eq 2 ]]; then
        create_user
    fi
    
    create_directories
    install_application
    setup_configuration
    
    if [[ $INSTALL_TYPE -eq 2 ]]; then
        install_service
        setup_logrotate
    fi
    
    create_scripts
    install_claude_integration
    print_completion_message
}

# Handle script interruption
trap 'error "Installation interrupted"' INT TERM

# Run main function
main "$@"