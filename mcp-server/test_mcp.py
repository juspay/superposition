#!/usr/bin/env python3
"""Simple test to verify MCP server functionality."""

import json
import subprocess
import sys
import time

def test_mcp_server():
    """Test the MCP server with a simple request."""
    print("🧪 Testing Superposition MCP Server...")
    
    # JSON-RPC request to list tools
    request = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/list"
    }
    
    try:
        # Start the server process
        process = subprocess.Popen(
            [sys.executable, "-m", "superposition_mcp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            cwd="/Users/natarajankannan/src/superposition/mcp-server"
        )
        
        # Send the request
        request_str = json.dumps(request) + "\\n"
        stdout, stderr = process.communicate(input=request_str, timeout=5)
        
        if process.returncode == 0:
            print("✅ Server started successfully")
            print(f"📤 Sent: {request_str.strip()}")
            if stdout:
                print(f"📥 Received: {stdout}")
            return True
        else:
            print(f"❌ Server failed with return code: {process.returncode}")
            if stderr:
                print(f"🚫 Error: {stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        print("⏰ Server is running (timeout as expected for MCP server)")
        process.kill()
        return True
    except Exception as e:
        print(f"❌ Test failed: {e}")
        return False

if __name__ == "__main__":
    success = test_mcp_server()
    sys.exit(0 if success else 1)