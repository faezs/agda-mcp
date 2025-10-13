#!/bin/bash

# Test script for Agda MCP Server

echo "=== Starting Agda MCP Server Tests ==="
echo ""

# Start the server in the background
cd /Users/faezs/agda-mcp
cabal run agda-mcp > /tmp/agda-mcp-output.log 2>&1 &
SERVER_PID=$!

echo "Server started with PID: $SERVER_PID"
sleep 3  # Give server time to start

# Function to send command and capture response
send_command() {
    local test_name=$1
    local json_cmd=$2

    echo "==================================="
    echo "Test: $test_name"
    echo "Command: $json_cmd"
    echo "-----------------------------------"

    # Send command via named pipe or direct communication
    echo "$json_cmd" | cabal run agda-mcp 2>&1 | head -20

    echo ""
}

# Test 1: Load file
send_command "Load File" '{"command": "load", "file": "/Users/faezs/agda-mcp/test/Example.agda"}'

# Test 2: Get goals
send_command "Get Goals" '{"command": "get_goals"}'

# Test 3: Get goal type for goal 0
send_command "Get Goal Type" '{"command": "get_goal_type", "goal_id": 0}'

# Test 4: Get context for goal 0
send_command "Get Context" '{"command": "get_context", "goal_id": 0}'

# Test 5: Try intro on goal 0
send_command "Intro on Goal 0" '{"command": "intro", "goal_id": 0}'

# Cleanup
echo "==================================="
echo "Killing server (PID: $SERVER_PID)"
kill $SERVER_PID 2>/dev/null

echo ""
echo "=== Test Complete ==="
