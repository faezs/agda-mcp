# Agda MCP Server

A [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) server that provides interactive Agda development capabilities to AI assistants like Claude Code. This enables AI-assisted proof development, interactive theorem proving, and exploration of Agda code through a standardized protocol.

## Features

- **Persistent REPL Session**: Maintains Agda interaction state across commands, preserving goals, context, and type-checking results
- **11 Interactive Commands**: Full coverage of essential Agda interaction operations
- **HTTP Transport**: Standards-compliant MCP server with HTTP/JSON-RPC transport
- **Backward Compatibility Patch**: Includes patch for mcp-server to work with Claude Code's HTTP transport
- **Type-Safe Integration**: Built with Haskell's type system for robust MCP protocol handling

## Architecture

The server runs a persistent Agda REPL in a background thread, communicating via channels:
- Commands are sent through a `Chan CommandWithResponse`
- Responses are routed back via `MVar Response`
- Each MCP tool call is converted to an `IOTCM` command and executed in the persistent REPL
- Goals, context, and interaction state persist between commands

This architecture ensures that operations like "load file" → "get goals" → "refine goal" work correctly with maintained state.

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- [Agda](https://agda.readthedocs.io/) (automatically provided by Nix environment)
- [Claude Code](https://docs.claude.com/en/docs/claude-code) or another MCP client

## Installation

1. Clone the repository:
```bash
git clone https://github.com/faezs/agda-mcp.git
cd agda-mcp
```

2. Enter the Nix development shell:
```bash
nix develop
```

3. Build the server:
```bash
cabal build
```

4. Run the server:
```bash
cabal run agda-mcp
```

The server starts on `http://localhost:3000/mcp` by default.

## Configuration

### Claude Code

Add to your Claude Code MCP configuration file (`~/.config/claude-code/mcp.json`):

```json
{
  "mcpServers": {
    "agda-mcp": {
      "transport": "http",
      "url": "http://localhost:3000/mcp",
      "command": "cabal",
      "args": ["run", "agda-mcp"],
      "cwd": "/path/to/agda-mcp"
    }
  }
}
```

### Other MCP Clients

The server implements MCP protocol version 2025-06-18 with HTTP transport. Configure your client to:
- Connect to: `http://localhost:3000/mcp`
- Use JSON-RPC 2.0 over HTTP POST
- Include `Content-Type: application/json` header
- Optionally include `MCP-Protocol-Version: 2025-06-18` header

## Available Tools

All tools follow the MCP naming convention (snake_case). Arguments are provided as JSON objects.

### 1. `agda_load`

Load and type-check an Agda file.

**Arguments:**
- `file` (string): Absolute or relative path to the Agda file

**Example:**
```json
{
  "name": "agda_load",
  "arguments": {
    "file": "/path/to/Example.agda"
  }
}
```

**Use Case:** Must be called first before any other operations. Initializes the Agda interaction session with the specified file.

---

### 2. `agda_get_goals`

List all goals/holes in the currently loaded file.

**Arguments:** None

**Example:**
```json
{
  "name": "agda_get_goals",
  "arguments": {}
}
```

**Returns:** List of goals with their IDs, types, and locations in the file.

**Use Case:** After loading a file, use this to see what needs to be proven or filled in.

---

### 3. `agda_get_goal_type`

Get the type expected at a specific goal.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal (starting from 0)

**Example:**
```json
{
  "name": "agda_get_goal_type",
  "arguments": {
    "goalId": 0
  }
}
```

**Returns:** The expected type that should fill this goal.

**Use Case:** Understand what type of expression is needed to complete a proof.

---

### 4. `agda_get_context`

Get the context (available variables and their types) at a specific goal.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal

**Example:**
```json
{
  "name": "agda_get_context",
  "arguments": {
    "goalId": 0
  }
}
```

**Returns:** List of variables in scope at this goal with their types.

**Use Case:** See what variables and hypotheses are available to use in a proof.

---

### 5. `agda_give`

Fill a goal/hole with a complete expression.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal to fill
- `expression` (string): The Agda expression to use (e.g., "zero", "suc n", "refl")

**Example:**
```json
{
  "name": "agda_give",
  "arguments": {
    "goalId": 0,
    "expression": "refl"
  }
}
```

**Use Case:** Complete a goal when you have the full solution.

---

### 6. `agda_refine`

Refine a goal with a constructor or function, introducing new sub-goals.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal to refine
- `expression` (string): Constructor or function name (e.g., "suc", "zero", "_+_")

**Example:**
```json
{
  "name": "agda_refine",
  "arguments": {
    "goalId": 0,
    "expression": "suc"
  }
}
```

**Use Case:** Make progress on a goal by applying a constructor, which creates new sub-goals for the constructor's arguments.

---

### 7. `agda_case_split`

Split a goal by pattern matching on a variable.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal
- `variable` (string): Name of the variable to pattern-match on

**Example:**
```json
{
  "name": "agda_case_split",
  "arguments": {
    "goalId": 0,
    "variable": "n"
  }
}
```

**Use Case:** Perform case analysis on a variable (e.g., split a natural number into zero and successor cases).

---

### 8. `agda_compute`

Normalize and display an expression in a goal's context.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal (for context)
- `expression` (string): Expression to normalize

**Example:**
```json
{
  "name": "agda_compute",
  "arguments": {
    "goalId": 0,
    "expression": "2 + 2"
  }
}
```

**Returns:** The normalized form of the expression.

**Use Case:** Evaluate expressions to see their simplified form.

---

### 9. `agda_infer_type`

Infer the type of an expression in a goal's context.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal (for context)
- `expression` (string): Expression to type-check

**Example:**
```json
{
  "name": "agda_infer_type",
  "arguments": {
    "goalId": 0,
    "expression": "suc zero"
  }
}
```

**Returns:** The inferred type of the expression.

**Use Case:** Check what type an expression has before using it.

---

### 10. `agda_intro`

Introduce variables using the intro tactic.

**Arguments:**
- `goalId` (integer): The numeric ID of the goal

**Example:**
```json
{
  "name": "agda_intro",
  "arguments": {
    "goalId": 0
  }
}
```

**Use Case:** Automatically introduce lambda-bound variables or pattern variables suggested by Agda.

---

### 11. `agda_why_in_scope`

Look up documentation and scope information for a name.

**Arguments:**
- `name` (string): The name to look up

**Example:**
```json
{
  "name": "agda_why_in_scope",
  "arguments": {
    "name": "suc"
  }
}
```

**Returns:** Information about where the name is defined and why it's in scope.

**Use Case:** Understand the origin and definition of functions, types, or constructors.

---

## Available Resources

Resources expose Agda information as readable data sources. They use URI-based routing.

### 1. `Goals`

**URI Pattern:** `resource://goals/{file}`

List all goals in the specified Agda file.

### 2. `GoalInfo`

**URI Pattern:** `resource://goal_info/{file}/{id}`

Detailed information about a specific goal, including its type, context, and location.

### 3. `FileContext`

**URI Pattern:** `resource://file_context/{file}`

Overall context and scope information for an Agda file.

## Example Workflow

```bash
# 1. Start the server
cabal run agda-mcp

# 2. In Claude Code, load an Agda file
/mcp agda-mcp agda_load file="/path/to/Example.agda"

# 3. Get all goals in the file
/mcp agda-mcp agda_get_goals

# 4. Check the type of goal 0
/mcp agda-mcp agda_get_goal_type goalId=0

# 5. See available variables
/mcp agda-mcp agda_get_context goalId=0

# 6. Refine the goal with a constructor
/mcp agda-mcp agda_refine goalId=0 expression="suc"

# 7. Fill a sub-goal
/mcp agda-mcp agda_give goalId=1 expression="zero"
```

## Development

### Project Structure

```
agda-mcp/
├── src/
│   ├── AgdaMCP/
│   │   ├── Types.hs        # MCP tool and resource definitions
│   │   ├── Server.hs       # MCP handlers and REPL integration
│   │   └── Repl.hs         # Persistent REPL adapter
│   └── Main.hs             # Entry point
├── patches/
│   └── mcp-server-header-optional.patch  # Compatibility patch
├── test/
│   └── Example.agda        # Sample Agda file for testing
├── agda-mcp.cabal          # Cabal package definition
├── flake.nix               # Nix flake for reproducible builds
└── README.md               # This file
```

### Building from Source

```bash
# Enter development environment
nix develop

# Build
cabal build

# Run tests (TODO)
cabal test

# Install locally
cabal install
```

### Running in Development Mode

```bash
# Rebuild and run
cabal run agda-mcp

# With verbose output (TODO: add verbose flag)
cabal run agda-mcp -- --verbose
```

## Troubleshooting

### Server Won't Start

**Issue:** Port 3000 already in use
```
bind: address already in use (Address already in use)
```

**Solution:** Kill existing process or change port (currently hardcoded to 3000).

### Connection Refused from Claude Code

**Issue:** `Failed to reconnect to agda-mcp`

**Possible Causes:**
1. Server not running: Start with `cabal run agda-mcp`
2. Wrong configuration: Verify MCP config file
3. Network issues: Check that localhost:3000 is accessible

### Goals Not Persisting

**Issue:** Goals disappear after loading file

**Solution:** This should not happen with the persistent REPL architecture. If it does:
1. Check server logs for errors
2. Verify the file loaded successfully with `agda_load`
3. Check that the REPL thread is running (look for "Starting persistent Agda REPL thread..." in logs)

### Type Checking Errors

**Issue:** Agda reports type errors or missing modules

**Solution:**
1. Ensure all dependencies are installed
2. Check that your Agda library path is correctly configured
3. Load the file first with `agda_load` before other operations

## mcp-server Patch

This project includes a patch to the `mcp-server` Haskell library (version 0.1.0.15) to improve compatibility with MCP clients that don't send the `MCP-Protocol-Version` HTTP header.

**What the patch does:**
- Makes the `MCP-Protocol-Version: 2025-06-18` HTTP header optional
- Falls back to protocol version negotiation during the `initialize` handshake
- Logs warnings when header is missing but continues processing

**Why it's needed:**
Claude Code's HTTP transport (as of version 2.0.13) sends the protocol version in the `initialize` JSON-RPC message but not as an HTTP header. The mcp-server 0.1.0.15 strictly requires the header per MCP spec 2025-06-18, causing connection failures.

**Patch location:** `patches/mcp-server-header-optional.patch`

This patch is applied automatically during the Nix build process via `pkgs.applyPatches` in `flake.nix`.

## Contributing

Contributions are welcome! Areas for improvement:

- [ ] Add comprehensive test suite
- [ ] Support for stdio transport in addition to HTTP
- [ ] Configurable port and host
- [ ] Better error messages and response parsing
- [ ] Support for more Agda interaction commands
- [ ] Resource implementations (currently stubs)
- [ ] Verbose/debug logging modes
- [ ] CI/CD pipeline

## License

BSD-3-Clause (same as Agda)

## Acknowledgments

- Built with [mcp-server](https://github.com/drshade/haskell-mcp-server) Haskell library
- Powered by [Agda](https://agda.readthedocs.io/)
- Inspired by [agda-mode](https://github.com/banacorn/agda-mode-vscode) for VSCode

---

**Note:** This is an experimental project. The MCP protocol and Claude Code's implementation are evolving, and this server may require updates for compatibility with future versions.
