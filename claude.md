# Agda MCP Server

An MCP (Model Context Protocol) server that brings interactive Agda development to AI assistants like Claude. Enables AI-assisted proof development, type checking, and code exploration through a standardized protocol.

## Quick Start

```bash
# Enter dev environment
nix develop

# Build
cabal build

# Run server (starts on localhost:3000/mcp)
cabal run agda-mcp

# Run tests
cabal test
```

## Project Structure

```
src/AgdaMCP/
├── Main.hs           - HTTP server entry point
├── Types.hs          - Tool/Resource definitions and descriptions
├── Server.hs         - Core MCP handlers, REPL integration, file edit extraction
├── Repl.hs           - Persistent Agda REPL adapter
├── SessionManager.hs - Multi-agent session isolation & garbage collection
├── FileEdit.hs       - File edit strategies (ReplaceHole, ReplaceLine, BatchEdits)
└── Format.hs         - Response formatting (Concise/Full modes)

test/
├── Spec.hs                    - Test entry point
├── AgdaMCP/
│   ├── ServerSpec.hs          - Basic command tests
│   ├── MultiAgentSpec.hs      - Session isolation tests
│   ├── EditPersistenceSpec.hs - File edit verification
│   └── TestUtils.hs           - Test utilities
└── *.agda                     - Test Agda files
```

## Key Technologies

- **Haskell** (GHC 9.10) with Template Haskell for automatic MCP handler derivation
- **agda** (>= 2.8.0) - Agda interactive development library
- **mcp-server** (>= 0.1.0.15) - MCP protocol implementation
- **Nix/flakes** - Reproducible builds
- **HTTP/JSON-RPC 2.0** - MCP transport

## Important Patterns

### File Edit Strategies (FileEdit.hs)

Three edit types matching Agda semantics:
1. **ReplaceHole** (Give, Refine, Auto) - In-place hole substitution
2. **ReplaceLine** (Case Split) - Structural line replacement for pattern matches
3. **BatchEdits** (SolveAll) - Multiple edits applied bottom-to-top to preserve positions

### Session Isolation (SessionManager.hs)

- `sessionId` parameter enables multi-agent isolation
- `sessionId = Nothing` uses shared default session (backward compatible)
- Each session has own REPL instance and file state

### Response Formatting (Format.hs)

- **Concise** (default): Human-readable, ~90% smaller
- **Full**: Complete JSON with ranges for programmatic use

### Persistent REPL (Server.hs + Repl.hs)

Channel-based communication maintains state across tool calls:
```
MCP Request → SessionManager → Chan CommandWithResponse → REPL → Response → File Edit → JSON
```

## Adding New Tools

1. Add constructor to `AgdaTool` in `Types.hs`
2. Add description to `agdaToolDescriptions` in `Types.hs`
3. Implement handler in `handleAgdaTool` in `Server.hs`
4. Apply `FileEdit` if the tool modifies files
5. Template Haskell auto-generates MCP schema

## Adding New Resources

1. Add simple constructor to `AgdaResource` in `Types.hs`
2. Add description to `agdaResourceDescriptions`
3. Implement handler in `handleAgdaResource` in `Server.hs`

## MCP Tools Available

**File & Session**: `agda_load`, `agda_list_postulates`

**Goal Inspection**: `agda_get_goals`, `agda_get_goal_type`, `agda_get_goal_type_implicits`, `agda_get_context`, `agda_get_context_implicits`, `agda_goal_type_context`

**Proof Development**: `agda_give`, `agda_refine`, `agda_case_split`, `agda_intro`, `agda_auto`, `agda_auto_all`, `agda_solve_one`

**Type Exploration**: `agda_compute`, `agda_infer_type`, `agda_why_in_scope`, `agda_show_module`, `agda_show_constraints`, `agda_search_about`

**Code Navigation**: `agda_goal_at_position`, `agda_goto_definition`, `agda_helper_function`

## Testing

Tests use temporary copies of Agda files to avoid corruption. Key test utilities:
- `withTempTestFile` - Creates temp file copy for safe testing
- Semantic session IDs enable parallel test execution
- `TestUtils.hs` contains shared assertions and helpers

## Configuration

Claude Code MCP configuration:
```json
{
  "mcpServers": {
    "agda-mcp": {
      "transport": "http",
      "url": "http://localhost:3000/mcp"
    }
  }
}
```

## Notes

- The `patches/` directory contains an MCP header compatibility patch (auto-applied by Nix)
- Library resolution uses project root for `agda-mcp.agda-lib`
- Default session timeout: 10 minutes with automatic garbage collection
