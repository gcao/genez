# Gene Language Extension for VS Code

This extension provides language support for the Gene programming language in Visual Studio Code.

## Features

- **Syntax Highlighting**: Full syntax highlighting for Gene code
- **Language Server Integration**: 
  - Parse error diagnostics
  - Document symbols (functions, variables, classes)
  - Basic auto-completion
  - Semantic tokens for enhanced highlighting
- **Code Navigation**: 
  - Bracket matching
  - Code folding
  - Auto-closing brackets

## Requirements

- The Gene language server must be built and available
- VS Code 1.74.0 or higher

## Installation

### From Source

1. Build the Gene language server:
   ```bash
   cd tools/gene-lang-server
   zig build
   ```

2. Install extension dependencies:
   ```bash
   cd tools/vscode-gene-extension
   npm install
   ```

3. Compile the extension:
   ```bash
   npm run compile
   ```

4. Copy to VS Code extensions folder:
   ```bash
   cp -r . ~/.vscode/extensions/gene-lang
   ```

5. Restart VS Code

## Configuration

The extension provides the following settings:

- `gene.languageServer.path`: Path to the gene-lang-server executable
- `gene.languageServer.trace.server`: Enable language server trace logging

## Usage

1. Open a `.gene` file in VS Code
2. The extension will automatically activate
3. You should see syntax highlighting and error diagnostics
4. Use Ctrl+Space for auto-completion
5. Use Ctrl+Shift+O to see document symbols

## Language Features

### Syntax Highlighting

The extension highlights:
- Keywords (fn, var, if, class, etc.)
- Literals (numbers, strings, booleans)
- Comments (line and block)
- Operators
- Properties (^property syntax)
- Special forms

### Auto-completion

Basic snippets are provided for common constructs:
- `fn` - Function definition
- `var` - Variable declaration
- `if` - If expression
- `match` - Pattern matching
- `class` - Class definition

## Development

To work on the extension:

1. Open the extension folder in VS Code
2. Run `npm run watch` to compile TypeScript in watch mode
3. Press F5 to launch a new VS Code window with the extension loaded
4. Make changes and reload the window to test

## Known Issues

- Position tracking in the language server needs improvement
- Hover information not yet implemented
- Go to definition not yet implemented

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.