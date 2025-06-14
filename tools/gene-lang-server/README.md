# Gene Language Server

A Language Server Protocol (LSP) implementation for the Gene programming language.

## Features

- Syntax highlighting (via semantic tokens)
- Document symbols (functions, variables, classes)
- Diagnostics (parse errors)
- Auto-completion (basic keywords and snippets)
- Hover information (planned)
- Go to definition (planned)
- Find references (planned)

## Building

From this directory:

```bash
zig build
```

This will create the `gene-lang-server` executable in `zig-out/bin/`.

## Usage

The language server communicates via stdio:

```bash
gene-lang-server --stdio
```

## VS Code Extension

To use this language server with VS Code, you'll need to create an extension. Here's a basic setup:

### 1. Create Extension Directory

```bash
mkdir -p ~/.vscode/extensions/gene-lang
cd ~/.vscode/extensions/gene-lang
```

### 2. Create package.json

```json
{
  "name": "gene-lang",
  "displayName": "Gene Language",
  "description": "Language support for Gene",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.74.0"
  },
  "categories": ["Programming Languages"],
  "activationEvents": [
    "onLanguage:gene"
  ],
  "main": "./out/extension.js",
  "contributes": {
    "languages": [{
      "id": "gene",
      "aliases": ["Gene", "gene"],
      "extensions": [".gene"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "gene",
      "scopeName": "source.gene",
      "path": "./syntaxes/gene.tmLanguage.json"
    }],
    "configuration": {
      "type": "object",
      "title": "Gene",
      "properties": {
        "gene.languageServer.path": {
          "type": "string",
          "default": "",
          "description": "Path to the gene-lang-server executable"
        }
      }
    }
  }
}
```

### 3. Create language-configuration.json

```json
{
  "comments": {
    "lineComment": "#",
    "blockComment": ["#<", ">#"]
  },
  "brackets": [
    ["(", ")"],
    ["[", "]"],
    ["{", "}"]
  ],
  "autoClosingPairs": [
    ["(", ")"],
    ["[", "]"],
    ["{", "}"],
    ["\"", "\""]
  ],
  "surroundingPairs": [
    ["(", ")"],
    ["[", "]"],
    ["{", "}"],
    ["\"", "\""]
  ]
}
```

### 4. Create Basic TextMate Grammar

Create `syntaxes/gene.tmLanguage.json` for basic syntax highlighting:

```json
{
  "name": "Gene",
  "patterns": [
    {
      "include": "#comments"
    },
    {
      "include": "#keywords"
    },
    {
      "include": "#strings"
    },
    {
      "include": "#numbers"
    },
    {
      "include": "#symbols"
    }
  ],
  "repository": {
    "comments": {
      "patterns": [
        {
          "name": "comment.line.gene",
          "match": "#.*$"
        },
        {
          "name": "comment.block.gene",
          "begin": "#<",
          "end": ">#"
        }
      ]
    },
    "keywords": {
      "patterns": [
        {
          "name": "keyword.control.gene",
          "match": "\\b(fn|var|if|else|do|class|new|match|macro|ns)\\b"
        }
      ]
    },
    "strings": {
      "patterns": [
        {
          "name": "string.quoted.double.gene",
          "begin": "\"",
          "end": "\"",
          "patterns": [
            {
              "name": "constant.character.escape.gene",
              "match": "\\\\."
            }
          ]
        }
      ]
    },
    "numbers": {
      "patterns": [
        {
          "name": "constant.numeric.gene",
          "match": "\\b\\d+(\\.\\d+)?\\b"
        }
      ]
    },
    "symbols": {
      "patterns": [
        {
          "name": "constant.language.gene",
          "match": "\\b(true|false|nil)\\b"
        }
      ]
    }
  },
  "scopeName": "source.gene"
}
```

### 5. Create Extension Code

Create `src/extension.ts`:

```typescript
import * as vscode from 'vscode';
import * as path from 'path';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('gene');
    let serverPath = config.get<string>('languageServer.path');
    
    if (!serverPath) {
        // Try to find it relative to the workspace
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (workspaceFolders) {
            serverPath = path.join(
                workspaceFolders[0].uri.fsPath,
                'tools/gene-lang-server/zig-out/bin/gene-lang-server'
            );
        }
    }

    if (!serverPath) {
        vscode.window.showErrorMessage('Gene language server path not configured');
        return;
    }

    const serverOptions: ServerOptions = {
        command: serverPath,
        args: ['--stdio']
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'gene' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.gene')
        }
    };

    client = new LanguageClient(
        'geneLangServer',
        'Gene Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
```

### 6. Install Dependencies and Build

```bash
npm install
npm install vscode-languageclient
npm run compile
```

## Architecture

The language server is structured as follows:

- `main.zig` - Entry point, handles command line arguments
- `server.zig` - Main server logic, handles LSP messages
- `lsp.zig` - LSP protocol types and constants
- `document.zig` - Document management and parsing
- `analysis.zig` - Code analysis functions (diagnostics, symbols, etc.)

## TODO

- [ ] Improve position tracking in AST nodes
- [ ] Implement hover information
- [ ] Implement go to definition
- [ ] Implement find references
- [ ] Add more sophisticated completions
- [ ] Implement formatting
- [ ] Add configuration options
- [ ] Improve error recovery in parser