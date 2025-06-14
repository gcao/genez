import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    console.log('Gene Language extension is now active!');

    // Get the language server path from configuration
    const config = vscode.workspace.getConfiguration('gene');
    let serverPath = config.get<string>('languageServer.path');

    // If not configured, try to find it relative to the workspace
    if (!serverPath || serverPath.length === 0) {
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (workspaceFolders && workspaceFolders.length > 0) {
            // Try common locations
            const possiblePaths = [
                path.join(workspaceFolders[0].uri.fsPath, 'tools/gene-lang-server/zig-out/bin/gene-lang-server'),
                path.join(workspaceFolders[0].uri.fsPath, 'zig-out/bin/gene-lang-server'),
                path.join(workspaceFolders[0].uri.fsPath, 'gene-lang-server'),
            ];

            for (const p of possiblePaths) {
                if (fs.existsSync(p)) {
                    serverPath = p;
                    break;
                }
            }
        }
    }

    if (!serverPath || !fs.existsSync(serverPath)) {
        vscode.window.showErrorMessage(
            'Gene language server not found. Please configure "gene.languageServer.path" in settings.'
        );
        return;
    }

    // Server options
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            args: ['--stdio'],
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            args: ['--stdio', '--debug'],
            transport: TransportKind.stdio
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        // Register the server for Gene documents
        documentSelector: [
            { scheme: 'file', language: 'gene' },
            { scheme: 'untitled', language: 'gene' }
        ],
        synchronize: {
            // Notify the server about file changes to '.gene' files contained in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.gene')
        },
        outputChannelName: 'Gene Language Server',
        traceOutputChannelName: 'Gene Language Server Trace'
    };

    // Create the language client and start the client
    client = new LanguageClient(
        'geneLangServer',
        'Gene Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client
    client.start();

    // Register additional commands
    context.subscriptions.push(
        vscode.commands.registerCommand('gene.restartServer', () => {
            vscode.window.showInformationMessage('Restarting Gene Language Server...');
            client.restart();
        })
    );

    // Status bar item
    const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    statusBarItem.text = '$(symbol-misc) Gene';
    statusBarItem.tooltip = 'Gene Language Server is running';
    statusBarItem.command = 'gene.restartServer';
    context.subscriptions.push(statusBarItem);

    // Show status bar item for Gene files
    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor(editor => {
            if (editor && editor.document.languageId === 'gene') {
                statusBarItem.show();
            } else {
                statusBarItem.hide();
            }
        })
    );

    // Check current editor
    if (vscode.window.activeTextEditor?.document.languageId === 'gene') {
        statusBarItem.show();
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}