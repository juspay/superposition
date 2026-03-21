import * as path from 'path';
import * as fs from 'fs';
import { ExtensionContext, workspace, window, commands, OutputChannel, Uri } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Trace,
    State,
    RevealOutputChannelOn
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: OutputChannel;

export function activate(context: ExtensionContext): void {
    outputChannel = window.createOutputChannel('SuperTOML Analyzer');
    outputChannel.appendLine('SuperTOML Analyzer extension is activating...');

    // Register the restart command
    const restartCommand = commands.registerCommand('superTOML.restartServer', async () => {
        if (client) {
            outputChannel.appendLine('Restarting SuperTOML Language Server...');
            await client.stop();
            client = undefined;
            startLanguageClient(context);
            window.showInformationMessage('SuperTOML Language Server restarted.');
        }
    });
    context.subscriptions.push(restartCommand);

    startLanguageClient(context);
}

function startLanguageClient(context: ExtensionContext): void {
    const serverOptions: ServerOptions = {
        command: getServerPath(context),
        args: [],
        options: {
            env: process.env
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'superTOML' },
            { scheme: 'untitled', language: 'superTOML' }
        ],
        synchronize: {
            configurationSection: 'superTOML'
        },
        outputChannel: outputChannel,
        traceOutputChannel: outputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Error,
        initializationOptions: getInitializationOptions(),
        middleware: {
            didOpen: (document, next) => {
                outputChannel.appendLine(`Opened document: ${document.uri.fsPath}`);
                return next(document);
            },
            didSave: (document, next) => {
                outputChannel.appendLine(`Saved document: ${document.uri.fsPath}`);
                return next(document);
            }
        }
    };

    client = new LanguageClient(
        'superTOML',
        'SuperTOML Analyzer',
        serverOptions,
        clientOptions
    );

    // Set trace level based on configuration
    const traceConfig = workspace.getConfiguration('superTOML').get<string>('trace.server', 'off');
    const traceLevel = traceConfig === 'verbose' ? Trace.Verbose :
        traceConfig === 'messages' ? Trace.Messages : Trace.Off;

    client.setTrace(traceLevel);

    // Listen for configuration changes
    context.subscriptions.push(
        workspace.onDidChangeConfiguration(event => {
            if (event.affectsConfiguration('superTOML.trace.server')) {
                const newTraceConfig = workspace.getConfiguration('superTOML').get<string>('trace.server', 'off');
                const newTraceLevel = newTraceConfig === 'verbose' ? Trace.Verbose :
                    newTraceConfig === 'messages' ? Trace.Messages : Trace.Off;
                client?.setTrace(newTraceLevel);
            }
            if (event.affectsConfiguration('superTOML.server.path')) {
                window.showInformationMessage(
                    'SuperTOML server path changed. Restart the server for changes to take effect.',
                    'Restart'
                ).then(selection => {
                    if (selection === 'Restart') {
                        commands.executeCommand('superTOML.restartServer');
                    }
                });
            }
        })
    );

    client.onDidChangeState(event => {
        if (event.newState === State.Running) {
            outputChannel.appendLine('SuperTOML Language Server is running.');
        } else if (event.newState === State.Stopped) {
            outputChannel.appendLine('SuperTOML Language Server has stopped.');
        }
    });

    client.start().catch(error => {
        outputChannel.appendLine(`Failed to start SuperTOML Language Server: ${error}`);
        window.showErrorMessage(
            `Failed to start SuperTOML Language Server. Check the output channel for details. Error: ${error}`
        );
    });

    context.subscriptions.push(client);
}

function getServerPath(context: ExtensionContext): string {
    const config = workspace.getConfiguration('superTOML');
    const configuredPath = config.get<string>('server.path', '');

    // If a path is explicitly configured, use it
    if (configuredPath && configuredPath.trim() !== '') {
        outputChannel.appendLine(`Using configured server path: ${configuredPath}`);
        return configuredPath;
    }

    // Try to find the binary in common locations
    const possiblePaths: string[] = [];

    // Check workspace folders
    if (workspace.workspaceFolders) {
        for (const folder of workspace.workspaceFolders) {
            const folderPath = folder.uri.fsPath;
            possiblePaths.push(
                path.join(folderPath, 'target', 'debug', 'supertoml-analyzer'),
                path.join(folderPath, 'target', 'release', 'supertoml-analyzer'),
                // Also check parent directory in case workspace is a subdirectory
                path.join(folderPath, '..', 'target', 'debug', 'supertoml-analyzer'),
                path.join(folderPath, '..', 'target', 'release', 'supertoml-analyzer')
            );
        }
    }

    // Check relative to extension path (development mode)
    const extensionRoot = path.dirname(path.dirname(context.extensionPath));
    possiblePaths.push(
        path.join(extensionRoot, 'target', 'debug', 'supertoml-analyzer'),
        path.join(extensionRoot, 'target', 'release', 'supertoml-analyzer')
    );

    // Try each path
    for (const p of possiblePaths) {
        const resolvedPath = path.resolve(p);
        if (fs.existsSync(resolvedPath)) {
            outputChannel.appendLine(`Found server at: ${resolvedPath}`);
            return resolvedPath;
        }
    }

    // Fall back to assuming it's in PATH
    const binaryName = process.platform === 'win32' ? 'supertoml-analyzer.exe' : 'supertoml-analyzer';
    outputChannel.appendLine(`Using binary from PATH: ${binaryName}`);
    return binaryName;
}

function getInitializationOptions(): object {
    const config = workspace.getConfiguration('superTOML');
    return {
        diagnostics: {
            enable: config.get<boolean>('diagnostics.enable', true)
        },
        completions: {
            enable: config.get<boolean>('completions.enable', true)
        },
        hover: {
            enable: config.get<boolean>('hover.enable', true)
        },
        checkOnSave: {
            enable: true
        }
    };
}

export async function deactivate(): Promise<void> {
    outputChannel.appendLine('SuperTOML Analyzer extension is deactivating...');
    if (client) {
        await client.stop();
    }
}
