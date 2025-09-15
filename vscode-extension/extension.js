const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');
const fs = require('fs');
const path = require('path');

let client;

function activate(context) {
  const serverModule = context.asAbsolutePath('minimu-lsp.js');

  // Check if the LSP server file exists
  if (!fs.existsSync(serverModule)) {
    vscode.window.showErrorMessage(`MiniMu Language Server not found: ${serverModule}`);
    return;
  }

  const serverOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: { module: serverModule, transport: TransportKind.ipc }
  };

  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'minimu' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.mmu')
    }
  };

  client = new LanguageClient(
    'minimuLanguageServer',
    'MiniMu Language Server',
    serverOptions,
    clientOptions
  );

  // Handle server state changes
  client.onDidChangeState((event) => {
    if (event.newState === 2) { // Running state
      console.log('MiniMu Language Server is ready');
    } else if (event.newState === 4) { // Stopped state
      console.log('MiniMu Language Server stopped');
    } else if (event.newState === 3) { // StartFailed state
      console.error('MiniMu Language Server failed to start');
      vscode.window.showErrorMessage('Failed to start MiniMu Language Server');
    }
  });

  try {
    // Add more detailed error handling
    client.onReady().then(() => {
      console.log('MiniMu Language Server is ready and connected');
      vscode.window.showInformationMessage('MiniMu Language Server started successfully');
    }).catch((error) => {
      console.error('Language Server ready promise failed:', error);
      vscode.window.showErrorMessage(`Language Server failed to become ready: ${error.message}`);
    });

    // Start the client and handle the promise
    client.start().then(() => {
      console.log('MiniMu Language Server start() completed');
    }).catch((error) => {
      console.error('Failed to start MiniMu Language Server:', error);
      vscode.window.showErrorMessage(`Failed to start MiniMu Language Server: ${error.message}`);
    });
  } catch (error) {
    console.error('Error starting MiniMu Language Server:', error);
    vscode.window.showErrorMessage(`Error starting MiniMu Language Server: ${error.message}`);
  }
}

function deactivate() {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

module.exports = {
  activate,
  deactivate
};
