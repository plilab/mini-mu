#!/usr/bin/env node

const { createConnection, TextDocuments, ProposedFeatures, TextDocumentSyncKind, SemanticTokensBuilder, SemanticTokensLegend } = require('vscode-languageserver/node');
const { TextDocument } = require('vscode-languageserver-textdocument');

// Create a connection for the server using the default transport (handles IPC from VS Code)
let connection;
try {
  connection = createConnection(ProposedFeatures.all);
  
  if (!connection) {
    console.error('Failed to create connection: connection is null');
    process.exit(1);
  }

  // Add error handling
  connection.onError((error) => {
    console.error(`Language server error: ${error}`);
  });

  connection.onClose(() => {
    console.log('Language server connection closed');
  });

  // Create a simple text document manager
  const documents = new TextDocuments(TextDocument);

  // MiniMu language tokens
  const tokenTypes = [
    'keyword',
    'string', 
    'number',
    'variable',
    'function',
    'operator',
    'comment',
    'type'
  ];

  const tokenModifiers = [
    'definition',
    'readonly'
  ];

  const legend = new SemanticTokensLegend(tokenTypes, tokenModifiers);

  connection.onInitialize((_params) => {
  console.log('Language server initializing...');
  const result = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      semanticTokensProvider: {
        legend: legend,
        full: true
      }
    }
  };
  console.log('Language server initialized successfully');
  return result;
});

connection.onInitialized(() => {
  console.log('Language server is ready to serve');
});

connection.onSemanticTokens((params) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return { data: [] };
  }

  const text = document.getText();
  const builder = new SemanticTokensBuilder(legend);

  tokenizeMiniMu(text, builder);

  return builder.build();
});



function tokenizeMiniMu(text, builder) {
  const lines = text.split('\n');

  // MiniMu keywords
  const keywords = new Set(['fn', 'run', 'let', 'have', 'letc', 'in', 'where', 'do', 'then', 'match', 'patch', 'with', 'import', 'export']);

  // Built-in types/constructors
  const builtinTypes = new Set(['True', 'False', 'Nil', 'List::', 'Z', 'S', 'Pair', 'Tuple']);

  lines.forEach((line, lineIndex) => {
    let charIndex = 0;

    // Skip whitespace
    function skipWhitespace() {
      while (charIndex < line.length && /\s/.test(line[charIndex])) {
        charIndex++;
      }
    }

    while (charIndex < line.length) {
      skipWhitespace();
      if (charIndex >= line.length) break;

      // Comments: -- and {- -}
      if (line.slice(charIndex, charIndex + 2) === '--') {
        builder.push(lineIndex, charIndex, line.length - charIndex,
          tokenTypes.indexOf('comment'), 0);
        break;
      }

      if (line.slice(charIndex, charIndex + 2) === '{-') {
        const start = charIndex;
        charIndex += 2;
        // Find end of block comment
        let depth = 1;
        while (charIndex < line.length && depth > 0) {
          if (line.slice(charIndex, charIndex + 2) === '{-') {
            depth++;
            charIndex += 2;
          } else if (line.slice(charIndex, charIndex + 2) === '-}') {
            depth--;
            charIndex += 2;
          } else {
            charIndex++;
          }
        }
        builder.push(lineIndex, start, charIndex - start,
          tokenTypes.indexOf('comment'), 0);
        continue;
      }

      // String literals
      if (line[charIndex] === '"') {
        const start = charIndex;
        charIndex++;
        while (charIndex < line.length && line[charIndex] !== '"') {
          if (line[charIndex] === '\\') charIndex++; // Skip escaped chars
          charIndex++;
        }
        if (charIndex < line.length) charIndex++; // Skip closing quote
        builder.push(lineIndex, start, charIndex - start,
          tokenTypes.indexOf('string'), 0);
        continue;
      }

      // Numbers
      if (/\d/.test(line[charIndex])) {
        const start = charIndex;
        while (charIndex < line.length && /\d/.test(line[charIndex])) {
          charIndex++;
        }
        builder.push(lineIndex, start, charIndex - start,
          tokenTypes.indexOf('number'), 0);
        continue;
      }

      // Operators - check multi-char operators first (order matters!)
      const twoCharOps = ['->', '|>', ':=', '<-'];
      let foundMultiChar = false;
      
      for (const op of twoCharOps) {
        if (line.slice(charIndex, charIndex + op.length) === op) {
          const start = charIndex;
          charIndex += op.length;
          builder.push(lineIndex, start, charIndex - start,
            tokenTypes.indexOf('operator'), 0);
          foundMultiChar = true;
          break;
        }
      }
      
      if (foundMultiChar) {
        continue;
      }

      // Single-char operators (excluding < and > to avoid conflicts with arrows)
      if (/[.@|:={},();_-]/.test(line[charIndex])) {
        const start = charIndex;
        charIndex++;
        builder.push(lineIndex, start, charIndex - start,
          tokenTypes.indexOf('operator'), 0);
        continue;
      }

      // Handle < and > separately only if they're not part of arrows
      if (line[charIndex] === '<' || line[charIndex] === '>') {
        // Double-check this isn't part of a multi-char operator we missed
        if ((line[charIndex] === '<' && line[charIndex + 1] === '-') ||
            (line[charIndex] === '-' && line[charIndex + 1] === '>')) {
          // This should have been caught above, skip for safety
          charIndex++;
          continue;
        }
        const start = charIndex;
        charIndex++;
        builder.push(lineIndex, start, charIndex - start,
          tokenTypes.indexOf('operator'), 0);
        continue;
      }

      // Identifiers (keywords, constructors, variables, functions)
      if (/[a-zA-Z_]/.test(line[charIndex])) {
        const start = charIndex;
        while (charIndex < line.length && /[a-zA-Z0-9_':]/.test(line[charIndex])) {
          charIndex++;
        }
        const identifier = line.slice(start, charIndex);

        let tokenType = 'variable';
        let modifier = 0;

        if (keywords.has(identifier)) {
          tokenType = 'keyword';
        } else if (builtinTypes.has(identifier) || /^[A-Z]/.test(identifier)) {
          tokenType = 'type';
        } else if (/^[a-z]/.test(identifier)) {
          // Check if it's a function definition
          const remainingLine = line.slice(charIndex).trim();
          if (remainingLine.startsWith(':=') ||
              (lineIndex > 0 && lines[lineIndex - 1].trim().startsWith('fn'))) {
            tokenType = 'function';
            modifier = 1 << tokenModifiers.indexOf('definition');
          }
        }

        builder.push(lineIndex, start, charIndex - start,
          tokenTypes.indexOf(tokenType), modifier);
        continue;
      }

      // Unknown character, skip
      charIndex++;
    }
  });
}

  // Make the text document manager listen on the connection
  documents.listen(connection);

  // Listen on the connection
  connection.listen();

} catch (error) {
  console.error('Failed to create LSP connection:', error);
  process.exit(1);
}
