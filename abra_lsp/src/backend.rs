use crate::utils::abra_error_to_diagnostic;
use abra_core::typecheck;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};
use tower_lsp::lsp_types::notification::PublishDiagnostics;
use tower_lsp::lsp_types::{Url, PublishDiagnosticsParams, InitializeParams, InitializeResult, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, InitializedParams, MessageType, DidOpenTextDocumentParams, TextDocumentItem, DidChangeTextDocumentParams, VersionedTextDocumentIdentifier};

#[derive(Debug)]
pub struct Backend {
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }

    fn get_diagnostics(&self, uri: Url, version: Option<i64>, text: String) -> PublishDiagnosticsParams {
        let module_path = uri.path();
        let diagnostics = match typecheck(module_path.to_string(), &text) {
            Ok(_) => vec![],
            Err(e) => {
                let diagnostic = abra_error_to_diagnostic(e, &text);
                vec![diagnostic]
            }
        };
        PublishDiagnosticsParams { uri, version, diagnostics }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full)),
            ..ServerCapabilities::default()
        };
        Ok(InitializeResult { capabilities, ..InitializeResult::default() })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::Info, "abra language server initialized").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let TextDocumentItem { uri, version, text, .. } = params.text_document;

        let diagnostics = self.get_diagnostics(uri, Some(version), text);
        self.client.send_custom_notification::<PublishDiagnostics>(diagnostics).await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let VersionedTextDocumentIdentifier { uri, version } = params.text_document;
        let text = if let Some(contents) = params.content_changes.pop() { contents.text } else { return; };

        let diagnostics = self.get_diagnostics(uri, version, text);
        self.client.send_custom_notification::<PublishDiagnostics>(diagnostics).await;
    }
}
