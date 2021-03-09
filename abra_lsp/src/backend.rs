use crate::utils::abra_error_to_diagnostic;
use abra_core::typecheck;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};
use tower_lsp::lsp_types::notification::PublishDiagnostics;
use tower_lsp::lsp_types::{Url, PublishDiagnosticsParams, InitializeParams, InitializeResult, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, InitializedParams, MessageType, DidOpenTextDocumentParams, TextDocumentItem, DidChangeTextDocumentParams, VersionedTextDocumentIdentifier};
use abra_core::parser::ast::ModuleId;
use abra_core::module_loader::{ModuleReader, ModuleLoader};

#[derive(Debug)]
struct LspModuleReader;

impl ModuleReader for LspModuleReader {
    fn read_module(&mut self, _module_id: &ModuleId) -> Option<String> {
        // TODO: Real implementation
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct Backend {
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }

    async fn module_id_from_url(&self, uri: &Url) -> ModuleId {
        let project_root = match self.client.workspace_folders().await {
            Ok(Some(folders)) => {
                folders.into_iter()
                    .find_map(|f| {
                        if uri.path().starts_with(f.uri.path()) {
                            Some(f.uri.path().to_string())
                        } else { None }
                    })
            }
            _ => None,
        };
        let module_uri = uri.path();
        let module_path = match project_root {
            None => module_uri.to_string(),
            Some(project_root) => module_uri.replace(&project_root, ""),
        };
        ModuleId::from_path(&module_path)
    }

    async fn get_diagnostics(&self, uri: Url, version: Option<i64>, text: String) -> PublishDiagnosticsParams {
        let module_id = self.module_id_from_url(&uri).await;

        let module_reader = LspModuleReader;
        let mut loader = ModuleLoader::new(module_reader);
        let diagnostics = match typecheck(module_id, &text, &mut loader) {
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

        let diagnostics = self.get_diagnostics(uri, Some(version), text).await;
        self.client.send_custom_notification::<PublishDiagnostics>(diagnostics).await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let VersionedTextDocumentIdentifier { uri, version } = params.text_document;
        let text = if let Some(contents) = params.content_changes.pop() { contents.text } else { return; };

        let diagnostics = self.get_diagnostics(uri, version, text).await;
        self.client.send_custom_notification::<PublishDiagnostics>(diagnostics).await;
    }
}
