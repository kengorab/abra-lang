use crate::utils::abra_error_to_diagnostic;
use abra_core::typecheck;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};
use tower_lsp::lsp_types::notification::PublishDiagnostics;
use tower_lsp::lsp_types::{Url, PublishDiagnosticsParams, InitializeParams, InitializeResult, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, InitializedParams, MessageType, DidOpenTextDocumentParams, TextDocumentItem, DidChangeTextDocumentParams, VersionedTextDocumentIdentifier};
use abra_core::parser::ast::ModuleId;
use abra_core::module_loader::{ModuleReader, ModuleLoader};
use std::path::PathBuf;
use abra_core::common::fs_module_reader::FsModuleReader;

#[derive(Debug)]
pub struct Backend {
    client: Client,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client }
    }

    async fn get_diagnostics(&self, uri: Url, version: Option<i64>, text: String) -> PublishDiagnosticsParams {
        let path = uri.to_file_path().unwrap();
        let parent = path.parent().unwrap().to_path_buf();
        let module_path = path.file_name().unwrap().to_str().unwrap().to_string();
        let module_id = ModuleId::parse_module_path(&format!("./{}", module_path)).unwrap();

        let mut module_reader = FsModuleReader::new(module_id.clone(), &parent);
        let mut loader = ModuleLoader::new(&mut module_reader);

        match typecheck(module_id, &text, &mut loader) {
            Ok(_) =>  PublishDiagnosticsParams { uri, version, diagnostics: vec![] },
            Err(e) => {
                let file_name = PathBuf::from(module_reader.get_module_name(&e.module_id()))
                    .with_extension("abra")
                    .canonicalize()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string();
                let diagnostic = abra_error_to_diagnostic(e, &file_name, &text);

                let uri = Url::from_file_path(file_name).unwrap();
                PublishDiagnosticsParams { uri, version, diagnostics: vec![diagnostic] }
            }
        }
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
