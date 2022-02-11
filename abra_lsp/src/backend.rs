use crate::utils::abra_error_to_diagnostic;
use abra_core::typecheck;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer};
use tower_lsp::lsp_types::notification::PublishDiagnostics;
use tower_lsp::lsp_types::{Url, PublishDiagnosticsParams, InitializeParams, InitializeResult, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, InitializedParams, MessageType, DidOpenTextDocumentParams, TextDocumentItem, DidChangeTextDocumentParams, VersionedTextDocumentIdentifier};
use abra_core::parser::ast::ModuleId;
use abra_core::module_loader::{ModuleReader, ModuleLoader};
use std::ops::Deref;
use std::path::PathBuf;

#[derive(Debug)]
pub struct LspModuleReader {
    project_root: Option<PathBuf>,
}

impl ModuleReader for LspModuleReader {
    fn resolve_module_path(&mut self, module_id: &ModuleId, with_respect_to: &ModuleId) -> String {
        todo!()
    }

    fn read_module(&mut self, module_id: &ModuleId, module_name: &String) -> Option<String> {
        todo!()
    }

    fn get_module_name(&self, module_id: &ModuleId) -> String {
        todo!()
    }
    // fn read_module(&self, module_id: &ModuleId) -> Option<String> {
    //     match &self.project_root {
    //         None => None,
    //         Some(project_root) => {
    //             let file_path = module_id.get_path(Some(project_root));
    //             match std::fs::read_to_string(file_path) {
    //                 Ok(contents) => Some(contents),
    //                 Err(_) => None
    //             }
    //         }
    //     }
    // }
}

#[derive(Debug)]
pub struct Backend {
    client: Client,
    project_root: Mutex<Option<String>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self { client, project_root: Mutex::new(None) }
    }

    async fn module_id_from_url(&self, uri: &Url) -> ModuleId {
        let project_root = self.project_root.lock().await;
        let module_uri = uri.path();
        let mut module_path = match project_root.deref() {
            None => module_uri.to_string(),
            Some(project_root) => module_uri.replace(project_root, ""),
        };
        if module_path.starts_with('/') {
            module_path = module_path.replacen('/', "", 1);
        }

        ModuleId::from_path(&module_path)
    }

    async fn get_diagnostics(&self, uri: Url, version: Option<i64>, text: String) -> PublishDiagnosticsParams {
        let module_id = self.module_id_from_url(&uri).await;

        let project_root = self.project_root.lock().await;
        let project_root_path = project_root.as_ref().map(|root| PathBuf::from(root));
        let mut module_reader = LspModuleReader { project_root: project_root_path };
        let mut loader = ModuleLoader::new(&mut module_reader);

        match typecheck(module_id, &text, &mut loader) {
            Ok(_) => PublishDiagnosticsParams { uri, version, diagnostics: vec![] },
            Err(e) => {
                let file_name = e.module_id().get_path(project_root.as_ref().unwrap_or(&"".to_string()));
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

        // Force mutex ref to drop, unlocking it
        {
            let mut project_root = self.project_root.lock().await;
            if project_root.is_none() {
                *project_root = match self.client.workspace_folders().await {
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
            }
        }

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
