use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use dashmap::DashMap;
use superposition_core::FsLoader;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::workspace::{self, FileKind, Overlay};
use crate::{completions, diagnostics, hover};

/// The latest check of a main file and the files it imports.
struct Group {
    files: Vec<PathBuf>,
    schema: toml::Table,
    /// Every file diagnostics were published to, so files that leave the
    /// group can be cleared.
    published: Vec<Url>,
}

pub struct Backend {
    client: Client,
    documents: DashMap<Url, String>,
    /// Imported file → the main file that imports it.
    owners: DashMap<PathBuf, PathBuf>,
    /// Main file → its latest check.
    groups: DashMap<PathBuf, Arc<Group>>,
    /// Main file → the generation of its newest check, so a slower, older
    /// check can't overwrite a newer one.
    latest: DashMap<PathBuf, u64>,
    generation: AtomicU64,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            owners: DashMap::new(),
            groups: DashMap::new(),
            latest: DashMap::new(),
            generation: AtomicU64::new(0),
        }
    }

    /// Re-check whatever `uri` belongs to and publish the results.
    async fn validate(&self, uri: &Url) {
        let Ok(path) = uri.to_file_path() else {
            // No file path (e.g. an untitled buffer): no imports to follow.
            let Some(text) = self.text(uri) else { return };
            let diags = diagnostics::compute(&text);
            self.client
                .publish_diagnostics(uri.clone(), diags, None)
                .await;
            return;
        };

        match FileKind::of(&path) {
            FileKind::Imported(section) => match self.main_of(&path).await {
                Some(main) => self.check_group(main).await,
                None => {
                    let text = self.text(uri).unwrap_or_default();
                    let diags = workspace::check_orphan(&path, section, &text);
                    self.client
                        .publish_diagnostics(uri.clone(), diags, None)
                        .await;
                }
            },
            FileKind::Main | FileKind::Other => self.check_group(path).await,
        }
    }

    fn text(&self, uri: &Url) -> Option<String> {
        self.documents.get(uri).map(|text| text.value().clone())
    }

    /// Every open document that has a file path.
    fn open_files(&self) -> HashMap<PathBuf, String> {
        self.documents
            .iter()
            .filter_map(|doc| {
                let path = doc.key().to_file_path().ok()?;
                Some((path, doc.value().clone()))
            })
            .collect()
    }

    /// The main file importing `path`: the cached owner if its group still
    /// lists `path`, otherwise found by walking up the folders.
    async fn main_of(&self, path: &Path) -> Option<PathBuf> {
        let cached = self.owners.get(path).map(|main| main.value().clone());
        if let Some(main) = cached {
            let still_imported = self
                .groups
                .get(&main)
                .is_some_and(|group| group.files.iter().any(|file| file == path));
            if still_imported {
                return Some(main);
            }
            self.owners.remove(path);
        }

        let open = self.open_files();
        let path = path.to_path_buf();
        tokio::task::spawn_blocking(move || {
            let loader = Overlay {
                open,
                fallback: &FsLoader,
            };
            workspace::find_main(&path, &loader)
        })
        .await
        .ok()
        .flatten()
    }

    /// Check `main` and its imports, then publish diagnostics to every file
    /// in the group and clear files that left it.
    async fn check_group(&self, main: PathBuf) {
        let generation = self.generation.fetch_add(1, Ordering::SeqCst) + 1;
        self.latest.insert(main.clone(), generation);

        let open = self.open_files();
        let check_main = main.clone();
        let Ok(check) = tokio::task::spawn_blocking(move || {
            let loader = Overlay {
                open,
                fallback: &FsLoader,
            };
            workspace::check_group(&check_main, &loader)
        })
        .await
        else {
            return;
        };
        let is_latest = self
            .latest
            .get(&main)
            .is_some_and(|latest| *latest.value() == generation);
        if !is_latest {
            return;
        }

        let previous = self
            .groups
            .get(&main)
            .map(|group| Arc::clone(group.value()));

        for file in check.files.iter().skip(1) {
            self.owners.insert(file.clone(), main.clone());
        }
        let mut published = Vec::new();
        for (path, diags) in check.diagnostics {
            if let Ok(url) = Url::from_file_path(&path) {
                self.client
                    .publish_diagnostics(url.clone(), diags, None)
                    .await;
                published.push(url);
            }
        }

        if let Some(previous) = previous {
            for url in previous
                .published
                .iter()
                .filter(|url| !published.contains(url))
            {
                self.client
                    .publish_diagnostics(url.clone(), vec![], None)
                    .await;
            }
            for file in previous
                .files
                .iter()
                .filter(|file| !check.files.contains(file))
            {
                self.owners.remove_if(file, |_, owner| *owner == main);
            }
        }

        self.groups.insert(
            main,
            Arc::new(Group {
                files: check.files,
                schema: check.schema,
                published,
            }),
        );
    }

    /// The group `uri` belongs to, for completion and hover.
    fn group_of(&self, uri: &Url) -> Option<Arc<Group>> {
        let path = uri.to_file_path().ok()?;
        let main = match FileKind::of(&path) {
            FileKind::Imported(_) => {
                self.owners.get(&path).map(|main| main.value().clone())?
            }
            FileKind::Main | FileKind::Other => path,
        };
        self.groups
            .get(&main)
            .map(|group| Arc::clone(group.value()))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Initializing superTOML Analyzer...")
            .await;
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        "=".to_string(),
                        " ".to_string(),
                        "{".to_string(),
                        ",".to_string(),
                    ]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "supertoml-analyzer".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "supertoml-analyzer initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents
            .insert(uri.clone(), params.text_document.text);
        self.validate(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.documents.insert(uri.clone(), change.text);
            self.validate(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.remove(&uri);
        // A file in a group keeps its diagnostics: re-check the group from
        // disk, since unsaved edits were just discarded.
        match self.group_of(&uri) {
            Some(group) if group.files.len() > 1 => {
                if let Some(main) = group.files.first() {
                    self.check_group(main.clone()).await;
                }
            }
            _ => {
                self.client.publish_diagnostics(uri, vec![], None).await;
            }
        }
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let Some(text) = self.text(uri) else {
            return Ok(None);
        };
        let group = self.group_of(uri);
        Ok(completions::compute_with(
            &text,
            pos,
            group.as_ref().map(|group| &group.schema),
        ))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let Some(text) = self.text(uri) else {
            return Ok(None);
        };
        let group = self.group_of(uri);
        Ok(hover::compute_with(
            &text,
            pos,
            group.as_ref().map(|group| &group.schema),
        ))
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let Some(text) = self.text(uri) else {
            return Ok(None);
        };

        // Use taplo formatter for VS Code "Even Better TOML" compatible formatting
        let formatted =
            taplo::formatter::format(&text, taplo::formatter::Options::default());

        let line_count = text.lines().count();
        let last_line_len = text.lines().last().map_or(0, |l| l.len());

        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: line_count as u32,
                    character: last_line_len as u32,
                },
            },
            new_text: formatted,
        }]))
    }
}
