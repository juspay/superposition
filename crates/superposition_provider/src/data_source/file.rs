use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use notify::{Event, EventKind, RecommendedWatcher, Watcher};
use serde_json::{Map, Value};
use superposition_core::{
    parse_toml_file, resolve_toml_imports, Config, ConfigFormat, JsonFormat,
};
use superposition_types::{ConfigFilter, PrefixList};
use tokio::sync::broadcast;

use crate::data_source::FetchResponse;
use crate::types::{Result, SuperpositionError, WatchStream};

use super::{ConfigData, ExperimentData, SuperpositionDataSource};

struct WatcherInner {
    watcher: RecommendedWatcher,
    /// Folders currently watched (non-recursively).
    dirs: HashSet<PathBuf>,
    broadcast_tx: broadcast::Sender<()>,
}

/// The files a config is made of: the main file plus, for TOML, every file it
/// imports.
#[derive(Default)]
struct ConfigFiles {
    paths: Vec<PathBuf>,
    /// `paths`, canonicalized, to match watcher events against.
    canonical: HashSet<PathBuf>,
}

impl ConfigFiles {
    fn new(paths: Vec<PathBuf>) -> Self {
        let canonical = paths.iter().map(|p| canonical(p)).collect();
        Self { paths, canonical }
    }

    /// The distinct folders holding these files.
    fn dirs(&self) -> HashSet<PathBuf> {
        self.paths.iter().map(|p| parent_dir(p)).collect()
    }
}

pub struct FileDataSource {
    file_path: PathBuf,
    file_format: &'static str,
    /// As of the last fetch (or watch start); shared with the watcher.
    files: Arc<RwLock<ConfigFiles>>,
    watcher: Mutex<Option<WatcherInner>>,
}

impl FileDataSource {
    pub fn new(file_path: PathBuf) -> std::result::Result<Self, String> {
        let file_format = match file_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|s| s.to_lowercase())
        {
            Some(ref ext) if ext == "json" => "json",
            // `.super.toml` reports `toml`; `.stoml` is SuperTOML too.
            Some(ref ext) if ext == "toml" || ext == "stoml" => "toml",
            Some(ext) => return Err(format!("Unsupported file extension '{}'.", ext)),
            None => {
                return Err(
                    "File path must have an extension to determine format.".into()
                );
            }
        };

        let files = ConfigFiles::new(vec![file_path.clone()]);
        Ok(Self {
            file_path,
            file_format,
            files: Arc::new(RwLock::new(files)),
            watcher: Mutex::new(None),
        })
    }

    /// The newest modification time across the config's files. A missing
    /// imported file counts as modified now, so the next fetch reports it.
    async fn last_modified_at(&self) -> Result<DateTime<Utc>> {
        let paths = self
            .files
            .read()
            .map(|files| files.paths.clone())
            .unwrap_or_else(|_| vec![self.file_path.clone()]);

        let mut newest = modified_at(&self.file_path).await?;
        for path in paths.iter().filter(|p| **p != self.file_path) {
            let modified = modified_at(path).await.unwrap_or_else(|_| Utc::now());
            newest = newest.max(modified);
        }
        Ok(newest)
    }

    async fn is_not_modified(&self, if_modified_since: DateTime<Utc>) -> Result<bool> {
        let last_modified_at = self.last_modified_at().await?;
        Ok(last_modified_at <= if_modified_since)
    }

    /// Remember the config's current files and, if watching, watch the folders
    /// holding them.
    fn set_files(&self, paths: Vec<PathBuf>) -> Result<()> {
        let files = ConfigFiles::new(paths);
        let dirs = files.dirs();
        if let Ok(mut current) = self.files.write() {
            *current = files;
        }

        let mut guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;
        if let Some(inner) = guard.as_mut() {
            sync_watched_dirs(inner, dirs);
        }
        Ok(())
    }
}

async fn modified_at(path: &Path) -> Result<DateTime<Utc>> {
    tokio::fs::metadata(path)
        .await
        .map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to read metadata for config file {:?}: {}",
                path, e
            ))
        })?
        .modified()
        .map(DateTime::<Utc>::from)
        .map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to read modified time for config file {:?}: {}",
                path, e
            ))
        })
}

/// The main file plus every file it imports. Falls back to just the main
/// file when it can't be read or its imports don't resolve: fixing that means
/// editing the main file, which is watched.
fn config_files(main: &Path, format: &str) -> Vec<PathBuf> {
    let mut paths = vec![main.to_path_buf()];
    if format == "toml" {
        let plan = std::fs::read_to_string(main)
            .ok()
            .and_then(|src| resolve_toml_imports(main, &src).ok());
        if let Some(plan) = plan {
            paths.extend(plan.imported_files().map(|import| import.path.clone()));
        }
    }
    paths
}

fn parse_config_file(path: &Path, format: &str) -> Result<Config> {
    let parse_error = |e: superposition_core::FormatError| {
        SuperpositionError::DataSourceError(format!(
            "Failed to parse {} config: {}",
            format.to_uppercase(),
            e
        ))
    };
    if format == "json" {
        let content = std::fs::read_to_string(path).map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to read config file {:?}: {}",
                path, e
            ))
        })?;
        JsonFormat::parse_config(&content).map_err(parse_error)
    } else {
        parse_toml_file(path).map_err(parse_error)
    }
}

fn parent_dir(path: &Path) -> PathBuf {
    match path.parent() {
        Some(dir) if !dir.as_os_str().is_empty() => dir.to_path_buf(),
        _ => PathBuf::from("."),
    }
}

/// Canonicalize, falling back to the canonical folder plus the file name for
/// files that no longer exist (e.g. in a remove event). macOS reports events
/// under `/private/var/...`, so both sides of a comparison go through this.
fn canonical(path: &Path) -> PathBuf {
    if let Ok(path) = path.canonicalize() {
        return path;
    }
    match path.file_name() {
        Some(name) => parent_dir(path)
            .canonicalize()
            .map(|dir| dir.join(name))
            .unwrap_or_else(|_| path.to_path_buf()),
        None => path.to_path_buf(),
    }
}

/// Whether a watcher event is about one of the config's files. Reads (access
/// events) never are: they don't change anything.
fn is_relevant(event: &Event, files: &ConfigFiles) -> bool {
    !matches!(event.kind, EventKind::Access(_))
        && event
            .paths
            .iter()
            .any(|path| files.canonical.contains(&canonical(path)))
}

/// Watch exactly `dirs`. A folder that can't be watched (e.g. an import
/// naming a subfolder that doesn't exist yet) is logged and skipped, and
/// retried on the next sync, so it never hides the parse error that reports
/// the missing file. `inner.dirs` records only folders actually watched.
fn sync_watched_dirs(inner: &mut WatcherInner, dirs: HashSet<PathBuf>) {
    let removed: Vec<PathBuf> = inner.dirs.difference(&dirs).cloned().collect();
    for dir in removed {
        if let Err(e) = inner.watcher.unwatch(&dir) {
            log::warn!("FileDataSource: failed to stop watching {:?}: {}", dir, e);
        }
        inner.dirs.remove(&dir);
    }
    let added: Vec<PathBuf> = dirs.difference(&inner.dirs).cloned().collect();
    for dir in added {
        match inner
            .watcher
            .watch(&dir, notify::RecursiveMode::NonRecursive)
        {
            Ok(()) => {
                inner.dirs.insert(dir);
            }
            Err(e) => log::warn!("FileDataSource: failed to watch {:?}: {}", dir, e),
        }
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        if let Some(if_modified_since) = if_modified_since {
            if self.is_not_modified(if_modified_since).await? {
                log::debug!(
                    "FileDataSource: config file not modified since {:?}",
                    if_modified_since
                );
                return Ok(FetchResponse::NotModified);
            }
        }

        let now = Utc::now();
        let path = self.file_path.clone();
        let format = self.file_format;
        let (config, files) = tokio::task::spawn_blocking(move || {
            (
                parse_config_file(&path, format),
                config_files(&path, format),
            )
        })
        .await
        .map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to read config file {:?}: {}",
                self.file_path, e
            ))
        })?;
        // Even when parsing fails, keep watching the files it was made of.
        self.set_files(files)?;
        let mut config = config?;

        config = config.filter(
            context,
            prefix_filter.map(PrefixList::from_iter).as_ref(),
            exclude_prefix_filter.map(PrefixList::from_iter).as_ref(),
        );

        Ok(FetchResponse::Data(ConfigData {
            data: config,
            fetched_at: now,
        }))
    }

    async fn fetch_active_experiments(
        &self,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::DataSourceError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_candidate_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _exclude_prefix_filter: Option<Vec<String>>,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::DataSourceError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    async fn fetch_matching_active_experiments(
        &self,
        _context: Option<Map<String, Value>>,
        _prefix_filter: Option<Vec<String>>,
        _exclude_prefix_filter: Option<Vec<String>>,
        _if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>> {
        Err(SuperpositionError::DataSourceError(
            "Experiments not supported by FileDataSource".into(),
        ))
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    fn watch(&self) -> Result<Option<WatchStream>> {
        // Acquire both locks upfront to prevent concurrent watcher creation
        let mut watcher_guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;

        // If already watching, return a new subscriber to the existing broadcast
        if let Some(inner) = watcher_guard.as_ref() {
            return Ok(Some(WatchStream {
                receiver: inner.broadcast_tx.subscribe(),
            }));
        }

        // Both checks confirmed None — safe to create under the lock
        let (tx, _rx) = broadcast::channel(16);
        let tx_clone = tx.clone();
        let files = Arc::clone(&self.files);

        // Watch folders rather than files: editors often save by writing a
        // new file and renaming it over the old one, which drops a watch on
        // the old file.
        let watcher = notify::recommended_watcher(
            move |res: std::result::Result<Event, notify::Error>| match res {
                Ok(event) => {
                    let relevant = files
                        .read()
                        .map(|files| is_relevant(&event, &files))
                        .unwrap_or(true);
                    if relevant {
                        let _ = tx_clone.send(());
                    }
                }
                Err(e) => {
                    log::error!("FileDataSource: watch error: {}", e);
                }
            },
        )
        .map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to create file watcher: {}",
                e
            ))
        })?;

        let mut inner = WatcherInner {
            watcher,
            dirs: HashSet::new(),
            broadcast_tx: tx,
        };
        let config = ConfigFiles::new(config_files(&self.file_path, self.file_format));
        sync_watched_dirs(&mut inner, config.dirs());
        // Without the main file's own folder nothing would ever be noticed.
        let main_dir = parent_dir(&self.file_path);
        if !inner.dirs.contains(&main_dir) {
            return Err(SuperpositionError::DataSourceError(format!(
                "Failed to watch folder {:?} of config file {:?}",
                main_dir, self.file_path
            )));
        }
        if let Ok(mut current) = self.files.write() {
            *current = config;
        }

        let subscriber = inner.broadcast_tx.subscribe();
        *watcher_guard = Some(inner);

        Ok(Some(WatchStream {
            receiver: subscriber,
        }))
    }

    async fn close(&self) -> Result<()> {
        let mut guard = self.watcher.lock().map_err(|e| {
            SuperpositionError::DataSourceError(format!(
                "Failed to lock watcher mutex: {}",
                e
            ))
        })?;
        *guard = None;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::time::{Duration, SystemTime};

    use super::*;

    /// A fresh folder under the system temp dir holding `files`.
    fn temp_config(name: &str, files: &[(&str, &str)]) -> PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "superposition-file-source-{}-{}",
            name,
            std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        for (file, src) in files {
            let path = dir.join(file);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, src).unwrap();
        }
        dir
    }

    fn touch(path: &Path, at: SystemTime) {
        std::fs::File::options()
            .append(true)
            .open(path)
            .unwrap()
            .set_modified(at)
            .unwrap();
    }

    const MAIN: &str = r#"
default-configs.import = ["pricing.default-configs.stoml"]
dimensions.import = ["geo.dimensions.stoml"]
overrides.import = ["city/delhi.overrides.stoml"]
"#;
    const PRICING: &str = r#"
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
"#;
    const GEO: &str = r#"
[dimensions]
city = { position = 1, schema = { type = "string" } }
"#;
    const DELHI: &str = r#"
[[overrides]]
_context_ = { city = "Delhi" }
per_km_rate = 21.0
"#;

    fn split_config(name: &str) -> PathBuf {
        temp_config(
            name,
            &[
                ("main.stoml", MAIN),
                ("pricing.default-configs.stoml", PRICING),
                ("geo.dimensions.stoml", GEO),
                ("city/delhi.overrides.stoml", DELHI),
            ],
        )
    }

    async fn fetch(source: &FileDataSource) -> Result<FetchResponse<ConfigData>> {
        source.fetch_filtered_config(None, None, None, None).await
    }

    #[test]
    fn stoml_files_are_toml() {
        for name in ["main.stoml", "main.super.toml", "config.toml"] {
            let source = FileDataSource::new(PathBuf::from(name)).unwrap();
            assert_eq!(source.file_format, "toml", "{name}");
        }
        assert!(FileDataSource::new(PathBuf::from("main.yaml")).is_err());
    }

    #[tokio::test]
    async fn reads_imported_files() {
        let dir = split_config("reads");
        let source = FileDataSource::new(dir.join("main.stoml")).unwrap();
        let FetchResponse::Data(data) = fetch(&source).await.unwrap() else {
            panic!("expected data");
        };
        assert_eq!(data.data.contexts.len(), 1);
        assert_eq!(data.data.default_configs.len(), 1);

        let paths = source.files.read().unwrap().paths.clone();
        assert_eq!(
            paths,
            [
                "main.stoml",
                "pricing.default-configs.stoml",
                "geo.dimensions.stoml",
                "city/delhi.overrides.stoml",
            ]
            .map(|file| dir.join(file))
        );
    }

    #[tokio::test]
    async fn a_change_to_an_imported_file_counts_as_modified() {
        let dir = split_config("modified");
        let source = FileDataSource::new(dir.join("main.stoml")).unwrap();
        let past = SystemTime::now() - Duration::from_secs(60);
        for file in [
            "main.stoml",
            "pricing.default-configs.stoml",
            "geo.dimensions.stoml",
            "city/delhi.overrides.stoml",
        ] {
            touch(&dir.join(file), past);
        }
        let FetchResponse::Data(data) = fetch(&source).await.unwrap() else {
            panic!("expected data");
        };
        assert!(source.is_not_modified(data.fetched_at).await.unwrap());

        touch(
            &dir.join("city/delhi.overrides.stoml"),
            SystemTime::now() + Duration::from_secs(60),
        );
        assert!(!source.is_not_modified(data.fetched_at).await.unwrap());
    }

    #[tokio::test]
    async fn parse_errors_name_the_imported_file() {
        let dir = split_config("errors");
        std::fs::write(
            dir.join("city/delhi.overrides.stoml"),
            DELHI.replace("per_km_rate", "base_fare"),
        )
        .unwrap();
        let source = FileDataSource::new(dir.join("main.stoml")).unwrap();
        let Err(err) = fetch(&source).await else {
            panic!("expected a parse error");
        };
        let err = err.to_string();
        assert!(err.contains("delhi.overrides.stoml"), "{err}");
        assert!(err.contains("base_fare"), "{err}");
        // The files are still known, so the watcher can pick up the fix.
        assert_eq!(source.files.read().unwrap().paths.len(), 4);
    }

    fn watched_dirs(source: &FileDataSource) -> HashSet<PathBuf> {
        source
            .watcher
            .lock()
            .unwrap()
            .as_ref()
            .unwrap()
            .dirs
            .clone()
    }

    /// `MAIN` with one more overrides import, `zones/blr.overrides.stoml`.
    fn main_importing_zones() -> String {
        MAIN.replace(
            "\"city/delhi.overrides.stoml\"",
            "\"city/delhi.overrides.stoml\", \"zones/blr.overrides.stoml\"",
        )
    }

    #[tokio::test]
    async fn watches_the_folders_of_imported_files() {
        let dir = split_config("folders");
        let source = FileDataSource::new(dir.join("main.stoml")).unwrap();
        let _stream = source.watch().unwrap().unwrap();
        assert_eq!(
            watched_dirs(&source),
            HashSet::from([dir.clone(), dir.join("city")])
        );

        // A new import in a new subfolder gets watched after the next fetch.
        std::fs::create_dir_all(dir.join("zones")).unwrap();
        std::fs::write(dir.join("zones/blr.overrides.stoml"), "").unwrap();
        std::fs::write(dir.join("main.stoml"), main_importing_zones()).unwrap();
        fetch(&source).await.unwrap();
        assert_eq!(
            watched_dirs(&source),
            HashSet::from([dir.clone(), dir.join("city"), dir.join("zones")])
        );
    }

    #[tokio::test]
    async fn a_missing_import_folder_doesnt_hide_the_parse_error() {
        let dir = split_config("missing-folder");
        std::fs::write(dir.join("main.stoml"), main_importing_zones()).unwrap();
        let source = FileDataSource::new(dir.join("main.stoml")).unwrap();

        // Watching starts anyway, without the folder that doesn't exist.
        let _stream = source.watch().unwrap().unwrap();
        assert_eq!(
            watched_dirs(&source),
            HashSet::from([dir.clone(), dir.join("city")])
        );

        // The fetch reports the missing file, not a watch failure.
        let Err(err) = fetch(&source).await else {
            panic!("expected a parse error");
        };
        let err = err.to_string();
        assert!(
            err.contains("can't find `zones/blr.overrides.stoml`"),
            "{err}"
        );

        // Once the folder exists, the next fetch watches it.
        std::fs::create_dir_all(dir.join("zones")).unwrap();
        std::fs::write(dir.join("zones/blr.overrides.stoml"), "").unwrap();
        fetch(&source).await.unwrap();
        assert!(watched_dirs(&source).contains(&dir.join("zones")));
    }

    #[tokio::test]
    async fn an_imported_file_change_fires_the_watch() {
        let dir = split_config("events");
        let source = FileDataSource::new(dir.join("main.stoml")).unwrap();
        let mut stream = source.watch().unwrap().unwrap();
        // Let the watcher settle before changing anything.
        tokio::time::sleep(Duration::from_millis(200)).await;

        std::fs::write(dir.join("city/delhi.overrides.stoml"), DELHI).unwrap();
        let event =
            tokio::time::timeout(Duration::from_secs(10), stream.receiver.recv()).await;
        assert!(event.is_ok(), "no watch event for an imported file change");
    }

    #[test]
    fn only_events_on_config_files_are_relevant() {
        let dir = split_config("relevant");
        let files = ConfigFiles::new(config_files(&dir.join("main.stoml"), "toml"));
        let event = |kind, file: &str| Event::new(kind).add_path(dir.join(file));
        let modify = EventKind::Modify(notify::event::ModifyKind::Any);
        let access = EventKind::Access(notify::event::AccessKind::Any);

        assert!(is_relevant(&event(modify, "geo.dimensions.stoml"), &files));
        assert!(is_relevant(
            &event(modify, "city/delhi.overrides.stoml"),
            &files
        ));
        assert!(!is_relevant(&event(modify, "notes.txt"), &files));
        assert!(!is_relevant(&event(access, "geo.dimensions.stoml"), &files));
        // A removed file can't be canonicalized; its folder still can.
        std::fs::remove_file(dir.join("geo.dimensions.stoml")).unwrap();
        let remove = EventKind::Remove(notify::event::RemoveKind::File);
        assert!(is_relevant(&event(remove, "geo.dimensions.stoml"), &files));
    }
}
