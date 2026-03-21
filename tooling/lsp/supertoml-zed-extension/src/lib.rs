use zed_extension_api::serde_json::{self, Value};
use zed_extension_api::{self as zed, Command, LanguageServerId, Result, Worktree};

struct SuperTomlAnalyzerExtension {
    cached_binary_path: Option<String>,
}

impl zed::Extension for SuperTomlAnalyzerExtension {
    fn new() -> Self {
        Self {
            cached_binary_path: None,
        }
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        _worktree: &Worktree,
    ) -> Result<Command> {
        // Check if we already have a cached path
        if let Some(path) = &self.cached_binary_path {
            return Ok(Command {
                command: path.clone(),
                args: Vec::new(),
                env: Default::default(),
            });
        }
        Ok(Command {
            command:
                "/Users/kartik.gajendra/superposition/target/debug/supertoml-analyzer"
                    .to_string(),
            args: Vec::new(),
            env: Default::default(),
        })

        // // Try to find supertoml-analyzer in PATH
        // if let Some(path) = worktree.which("supertoml-analyzer") {
        //     self.cached_binary_path = Some(path.clone());
        //     return Ok(Command {
        //         command: path,
        //         args: Vec::new(),
        //         env: Default::default(),
        //     });
        // }

        // // Try to find it relative to the workspace (for development)
        // let worktree_root = worktree.root_path();
        // let possible_paths = [
        //     format!("{}/target/debug/supertoml-analyzer", worktree_root),
        //     format!("{}/target/release/supertoml-analyzer", worktree_root),
        //     format!("{}/../target/debug/supertoml-analyzer", worktree_root),
        //     format!("{}/../target/release/supertoml-analyzer", worktree_root),
        // ];

        // for path in possible_paths {
        //     if std::path::Path::new(&path).exists() {
        //         self.cached_binary_path = Some(path.clone());
        //         return Ok(Command {
        //             command: path,
        //             args: Vec::new(),
        //             env: Default::default(),
        //         });
        //     }
        // }

        // Err("supertoml-analyzer not found in PATH or target directory. Please build it with 'cargo build' or install it to PATH.".to_string())
    }

    fn language_server_initialization_options(
        &mut self,
        _language_server_id: &LanguageServerId,
        _worktree: &Worktree,
    ) -> Result<Option<Value>> {
        Ok(Some(serde_json::json!({
            "diagnostics": {
                "enable": true
            },
            "checkOnSave": {
                "enable": true
            },
            "completions": {
                "enable": true
            },
            "hover": {
                "enable": true
            }
        })))
    }
}

zed::register_extension!(SuperTomlAnalyzerExtension);
