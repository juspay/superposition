((rust-mode . ((eglot-workspace-configuration
                . ((rust-analyzer
                    . (:checkOnSave (:command "clippy" :enable t)
                                    :cargo (:features "all"))))))))
