/**
 * File-based data source — loads configuration from a local TOML/JSON file.
 *
 * Mirrors Rust/Python/Java `FileDataSource`. Honours `if_modified_since` via the file's last-modified
 * time (a 304-equivalent), and supports watching the file for changes via `fs.watch`. Filtering is
 * not applied here (the file is parsed whole); the local provider filters during evaluation.
 */

import * as fs from "fs";
import * as path from "path";

import { NativeResolver } from "superposition-bindings";

import { SuperpositionError } from "./errors";
import {
    BaseDataSource,
    Config,
    ConfigData,
    ExperimentData,
    FetchResponse,
} from "./data-source";

/** A live watch subscriber: a push channel the shared watcher fans changes out to, plus a `wake`
 *  used to unpark a waiting consumer (on a change, or when the source is closing). */
interface Subscriber {
    push: (filePath: string) => void;
    wake: () => void;
}

export class FileDataSource extends BaseDataSource {
    private readonly fileFormat: "toml" | "json";
    private readonly resolver: NativeResolver;
    private watcher: fs.FSWatcher | null = null;
    private readonly subscribers = new Set<Subscriber>();
    private closed = false;

    constructor(private readonly filePath: string) {
        super();
        const extension = path.extname(filePath).toLowerCase();
        if (extension === ".toml") {
            this.fileFormat = "toml";
        } else if (extension === ".json") {
            this.fileFormat = "json";
        } else {
            throw SuperpositionError.dataSourceError(
                `Unsupported file extension: ${filePath}. Supported formats are 'json' and 'toml'.`,
            );
        }
        // parseConfigFileWithFilters is an instance method on NativeResolver. koffi.load caches by
        // path, so this does not re-load the native lib when a provider already holds its own resolver.
        this.resolver = new NativeResolver();
    }

    /** The file's last-modified time. */
    private async lastModifiedAt(): Promise<Date> {
        try {
            const stats = await fs.promises.stat(this.filePath);
            return stats.mtime;
        } catch (error) {
            throw SuperpositionError.dataSourceError(
                `Failed to read modified time for config file ${this.filePath}: ${
                    error instanceof Error ? error.message : String(error)
                }`,
                error,
            );
        }
    }

    /** Whether the file is unchanged since `ifModifiedSince` (mtime at or before it). */
    private async isNotModified(ifModifiedSince: Date): Promise<boolean> {
        return (
            (await this.lastModifiedAt()).getTime() <= ifModifiedSince.getTime()
        );
    }

    private parse(
        content: string,
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Config {
        // Filter is applied at parse time by the native lib, honouring context + prefixes (matching
        // the Python/Java file sources).
        return this.resolver.parseConfigFileWithFilters(
            content,
            this.fileFormat,
            context,
            prefixFilter,
            excludePrefixFilter,
        );
    }

    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date,
    ): Promise<FetchResponse<ConfigData>> {
        if (
            ifModifiedSince !== undefined &&
            (await this.isNotModified(ifModifiedSince))
        ) {
            return FetchResponse.notModified<ConfigData>();
        }

        try {
            const now = new Date();
            const content = await fs.promises.readFile(this.filePath, "utf8");
            return FetchResponse.data<ConfigData>({
                data: this.parse(
                    content,
                    context,
                    prefixFilter,
                    excludePrefixFilter,
                ),
                fetchedAt: now,
            });
        } catch (error) {
            if (error instanceof SuperpositionError) {
                throw error;
            }
            throw SuperpositionError.dataSourceError(
                `Failed to read config file ${this.filePath}: ${
                    error instanceof Error ? error.message : String(error)
                }`,
                error,
            );
        }
    }

    fetchActiveExperiments(): Promise<FetchResponse<ExperimentData>> {
        return Promise.reject(
            SuperpositionError.dataSourceError(
                "Experiments not supported by FileDataSource",
            ),
        );
    }

    fetchCandidateActiveExperiments(): Promise<FetchResponse<ExperimentData>> {
        return Promise.reject(
            SuperpositionError.dataSourceError(
                "Experiments not supported by FileDataSource",
            ),
        );
    }

    fetchMatchingActiveExperiments(): Promise<FetchResponse<ExperimentData>> {
        return Promise.reject(
            SuperpositionError.dataSourceError(
                "Experiments not supported by FileDataSource",
            ),
        );
    }

    override supportsExperiments(): boolean {
        return false;
    }

    /**
     * Watch the file for changes, yielding its path on every change. Every caller gets its own stream
     * of events; they share one OS watcher, which starts with the first subscriber and stops when the
     * last one leaves.
     */
    override async *watch(): AsyncGenerator<string, void, unknown> {
        const queue: string[] = [];
        let notify: (() => void) | null = null;
        const wake = (): void => {
            if (notify) {
                const n = notify;
                notify = null;
                n();
            }
        };
        const subscriber: Subscriber = {
            push: (p: string) => {
                queue.push(p);
                wake();
            },
            wake,
        };

        this.subscribers.add(subscriber);
        this.ensureWatcher();
        try {
            while (!this.closed) {
                if (queue.length === 0) {
                    // Park until a change arrives or close() wakes us. Never parks indefinitely on
                    // shutdown: close() flips `closed` and wakes every subscriber, so the loop exits
                    // and the generator's finally runs instead of hanging a caller's return().
                    await new Promise<void>((resolve) => {
                        notify = resolve;
                    });
                    if (this.closed) {
                        break;
                    }
                }
                while (queue.length > 0) {
                    yield queue.shift() as string;
                }
            }
        } finally {
            this.subscribers.delete(subscriber);
            // The watcher exists for the subscribers; with none left it is just an open handle.
            if (this.subscribers.size === 0) {
                this.stopWatcher();
            }
        }
    }

    /** Start the shared watcher, unless a previous subscriber already did. */
    private ensureWatcher(): void {
        if (this.watcher) {
            return;
        }
        // realpath, so the directory we register matches the paths the OS reports back.
        const real = fs.realpathSync(this.filePath);
        const base = path.basename(real);
        const dir = path.dirname(real);
        this.watcher = fs.watch(dir, (_event, filename) => {
            if (filename && path.basename(filename.toString()) !== base) {
                return;
            }
            for (const sub of this.subscribers) {
                sub.push(this.filePath);
            }
        });
    }

    private stopWatcher(): void {
        if (this.watcher) {
            this.watcher.close();
            this.watcher = null;
        }
    }

    async close(): Promise<void> {
        this.closed = true;
        // Wake any parked watch consumers so their generators observe `closed` and exit.
        for (const sub of this.subscribers) {
            sub.wake();
        }
        this.subscribers.clear();
        this.stopWatcher();
    }
}
