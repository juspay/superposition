package io.juspay.superposition.openfeature.data_source;

import io.juspay.superposition.openfeature.FfiUtils;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.time.Instant;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CopyOnWriteArrayList;

import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uniffi.superposition_client.OperationException;
import uniffi.superposition_types.Config;

/**
 * File-based implementation of SuperpositionDataSource.
 *
 * Reads configuration from a local JSON or TOML file, picking the parser from the file
 * extension. Honors {@code ifModifiedSince} via the file's last-modified time, returning
 * {@code NotModified} when the file is unchanged.
 * Context and prefix filtering are applied by the core via
 * {@link FfiUtils#parseConfigFileWithFilters}, so they prune contexts and overrides exactly
 * the way the server would.
 *
 * Supports file watching via {@link java.nio.file.WatchService}. Each call to {@link #watch()}
 * returns its own {@link WatchStream}; a single daemon thread fans every change to the watched
 * file out to all of them.
 *
 * Does NOT support experiments.
 */
public class FileDataSource implements SuperpositionDataSource {

    private static final Logger log = LoggerFactory.getLogger(FileDataSource.class);

    private static final String JSON = "json";
    private static final String TOML = "toml";

    private final Path filePath;
    private final String fileFormat;

    private final Object watcherLock = new Object();
    private final CopyOnWriteArrayList<WatchStream> subscribers = new CopyOnWriteArrayList<>();
    private WatchService watchService = null;

    /**
     * Create a new file-based data source.
     *
     * @param filePath path to the configuration file; the extension must be {@code .json} or {@code .toml}
     * @throws SuperpositionError if the extension is missing or unsupported
     */
    public FileDataSource(Path filePath) throws SuperpositionError {
        this.filePath = filePath;
        this.fileFormat = formatOf(filePath);
        log.debug("FileDataSource initialized for {} file: {}", fileFormat, filePath);
    }

    private static String formatOf(Path filePath) throws SuperpositionError {
        String name = filePath.getFileName().toString();
        int dot = name.lastIndexOf('.');
        if (dot < 0 || dot == name.length() - 1) {
            throw SuperpositionError.dataSourceError(
                "File path must have an extension to determine format: " + filePath);
        }
        String extension = name.substring(dot + 1).toLowerCase(Locale.ROOT);
        return switch (extension) {
            case JSON -> JSON;
            case TOML -> TOML;
            default -> throw SuperpositionError.dataSourceError(
                "Unsupported file extension '" + extension + "'. Supported formats are 'json' and 'toml'.");
        };
    }

    @Override
    public FetchResponse<ConfigData> fetchFilteredConfig(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        if (ifModifiedSince.isPresent() && isNotModified(ifModifiedSince.get())) {
            log.debug("FileDataSource: config file not modified since {}", ifModifiedSince.get());
            return FetchResponse.notModified();
        }
        Instant now = Instant.now();

        String content;
        try {
            content = Files.readString(filePath);
        } catch (IOException e) {
            throw SuperpositionError.dataSourceError(
                "Failed to read config file " + filePath + ": " + e.getMessage(), e);
        }

        Config config;
        try {
            config = FfiUtils.parseConfigFileWithFilters(
                content, fileFormat, context.orElse(null), prefixFilter.orElse(null));
        } catch (OperationException e) {
            throw SuperpositionError.dataSourceError(
                "Failed to parse " + fileFormat.toUpperCase(Locale.ROOT) + " config from "
                    + filePath + ": " + e.getMessage(), e);
        }

        return FetchResponse.data(new ConfigData(config, now));
    }

    /** The file's last-modified time. */
    private Instant lastModifiedAt() throws SuperpositionError {
        try {
            return Files.getLastModifiedTime(filePath).toInstant();
        } catch (IOException e) {
            throw SuperpositionError.dataSourceError(
                "Failed to read modified time for config file " + filePath + ": " + e.getMessage(),
                e);
        }
    }

    /** Whether the file is unchanged since {@code ifModifiedSince} (mtime at or before it). */
    private boolean isNotModified(Instant ifModifiedSince) throws SuperpositionError {
        return !lastModifiedAt().isAfter(ifModifiedSince);
    }

    @Override
    public FetchResponse<ExperimentData> fetchActiveExperiments(
            Optional<Instant> ifModifiedSince) throws SuperpositionError {
        throw SuperpositionError.dataSourceError("Experiments not supported by FileDataSource");
    }

    @Override
    public FetchResponse<ExperimentData> fetchCandidateActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince) throws SuperpositionError {
        throw SuperpositionError.dataSourceError("Experiments not supported by FileDataSource");
    }

    @Override
    public FetchResponse<ExperimentData> fetchMatchingActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince) throws SuperpositionError {
        throw SuperpositionError.dataSourceError("Experiments not supported by FileDataSource");
    }

    @Override
    public boolean supportsExperiments() {
        return false;
    }

    /**
     * Set up file watching. Each caller gets its own {@link WatchStream}; one daemon thread
     * fans OS events for the target file out to every live stream.
     */
    @Override
    public Optional<WatchStream> watch() throws SuperpositionError {
        synchronized (watcherLock) {
            WatchStream stream = new WatchStream();
            subscribers.add(stream);

            if (watchService != null) {
                log.debug("FileDataSource: added subscriber to existing watcher for {}", filePath);
                return Optional.of(stream);
            }

            Path dir = filePath.toAbsolutePath().getParent();
            if (dir == null) {
                subscribers.remove(stream);
                throw SuperpositionError.dataSourceError(
                    "Cannot watch file with no parent directory: " + filePath);
            }

            WatchService service = null;
            try {
                service = dir.getFileSystem().newWatchService();
                dir.register(
                    service,
                    StandardWatchEventKinds.ENTRY_CREATE,
                    StandardWatchEventKinds.ENTRY_MODIFY,
                    StandardWatchEventKinds.ENTRY_DELETE);
            } catch (Exception e) {
                closeQuietly(service);
                subscribers.remove(stream);
                throw SuperpositionError.dataSourceError(
                    "Failed to set up file watcher for " + filePath + ": " + e.getMessage(), e);
            }

            // Only reachable when registration succeeded — the catch above always throws. The
            // field must not be published before that: a half-registered watcher would leave
            // watchService non-null with no thread running (it starts below), so the next watch()
            // would take the "existing watcher" branch and hand back a stream that never fires.
            watchService = service;

            Thread watchThread = getWatchThread();
            watchThread.setDaemon(true);
            watchThread.start();

            return Optional.of(stream);
        }
    }

    private @NotNull Thread getWatchThread() {
        Path watchedFileName = filePath.getFileName();
        WatchService ws = watchService;

        return new Thread(() -> {
            log.debug("FileDataSource: watch thread started for {}", filePath);
            while (!Thread.currentThread().isInterrupted()) {
                WatchKey key;
                try {
                    key = ws.take();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                } catch (java.nio.file.ClosedWatchServiceException e) {
                    log.debug("FileDataSource: watch service closed for {}", filePath);
                    break;
                }

                for (WatchEvent<?> event : key.pollEvents()) {
                    // Editors and config deploys often save via atomic rename, which surfaces
                    // as CREATE (or OVERFLOW) rather than MODIFY — all of them mean "re-read".
                    Object ctx = event.context();
                    if (!(ctx instanceof Path) || watchedFileName.equals(ctx)) {
                        log.debug("FileDataSource: change detected in {}", filePath);
                        subscribers.forEach(WatchStream::notifyChange);
                    }
                }

                if (!key.reset()) {
                    log.warn("FileDataSource: watch key no longer valid for {}", filePath);
                    break;
                }
            }
            log.debug("FileDataSource: watch thread exiting for {}", filePath);
        }, "superposition-file-watcher-" + filePath.getFileName());
    }

    @Override
    public void close() {
        synchronized (watcherLock) {
            // Closing unblocks the watch thread with ClosedWatchServiceException.
            closeQuietly(watchService);
            watchService = null;
            subscribers.forEach(WatchStream::close);
            subscribers.clear();
        }
        log.debug("FileDataSource closed for {}", filePath);
    }

    /** Releases the watcher's file descriptor. Failing to close it is not worth failing over. */
    private static void closeQuietly(WatchService service) {
        if (service == null) {
            return;
        }
        try {
            service.close();
        } catch (IOException e) {
            log.warn("FileDataSource: error closing watch service: {}", e.getMessage());
        }
    }
}
