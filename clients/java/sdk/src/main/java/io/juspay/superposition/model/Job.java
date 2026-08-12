
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Job implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Job");
    private static final Job $INSTANCE = new Job();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("job_type", BackgroundJobType.$SCHEMA,
        "kronos_job_id", PreludeSchemas.STRING,
        "name", PreludeSchemas.STRING,
        "description", PreludeSchemas.STRING,
        "progress", PreludeSchemas.INTEGER,
        "created_at", SharedSchemas.DATE_TIME,
        "workspace_schema", PreludeSchemas.STRING,
        "logs", PreludeSchemas.DOCUMENT,
        "status", BackgroundJobStatus.$SCHEMA);

    private static final List<Schema> $OPERATIONS = List.of(CancelJob.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Job instance() {
        return $INSTANCE;
    }

    private Job() {}

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Map<String, Schema> identifiers() {
        return $IDENTIFIERS;
    }

    @Override
    public Map<String, Schema> properties() {
        return $PROPERTIES;
    }

    @Override
    public Schema read() {
        return GetJob.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListJobs.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

