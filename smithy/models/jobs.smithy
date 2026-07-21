$version: "2.0"

namespace io.superposition

resource Job {
    identifiers: {
        workspace_id: String
        org_id: String
        id: String
    }
    properties: {
        kronos_job_id: String
        description: String
        type: BackgroundJobType
        status: BackgroundJobStatus
        name: String
        progress: Integer
        workspace_schema: String
        created_at: DateTime
        logs: Document
    }
    read: GetJob
    list: ListJobs
    operations: [
        CancelJob
    ]
}

@documentation("Type of background job.")
enum BackgroundJobType {
    WEBHOOK
    PRIORITY_RECOMPUTE
    REDUCE
}

@documentation("Lifecycle status of a background job.")
enum BackgroundJobStatus {
    CREATED
    SCHEDULED
    INPROGRESS
    FAILED
    COMPLETED
}

@documentation("Execution details fetched from Kronos for a job.")
structure ExecutionDetails {
    attempt_count: Long

    max_attempts: Long

    started_at: DateTime

    completed_at: DateTime

    duration_ms: Long

    execution_status: String
}

@documentation("Full job detail including Kronos execution information.")
structure JobDetailResponse for Job {
    @required
    $id

    @required
    $kronos_job_id

    @required
    $description

    @required
    $type

    @required
    $status

    @required
    $name

    @required
    $progress

    @required
    $workspace_schema

    @required
    $created_at

    @required
    $logs

    execution: ExecutionDetails
}

@documentation("Job summary returned by list operations. Does not include workspace_schema.")
structure JobSummary for Job {
    @required
    $id

    @required
    $kronos_job_id

    @required
    $description

    @required
    $type

    @required
    $status

    @required
    $name

    @required
    $progress

    @required
    $created_at

    @required
    $logs
}

list JobList {
    member: JobSummary
}

@documentation("Response returned when a job is submitted. Contains the BJM job ID, Kronos job ID, and initial status.")
structure JobCreateResponse for Job {
    @required
    $id

    @required
    $kronos_job_id

    @required
    $status
}

@documentation("Retrieves a paginated list of background jobs in the workspace, optionally filtered by type and status.")
@readonly
@http(method: "GET", uri: "/jobs")
@tags(["Background Jobs"])
operation ListJobs {
    input := with [WorkspaceMixin] {
        @httpQuery("status")
        @notProperty
        status: BackgroundJobStatus

        @httpQuery("job_type")
        @notProperty
        job_type: BackgroundJobType
    }

    output := {
        @required
        data: JobList
    }
}

@documentation("Retrieves detailed information about a specific background job, including Kronos execution details such as attempt count, timing, and duration.")
@readonly
@http(method: "GET", uri: "/jobs/{id}")
@tags(["Background Jobs"])
operation GetJob with [GetOperation] {
    input := for Job with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: JobDetailResponse
}

@documentation("Cancels a background job that is not in a terminal state (COMPLETED or FAILED). Sends a cancellation request to Kronos and marks the job as FAILED.")
@http(method: "POST", uri: "/jobs/{id}/cancel")
@tags(["Background Jobs"])
operation CancelJob with [GetOperation] {
    input := for Job with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }
}
