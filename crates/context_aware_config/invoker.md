## Task States

-   New [Represent new task request to the server]
-   Ready [Ready to be executed]
-   Delayed [Delayed and Repeated tasks]
-   Watching [Delayed Task being observed by an executor]
-   Reserved [Picked up by an executor for processing]
-   Aborted [Terminal State]
-   Success [Terminal State]
-   Buried [Terminal State]

## Transitions

-   New => Ready
-   New => Delayed

-   Delayed => Watching _(When an executor is observing if its time to execute the task)_
-   Watching => Delayed _(Not yet to be executed)_
-   Watching => Ready _(When its time to execute the task)_

-   Ready => Reserved

-   Reserved => Ready _(Execution exceeded expected time to execute. Or, Task execution failed, moved to ready to be retired until MAX_RETRIES)_
-   Reserved => Success
-   Reserved => Aborted _(Task execution fails, retired for MAX_RETRIES times)_
-   Reserved => Buried _(Reserved for a long time)_

-   Read|Delayed => Aborted _(Manual Abort)_

## Architecture

Processing layer:

-   HTTP Server: Receives new task request
-   Executor

Persistence layer:

-   Tasks table

## HTTP Server

### Endpoints

-   POST /tasks _# Create a new task_

### Create Task Workflow

-   If task falls in delayed or repeated execution category, insert into `Tasks` table with state as `Delayed`
-   Else, insert into `Tasks` table with state as `Ready`

## Tasks

-   **Each task by default have to be executed within `TASK_TIMEOUT` after getting `Reserved`**
-   **To factor in the network latency, we will use `TASK_TIMEOUT + DELTA` to check for dead tasks**

### Metadata

-   state
-   created_at
-   reserved_at

## Executor

**Each executor is going to have its own `JOB_BUFFER` of fixed size `JOB_BUFFER_SIZE`**

### Watching Delayed/Repeated tasks

-   Fetches `Delayed` tasks and move them to `Watching` state
-   If any task is ready to be executed move it to `Ready` state
-   For repeated tasks, clone the task information with state as `Ready`
-   If task is not ready to be executed move it back to `Delayed` state from `Watching`

### Executing tasks

-   If `JOB_BUFFER` is empty, fetch `Ready` tasks
-   Move tasks to `Reserved` state, set `reserved_at`
-   Starts executing each task
    -   Executed successfully
        -   move task to `Success` state
    -   Execution failed
        -   increment `retry_count`
        -   if `retry_count` is less than `MAX_RETRIES`, move to `Ready` state
        -   if `retry_count` is equal to `MAX_RETRIES` move to `Aborted` state
    -   Execution time exceeded `TASK_TIMEOUT`
        -   increment `retry_count`
        -   if `retry_count` is less than `MAX_RETRIES`, move to `Ready` state
        -   if `retry_count` is equal to `MAX_RETRIES` move to `Aborted` state
    -   Remove task from `JOB_BUFFER`

#### Scenarios of failure

-   Task execution failed

    -   In this case the task is moved to `Ready` state, and can be picked up again for execution

-   Executor crashed

#### Possible Solution

Not a very frequent check,

-   Each executor can check for `Reserved` tasks where `current_time - reserved_at > TASK_TIMEOUT + DELTA` and,
    -   Move task to `Buried`

## Concurrency

### Race Condition

Multiple workers can end doing,

-   selecting same `Ready` tasks and updating state to `Reserved`
-   selecting same `Delayed` tasks and updating state to `Watching`

### Possible Solution

Something I found on Internet and also took help from ChatGPT.

```pgsql
BEGIN;

-- Select the first 100 'Ready' tasks and lock them for update
WITH locked_tasks AS (
  SELECT id FROM tasks
  WHERE state = 'Ready'
  ORDER BY id
  LIMIT 100
  FOR UPDATE -- locks row, prevents from being locked, modified or deleted by other transaction, until the current transaction ends
  SKIP LOCKED -- skips the rows which are locked
)
-- Update the selected rows to 'Reserved'
UPDATE tasks
SET state = 'Reserved'
WHERE id IN (SELECT id FROM locked_tasks);

COMMIT;
```

### `FOR UPDATE` and `SKIP LOCKED`

[Row-Level Locking](https://www.postgresql.org/docs/current/explicit-locking.html#LOCKING-ROWS)

[Postgres Doc -> The Locking Clause](https://www.postgresql.org/docs/current/sql-select.html)
Search for `The Locking Clause` on the above page to know more about `SKIP LOCKED`

[Hacker news discussion on SKIP LOCKED](https://news.ycombinator.com/item?id=20020501)

### Performance

_working on this_

-   Check it normal reads will be effected by `Row-level Locking`
