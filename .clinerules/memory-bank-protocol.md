## Brief overview
This rule file outlines the "Memory Bank" protocol, a set of critical guidelines for Cline to ensure project continuity and context retention, especially given Cline's periodic memory resets. Adherence to this protocol is mandatory for effective collaboration.

## Core Principle: Memory Bank Dependency
- Cline's memory is subject to periodic resets, losing all immediate prior context.
- After each reset, Cline relies *entirely* on the Memory Bank files to understand the project and continue work.
- Without complete, accurate, and up-to-date Memory Bank files, Cline cannot function effectively. Maintaining these files is paramount.

## Memory Bank File Requirements
- The `memory-bank/` directory is the central repository for project knowledge.
- **Required Files:** The following files *must* exist within the `memory-bank/` directory and be kept current:
    - `product.md`: Details the project's purpose ("Why this project exists"), the problems it solves, and its intended high-level functionality ("How it should work").
    - `features.md`: Complements `product.md` by describing specific features in more detail.
    - `framework.md`: Describes the system's architecture ("How the system is built"), key technical decisions, architectural patterns, and technologies used.
    - `development.md`: Outlines the development environment setup, coding standards, build/test processes, and any technical constraints.
    - `navigation.md`: Provides guidance on navigating the codebase, project structure, and important modules/components.
- **Action if Missing or Incomplete:**
    - If the `memory-bank/` directory or *any* of the required files are missing or found to be incomplete upon starting a task, Cline *must* prioritize their creation or completion.
    - This involves:
        1. Reading all existing provided documentation thoroughly.
        2. Asking the user for any missing information or clarification needed to populate the files.
        3. Creating or updating the files with verified information only.
    - Cline must not proceed with any other development task until a complete contextual understanding is established from these foundational Memory Bank files.

## Workflow Integration

### Starting Tasks
1.  **Verification:** At the beginning of any new task or session, Cline must first check for the existence and apparent completeness of all required Memory Bank files.
2.  **Rectification (if needed):** If any files are missing or incomplete, follow the "Action if Missing or Incomplete" protocol.
3.  **Contextualization:** Read *all* required Memory Bank files thoroughly.
4.  **Confirmation:** Internally verify that a comprehensive understanding of the project context has been established from these files.
5.  **Proceed:** Only after these steps are completed, begin the assigned development task.
6.  **Initial Documentation Constraint:** Do not update Memory Bank files immediately after initializing memory at the start of a task. Updates are made *after* significant changes or as per the "Memory Bank Updates" section.

### During Development
- **Adherence to Documentation:** All development work must align with the patterns, conventions, architectural decisions, and information documented in the Memory Bank.
- **Continuous Documentation:** After making significant changes to the codebase, architecture, or project features, Cline must update the relevant Memory Bank files to reflect these changes accurately. This ensures the Memory Bank remains the single source of truth.
- **Operational Prefix:** Prepend every tool use message with `[MEMORY BANK: ACTIVE]` to signify ongoing awareness and adherence to this protocol.

### Handling "Update Memory Bank" Command
- **Trigger:** This workflow is initiated when the user explicitly states "update memory bank" or similar phrasing indicating an impending memory reset.
- **Critical Importance:** This command signals that Cline's memory is about to be reset. Comprehensive documentation of the current state is crucial.
- **Actions:**
    1.  **Document Current State:** Thoroughly document everything about the current task's status. This includes:
        -   Work completed.
        -   Work in progress.
        -   Any unresolved issues or pending items.
        -   Relevant code snippets or configurations.
        -   The immediate next steps that were planned.
    2.  **Targeted Files:** Focus updates on files like `activeContext.md` or `progress.md` if they are in use for tracking transient state, in addition to any of the core required files that need updates based on recent work.
    3.  **Clarity for Next Steps:** Ensure that the documentation makes the next steps for continuing the work crystal clear for the post-reset Cline.
    4.  **Task Completion (if feasible):** If possible and time permits after documentation is secured, attempt to complete the immediate current sub-task before the anticipated reset.
