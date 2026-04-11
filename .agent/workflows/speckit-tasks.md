---
description: Generate an actionable, dependency-ordered tasks.md from the plan.
---

# Step 1: Initialize Task Context
**Goal**: Load design artifacts and format rules.

1. **Load Skill**: Use `view_file` to read `.agent/skills/speckit-specify/SKILL.md` (CRITICAL: Read Section 6 "Task Generation Rules").
2. **Prerequisites**: Run `run_command` with:
   `pwsh -ExecutionPolicy Bypass -File .agent/skills/speckit-specify/resources/scripts/powershell/check-prerequisites.ps1 -Json -RequireTasks -IncludeTasks`
3. **Load Context**:
   - Read `plan.md` (Tech stack, Phases).
   - Read `spec.md` (User Stories, Priorities).
   - Read `data-model.md` & `contracts/` (if exist).

# Step 2: Generate Tasks
**Goal**: Create a step-by-step implementation guide.

1. **Follow Strict Format**: Every task MUST be:
   `- [ ] [TaskID] [P?] [Story?] Description with file path`
   Example: `- [ ] T001 [US1] Create User model in src/models/user.py`
2. **Phase Organization**:
   - **Phase 1: Setup**: Project init, config.
   - **Phase 2: Foundations**: Shared libs, database setup.
   - **Phase 3+: User Stories**: Group by Story (Priority 1 first).
     - *Inside Story*: Tests -> Data -> Logic -> UI.
3. **Validation**: Ensure every task has a FILE PATH.

# Step 3: Write tasks.md
**Goal**: Save the task list.

1. **Write File**: Use `write_to_file` to save content to `tasks.md`.
2. **Report**: Summarize task count, identifying any Parallel [P] opportunities.
