---
description: Execute the implementation plan by processing and executing all tasks defined in tasks.md
---

# Step 1: Initialize Implementation Context
**Goal**: Prepare project environment.

1. **Load Skill**: Use `view_file` to read `.agent/skills/speckit-specify/SKILL.md`.
2. **Prerequisites**: Check ignores (git, docker, lint).
   - Use `run_command` to verify `.gitignore`, `.dockerignore` etc.
3. **Load Tasks**: Read `tasks.md`.

# Step 2: Verify Checklists
**Goal**: Ensure no blocking quality issues.

1. **Scan Checklists**: Look in `checklists/` directory.
2. **Status Check**:
   - Count Checked [x] vs Unchecked [ ] items.
   - If any incomplete -> **STOP** (Ask user for confirmation).
   - If all complete -> **PROCEED**.

# Step 3: Execute Task Phases
**Goal**: Implement feature incrementally.

1. **Phase 1 (Setup)**: Execute setup tasks. Verify.
2. **Phase 2 (Foundations)**: Execute blocking tasks. Verify.
3. **Phase 3+ (User Stories)**:
   - For each story:
     - Write Tests (if TDD).
     - Implement Core Logic.
     - Verify against `spec.md`.
   - **Task Tracking**: Mark tasks as `[x]` in `tasks.md` immediately upon completion.

# Step 4: Verification & Handover
**Goal**: Final proof of work.

1. **Run Tests**: Execute project test suite.
2. **Review Spec**: Ensure all requirements met.
3. **Report**: Summarize completed story points.
