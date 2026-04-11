---
description: Create or update the implementation plan (Phase 0 Research & Phase 1 Design).
---

# Step 1: Initialize Planning Context
**Goal**: Prepare the environment and load requirements.

1. **Load Skill**: Use `view_file` to read `.agent/skills/speckit-specify/SKILL.md` (Focus on Section 7: Planning Heuristics).
2. **Setup**: Run `run_command` with:
   `pwsh -ExecutionPolicy Bypass -File .agent/skills/speckit-specify/resources/scripts/powershell/setup-plan.ps1 -Json`
3. **Load Context**:
   - Read `spec.md`.
   - Read `.agent/skills/speckit-specify/resources/memory/constitution.md`.
   - Read the generated/existing `plan.md`.

# Step 2: Phase 0 - Research
**Goal**: Resolve any unknowns before designing.

1. **Identify Unknowns**: Check `spec.md` for `NEEDS CLARIFICATION` or technical uncertainties.
2. **Execute Research**: Use `search_web` or codebase exploration to answer questions.
3. **Document Findings**: Create or update `research.md` with "Decision", "Rationale", and "Alternatives".

# Step 3: Phase 1 - Design
**Goal**: Define the technical architecture.

1. **Data Model**: specific Entities, Fields, and Relationships in `data-model.md`.
2. **API Contracts**: Define Endpoints and Schemas in `contracts/` directory.
3. **Update Plan**: Fill in `plan.md` sections (Tech Stack, Architecture).
4. **Constitution Check**: Verify the design adheres to the Constitution.

# Step 4: Update Agent Context
**Goal**: Refresh the agent's memory banks.

1. **Run Script**: Execute:
   `pwsh -ExecutionPolicy Bypass -File .agent/skills/speckit-specify/resources/scripts/powershell/update-agent-context.ps1 -AgentType claude`

# Step 5: Report Status
**Goal**: Inform user of plan readiness.

1. **Summarize**: List generated artifacts (`plan.md`, `research.md`, `data-model.md`).
2. **Next Step**: Suggest running `/speckit.tasks` to break down the plan.
