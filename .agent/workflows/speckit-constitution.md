---
description: Create or update the project constitution from interactive or provided principle inputs.
---

# Step 1: Initialize Constitution
**Goal**: Load memory template.

1. **Load Skill**: Read `.agent/skills/speckit-specify/SKILL.md`.
2. **Load Template**: Read `.agent/skills/speckit-specify/resources/memory/constitution.md`.

# Step 2: Update Process
**Goal**: Amend the constitution.

1. **Identify Placeholders**: Look for `[PRINCIPLE_NAME]` etc.
2. **Derive Values**: Use user input or existing repo context (`README.md`).
3. **Version Bump**: Increment semantic version (Major/Minor/Patch).

# Step 3: Propagation
**Goal**: Ensure consistency.

1. **Check Dependent Templates**:
   - `plan-template.md` (Constitution Check).
   - `spec-template.md` (Constraints).
   - `tasks-template.md`.
2. **Update**: If constitution changes affect these templates, list the required updates.

# Step 4: Save & Report
**Goal**: Finalize governance.

1. **Write**: Overwrite `.agent/skills/speckit-specify/resources/memory/constitution.md`.
2. **Report**: Summarize new principles and version.
