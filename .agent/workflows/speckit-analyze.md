---
description: Perform a consistency and quality analysis across spec, plan, and tasks.
---

# Step 1: Initialize Analysis Context
**Goal**: Load the analysis rules and project artifacts.

1. **Load Skill**: Use `view_file` to read `.agent/skills/speckit-specify/SKILL.md` (focus on Section 7: Analysis Heuristics).
2. **Prerequisites**: Run `run_command` with:
   `pwsh -ExecutionPolicy Bypass -File .agent/skills/speckit-specify/resources/scripts/powershell/check-prerequisites.ps1 -Json -RequireTasks -IncludeTasks`
3. **Load Artifacts**:
   - Read `spec.md` (Overview, Requirements).
   - Read `plan.md` (Architecture, Data Model).
   - Read `tasks.md` (Task coverage).
   - Read `.agent/skills/speckit-specify/resources/memory/constitution.md` (Constitution).

# Step 2: Perform Analysis
**Goal**: Identify inconsistencies without modifying files.

1. **Check Constitution**: Are there any violations of the Constitution in the Plan or Tasks? (CRITICAL)
2. **Check Coverage**:
   - Do all requirements in `spec.md` have corresponding tasks in `tasks.md`?
   - Do tasks reference files defined in `plan.md`?
3. **Check Ambiguity**: Are there vague terms ("robust", "fast") without metrics?
4. **Check Duplication**: Are there duplicate requirements?

# Step 3: Report Findings
**Goal**: Output a structured report to the user.

1. **Generate Table**: Create a Markdown table summarizing findings:
   `| ID | Category | Severity | Summary | Recommendation |`
2. **Severity Levels**:
   - **CRITICAL**: Constitution violation or missing core artifact.
   - **HIGH**: Untestable requirement or coverage gap.
   - **MEDIUM**: Ambiguity or terminology drift.
3. **Next Actions**: Recommend whether to proceed to Implementation or refine the Spec/Plan.
**Constraint**: Do NOT modify the files automatically. Only report.
