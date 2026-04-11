---
description: Generate a custom checklist (Unit Tests for Requirements).
---

# Step 1: Initialize Checklist Context
**Goal**: Define the scope of verification.

1. **Load Skill**: Read `.agent/skills/speckit-specify/SKILL.md` (Focus on Section 5: "Unit Tests for Requirements").
2. **Context**: Read `spec.md` and `plan.md` to understand domain.

# Step 2: Define Checklist Type
**Goal**: Decide what kind of checklist to build (UX, Security, API, Test, etc).

1. **Ask User**: If not specified in arguments, ask: "What domain do you want to check? (e.g., UX, Security, API, Performance)".
2. **Derive Questions**: Create 3 diagnostic questions to refine scope if needed.

# Step 3: Generate Checklist Items
**Goal**: Create high-quality, non-implementation checklist items.

1. **Apply Heuristics**:
   - ✅ Correct: "Is 'fast' quantified?"
   - ❌ Wrong: "Verify page loads fast."
2. **Categories**:
   - Requirement Completeness
   - Requirement Clarity
   - Requirement Consistency
   - Edge Case Coverage

# Step 4: Save Checklist
**Goal**: Output the file.

1. **Create File**: `checklists/[domain].md`.
2. **Content**: Use the strict format with `CHK00X` IDs.
3. **Report**: Link to the new file.
