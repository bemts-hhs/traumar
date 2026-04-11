---
description: Identify underspecified areas in the feature spec by asking targeted clarification questions.
---

# Step 1: Initialize Clarification Context
**Goal**: Analyze spec for gaps.

1. **Load Skill**: Read `.agent/skills/speckit-specify/SKILL.md` (Focus on Section 2.3 "Strict Clarification Limit" and Section 4 "Question Table Format").
2. **Prerequisites**: Check `spec.md` exists. if not, Abort.
3. **Scan Spec**: Load `spec.md` and identify ambiguous text ("fast", "robust"), missing edge cases, or undefined data models.

# Step 2: Generate Questions
**Goal**: Formulate high-impact questions.

1. **Constraints**:
   - MAX **3 Questions** total (System default).
   - Prioritize: Scope > Security > UX.
2. **Formulate**:
   - Draft the questions internally.
   - Use the "Question Table Format" from SKILL.md.
   - For each Question, provide **Suggested Answer**.

# Step 3: Interactive Clarification Loop
**Goal**: Get user answers.

1. **Ask**: Present questions to user using `notify_user` (or standard output if running in cli).
2. **Wait**: detailed response.
3. **Parse**: Understand user selection (A, B, or Custom).

# Step 4: Update Spec
**Goal**: Encode decisions into `spec.md`.

1. **Apply Changes**:
   - Add `## Clarifications` section if missing.
   - Update the actual requirement text (e.g., change "fast" to "< 200ms").
   - Do NOT just append Q&A at the bottom; fix the ambiguity in place where possible.
2. **Save**: Write updated `spec.md`.
