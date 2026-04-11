---
description: Create a new feature specification using Speckit SDD methodology
---

# Step 1: Initialize SDD Skill Context
**Goal**: Load the expert knowledge required for creating high-quality specifications.

1. **Load Skill**: Use `view_file` to read the skill definition at `.agent/skills/speckit-specify/SKILL.md`.
2. **Internalize**: Read and understand the "Specification Drafting Strategy", "Success Criteria Guidelines", and "Quality Validation" sections. You will need to apply these strict rules in Step 4.

# Step 2: Acquire Feature Description
**Goal**: Ensure we have a clear feature request to work with.

1. **Check Input**: If the user has not provided a feature description in the chat, **STOP** and ask them for it.
2. **Wait**: Do not proceed until you have a description.

# Step 3: Create Feature Branch
**Goal**: Set up the git branch and directory structure.

1. **Generate Short Name**: Analyze the description and create a concise 2-4 word name (action-noun, e.g., `add-user-auth`) as per Skill guidelines.
2. **Determine Feature Number**:
   - Run `git fetch --all --prune`
   - Run `git branch -a` to see existing branches.
   - Look for patterns like `NNN-short-name`.
   - If none found -> Number is `1`.
   - If found -> Number is `Highest + 1`.
3. **Execute Script**:
   - Run the creation script using `run_command`.
   - **Command**: `pwsh -ExecutionPolicy Bypass -File .agent/skills/speckit-specify/resources/scripts/powershell/create-new-feature.ps1 -Number <CALCULATED_NUMBER> -ShortName "<SHORT_NAME>" "<FEATURE_DESCRIPTION>"`
   - **Note**: Replace `<CALCULATED_NUMBER>`, `<SHORT_NAME>`, and `<FEATURE_DESCRIPTION>` with actual values.

# Step 4: Draft Specification
**Goal**: write the `spec.md` file.

1. **Locate File**: The script in Step 3 will have created a file at `specs/NNN-short-name/spec.md`.
2. **Read Template**: Read `.agent/skills/speckit-specify/resources/templates/spec-template.md` to understand the sections if the file is empty, or read the generated file.
3. **Apply Skill Strategy**:
   - **Parse**: Identify Actors, Actions, Data.
   - **No Tech Jargon**: Ensure Success Criteria are technology-agnostic.
   - **Measurable**: Ensure Criteria have numbers/times.
   - **Assumptions**: Fill in reasonable defaults (as per Skill guide).
4. **Write Content**: Use `write_to_file` (or `replace_file_content`) to populate the `spec.md`.

# Step 5: Quality Validation & Review
**Goal**: Ensure the spec meets the "Strict" quality standards before handling it to the user.

1. **Self-Review**: Compare your written spec against the "Specification Quality Validation" checklist in `SKILL.md`.
   - *Are there implementation details?* -> Remove them.
   - *Are requirements testable?* -> Fix them.
   - *Are there >3 Clarifications?* -> Prune them.
2. **Refine**: Update the file if necessary.
3. **Notify User**: Inform the user that the spec is ready for review. Provide the link to the file.
