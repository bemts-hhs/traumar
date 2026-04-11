---
name: Speckit Specify
description: Professional Spec-Driven Development (SDD) Architect skill for defining feature specifications.
---

# Speckit Specify Skill

## Purpose
This skill equips you with the knowledge and resources to act as a **Spec-Driven Development Architect**. Your goal is to transform vague feature descriptions into rigorous, testable, and business-focused specifications.

## Resources
These resources are located in the skill's directory:
- **Scripts**: `.agent/skills/speckit-specify/resources/scripts/`
- **Templates**: `.agent/skills/speckit-specify/resources/templates/`
- **Spec Template**: `.agent/skills/speckit-specify/resources/templates/spec-template.md`
- **Tasks Template**: `.agent/skills/speckit-specify/resources/templates/tasks-template.md`
- **Checklist Template**: `.agent/skills/speckit-specify/resources/templates/checklist-template.md`
- **Constitution**: `.agent/skills/speckit-specify/resources/memory/constitution.md`

## 1. Feature Naming Convention (Short Name)
When asked to generate a short name for a feature:
- use **action-noun** format (e.g., `add-user-auth`, `fix-payment-bug`).
- keep it **2-4 words**.
- preserve technical terms (e.g., `oauth2`, `jwt`).
- ensure it is concise but descriptive.

## 2. Specification Drafting Strategy
When writing a specification from a user prompt, follow this mental process:

1.  **Parse & Extract**: Identify Actors, Actions, Data, and Constraints.
2.  **Make Informed Guesses**: Fill in details using context and industry standards. Document these in the **Assumptions** section.
3.  **Strict Clarification Limit**:
    - Max **3** `[NEEDS CLARIFICATION]` markers total.
    - Only ask about: Scope, Security/Privacy, or High-Impact UX.
    - *Prioritize Scope > Security > UX > Technical Details.*
4.  **Functional Requirements**:
    - Must be testable.
    - Use reasonable defaults for unspecified details.
5.  **Success Criteria**:
    - **Measurable**: Time, percentage, count.
    - **Technology-agnostic**: No frameworks/DB names.
    - **User-focused**: "Users can..." not "API returns..."

### Success Criteria Examples
- ✅ GOOD: "Users can complete checkout in under 3 minutes"
- ❌ BAD: "API response time is under 200ms"

## 3. Specification Quality Validation
Before finalizing a spec, you must validate it against these criteria.

### Content Quality
- [ ] No implementation details (languages, frameworks).
- [ ] Written for business stakeholders, not devs.
- [ ] Focused on User Value.

### Requirement Completeness
- [ ] No `[NEEDS CLARIFICATION]` markers remain (or max 3 critical ones).
- [ ] Requirements are unambiguous.
- [ ] Success criteria are measurable and tech-agnostic.
- [ ] Edge cases identified.

### Verification Logic
- If items **FAIL**: List specific issues, update the spec, and re-validate (Mental Loop, max 3 iterations).
- If **NEEDS CLARIFICATION (>3)**: Prune to top 3, guess the rest.
- If **NEEDS CLARIFICATION (<=3)**: Present questions to user using the "Question Table" format defined in standard SDD practices.

## 4. Question Table Format (For Clarifications)
If you must ask the user (max 3 questions), use this format:

```markdown
## Question [N]: [Topic]
**Context**: [Quote spec]
**We need to know**: [Question]
**Suggested Answers**:
| Option | Answer | Implications |
|--------|--------|--------------|
| A      | ...    | ...          |
| B      | ...    | ...          |
```

## 5. Checklist Strategy ("Unit Tests for Requirements")
When generating checklists, treat them as **Unit Tests for English requirements**.
- **NOT for verification**: Do NOT write "Verify button works".
- **FOR requirements validation**: Write "Are visual hierarchy requirements defined?".
- **Core Principle**: Test the Requirements, Not the Implementation.

### Correct Pattern ✅
- "Is 'fast loading' quantified with specific timing thresholds? [Clarity]"
- "Are error handling requirements defined for all API failure modes? [Completeness]"
- "Are requirements consistent between landing and detail pages? [Consistency]"

### Incorrect Pattern ❌
- "Test page load speed" (Implementation test)
- "Check if API returns 500" (QA test)

## 6. Task Generation Rules (Strict Checklist)
When generating `tasks.md`, you must enforce this STRICT format:

`- [ ] [TaskID] [P?] [Story?] Description with file path`

### Components
1. **Checkbox**: `-[ ]`
2. **Task ID**: `T001`, `T002`...
3. **[P]**: Parallel marker (optional).
4. **[Story]**: `[US1]`, `[US2]` (Required for story phases).
5. **Description**: Clear action + **File Path**.

### Organization Strategy
- **Phase 1: Setup**: Infrastructure & Config.
- **Phase 2: Foundations**: Blocking prerequisites.
- **Phase 3+: User Stories**: Implement by story (Story 1 -> Story 2).
  - Inside Story: Models -> Services -> API/UI -> Tests.
- **Final Phase**: Polish.

## 7. Analysis & Planning Heuristics
- **Analysis**: Be token-efficient. Do not read entire files if unnecessary. Look for "Constitution Violations" (Critical) and "Requirement Gaps".
- **Planning**:
  - **Phase 0 (Research)**: Resolve `NEEDS CLARIFICATION` via research.
  - **Phase 1 (Design)**: Define Data Models & API Contracts.
  - **Phase 2 (Tasks)**: Break down into implementation steps.
