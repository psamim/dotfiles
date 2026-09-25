# Properties and Metadata Reference

Properties provide structured metadata for org-mode headlines and files.

## Properties Drawer Syntax

Properties are stored in a `:PROPERTIES:` drawer immediately after a headline:

```org
* Headline Title
:PROPERTIES:
:CUSTOM_ID: unique-identifier
:CATEGORY: project
:EFFORT:   2:00
:END:

Content goes here after the properties drawer.
```

**Format rules**:
- Drawer must start immediately after headline (no blank lines)
- Property format: `:KEY: value` (colon-key-colon space value)
- Keys are case-insensitive but typically UPPERCASE
- Drawer closes with `:END:`
- Content starts after the `:END:` line

## Common Standard Properties

### ID

Unique identifier for the entry (typically UUID):

```org
* My Entry
:PROPERTIES:
:ID: 8f7a3c2e-9b1d-4e5f-a6c7-3d8e9f0a1b2c
:END:
```

Use for stable references across moves/renames. Generate with `uuidgen` or `uuid.uuid4()` in Python.

### CUSTOM_ID

Human-readable ID for internal linking:

```org
* Introduction
:PROPERTIES:
:CUSTOM_ID: intro
:END:
```

Link to this with `[[#intro]]`.

### CATEGORY

Categorizes the entry:

```org
* Task
:PROPERTIES:
:CATEGORY: work
:END:
```

Appears in agenda views. Inherits to sub-trees unless overridden.

### EFFORT

Estimated time/effort:

```org
* Development Task
:PROPERTIES:
:EFFORT: 3:30
:END:
```

Format: `H:MM` or `H:MM:SS`. Used for time tracking and planning.

### ORDERED

Forces sequential completion of sub-tasks:

```org
* Project
:PROPERTIES:
:ORDERED: t
:END:
** TODO Step 1
** TODO Step 2
** TODO Step 3
```

Value `t` means children must be completed in order.

### PRIORITY

Alternative to `[#A]` priority cookie:

```org
* Task
:PROPERTIES:
:PRIORITY: A
:END:
```

Usually better to use priority cookies in headline: `* TODO [#A] Task`

### CREATED

Creation timestamp:

```org
* New Entry
:PROPERTIES:
:CREATED: [2025-12-13 Fri 14:30]
:END:
```

Use inactive timestamps `[...]` for metadata that shouldn't trigger agenda.

### LAST_MODIFIED

Last modification time:

```org
* Document
:PROPERTIES:
:LAST_MODIFIED: [2025-12-13 Fri 15:45]
:END:
```

## Custom Properties

You can define any custom properties:

```org
* Book
:PROPERTIES:
:TITLE: The Org Manual
:AUTHOR: Carsten Dominik
:YEAR: 2025
:ISBN: 1234567890
:RATING: 5
:STATUS: reading
:END:
```

**Naming conventions**:
- Use UPPERCASE for property keys
- Use underscores or hyphens for multi-word keys
- Examples: `PROJECT_STATUS`, `DUE_DATE`, `ASSIGNED_TO`

## Property Inheritance

Properties can inherit from parent headlines to children:

```org
* Project Alpha
:PROPERTIES:
:CATEGORY: work
:CLIENT: Acme Corp
:END:

** Task 1
Task 1 inherits CATEGORY:work and CLIENT:Acme Corp

** Task 2
:PROPERTIES:
:CATEGORY: urgent
:END:

Task 2 overrides CATEGORY but still inherits CLIENT
```

Properties that commonly inherit:
- `CATEGORY`
- `ARCHIVE`
- Custom properties (configurable in Emacs)

Properties that don't inherit:
- `ID`
- `CUSTOM_ID`

## Column View Properties

Special properties for column view display:

```org
* Project
:PROPERTIES:
:COLUMNS: %25ITEM %TODO %3PRIORITY %TAGS %EFFORT
:END:
```

This defines how properties display in column view (Emacs-specific).

## File-Level Properties

Properties for the entire file (at the very top):

```org
#+PROPERTY: AUTHOR John Doe
#+PROPERTY: PROJECT Alpha
#+PROPERTY: CATEGORY work

* First headline
This inherits file-level properties
```

## Special Property Values

### Dates

Use org timestamps:

```org
:PROPERTIES:
:DEADLINE_DATE: <2025-12-31 Tue>
:REVIEW_DATE: [2025-06-01 Sun]
:END:
```

### Lists

Properties are single-value, but you can use delimiters:

```org
:PROPERTIES:
:TAGS_LIST: project, important, work
:CONTRIBUTORS: Alice, Bob, Charlie
:END:
```

### Numbers

Store as plain values:

```org
:PROPERTIES:
:BUDGET: 10000
:PROGRESS: 75
:VERSION: 2.1
:END:
```

### Boolean

Use `t` for true, `nil` for false:

```org
:PROPERTIES:
:COMPLETED: t
:REVIEWED: nil
:PUBLISHED: t
:END:
```

## Property Drawer Positioning

**Critical**: Properties drawer must be the first drawer and immediately follow the headline:

**Correct**:
```org
* Headline
:PROPERTIES:
:ID: abc123
:END:

Content here.
```

**Incorrect** (blank line):
```org
* Headline

:PROPERTIES:
:ID: abc123
:END:
```

**Incorrect** (content before properties):
```org
* Headline
Some content first.
:PROPERTIES:
:ID: abc123
:END:
```

## Multiple Drawers

You can have multiple drawers, but PROPERTIES must be first:

```org
* Headline
:PROPERTIES:
:ID: abc123
:END:

:LOGBOOK:
- State "DONE"       from "TODO"       [2025-12-13 Fri]
:END:

:CUSTOM_DRAWER:
Custom data here
:END:

Content goes here.
```

## When to Use Properties

Use properties when you need:
- **Unique identifiers**: For stable references (ID, CUSTOM_ID)
- **Structured metadata**: For querying/filtering (custom properties)
- **Time tracking**: For effort estimates and logging
- **Categorization**: For organizing and agenda views
- **Programmatic access**: When scripts need to read/write metadata

Don't use properties for:
- **Tags**: Use `:tags:` instead
- **TODO states**: Use TODO keywords
- **Priorities**: Use `[#A]` priority cookies
- **Timestamps**: Use SCHEDULED/DEADLINE for scheduling

## Property Search and Queries

Properties enable powerful searches (Emacs-specific):

Search for entries where `STATUS` is `active`:
```
:STATUS: "active"
```

Search for entries with `PRIORITY` A or B:
```
:PRIORITY:>="A"&:PRIORITY:<="B"
```

## Example: Complete Property Usage

```org
* Project: Website Redesign
:PROPERTIES:
:ID: 1f8e3c2a-4b5d-6e7f-8a9b-0c1d2e3f4a5b
:CUSTOM_ID: website-redesign
:CATEGORY: projects
:CLIENT: Acme Corporation
:BUDGET: 50000
:START_DATE: <2025-01-15 Wed>
:DUE_DATE: <2025-03-31 Mon>
:PROGRESS: 35
:STATUS: in-progress
:ASSIGNED_TO: Alice, Bob
:EFFORT: 80:00
:CREATED: [2025-01-10 Fri 09:00]
:LAST_MODIFIED: [2025-12-13 Fri 16:00]
:END:

Project description and notes go here.

** TODO Design mockups
:PROPERTIES:
:ID: 2a9f4d3b-5c6e-7f8a-9b0c-1d2e3f4a5b6c
:EFFORT: 16:00
:ASSIGNED_TO: Alice
:END:

** TODO Frontend development
:PROPERTIES:
:ID: 3b0a5e4c-6d7f-8a9b-0c1d-2e3f4a5b6c7d
:EFFORT: 40:00
:ASSIGNED_TO: Bob
:END:
```
