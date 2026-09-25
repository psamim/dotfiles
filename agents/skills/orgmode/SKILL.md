---
name: orgmode
description: |
  Org-mode syntax and formatting knowledge. Reference docs for headings, lists, links, properties, timestamps, and other org-mode constructs.

  Triggers: org-mode, .org files, org syntax, org formatting, org properties, org timestamps
---

# Org-mode Syntax Skill

Pure org-mode knowledge for writing and formatting `.org` files. No emacsclient or Emacs daemon required.

For general org-roam note CRUD (creating notes, searching, linking), use whatever note-management skill/tooling is available in the current session instead. For editing the `roam/journal/<year>.org` datetree files specifically (heading format, chronological ordering, how to insert entries), see **org-roam-journal.md** below — read it before writing to any file under `roam/journal/`.

## Quick Reference

### Headings

```org
* Top-level heading
** Second level
*** Third level
```

### Text Formatting

```org
*bold* /italic/ _underline_ ~code~ =verbatim= +strikethrough+
```

### Lists

```org
- Unordered item
  - Nested item
1. Ordered item
2. Second item
- [ ] Checkbox unchecked
- [X] Checkbox checked
```

### Links

```org
[[https://example.com][Description]]
[[file:path/to/file.org][File link]]
[[id:uuid-here][ID link]]
```

### Properties

```org
:PROPERTIES:
:ID:       some-uuid
:CUSTOM:   value
:END:
```

### Keywords

```org
#+TITLE: Document Title
#+FILETAGS: :tag1:tag2:
#+DATE: [2026-03-01 Sun]
#+AUTHOR: Name
```

### Timestamps

```org
<2026-03-01 Sun>          Active timestamp
[2026-03-01 Sun]          Inactive timestamp
<2026-03-01 Sun 10:00>    With time
SCHEDULED: <2026-03-01 Sun>
DEADLINE: <2026-03-05 Thu>
```

### Source Blocks

```org
#+BEGIN_SRC python
def hello():
    print("Hello")
#+END_SRC
```

### Tables

```org
| Name  | Value |
|-------+-------|
| Alice |    42 |
| Bob   |    17 |
```

## Paragraphs and Line Wrapping

Do **not** hard-wrap prose at a fixed column width. Write each paragraph as
one long single line — long lines are fine — and let the editor soft-wrap it
for display. To start a new paragraph, use a blank line (i.e. a real newline
between paragraphs), not a wrapped line inside the same paragraph.

- One paragraph = one line. Reflow wrapped text back into a single line.
- New paragraph = one blank line separating it from the previous.
- This applies to body prose. List items, table rows, source blocks, and
  property drawers keep their own line structure as usual.

## Tag Constraints

Org-mode tags **cannot contain hyphens**. Use underscores instead:
- Invalid: `my-tag`, `web-dev`
- Valid: `my_tag`, `web_dev`

## Detailed References

- **org-syntax.md** - Complete org-mode syntax reference
- **properties.md** - Property drawers, node properties, and inheritance
- **timestamps.md** - Date/time formats, scheduling, deadlines, repeaters
- **links.md** - Internal links, external links, ID links, file links
- **examples.md** - Common formatting patterns and best practices
- **org-roam-journal.md** - `roam/journal/<year>.org` datetree format (year → ISO week → day), chronological-order rules, and how to edit these files
