# Org-Mode Core Syntax Reference

This reference covers the essential syntax elements of org-mode files.

## Headlines

Headlines create the document structure using asterisks (`*`). The number of asterisks indicates the level:

```org
* Top-level headline
** Second-level headline
*** Third-level headline
**** Fourth-level headline
```

### Headlines with Metadata

Headlines can include TODO keywords, priority, tags, and titles:

```org
* TODO [#A] Important task :work:urgent:
** DONE Completed task :project:
*** Meeting with team :meeting:
```

**Format**: `*+ [TODO-keyword] [priority] Title :tag1:tag2:`

- TODO keywords: `TODO`, `DONE`, or custom states
- Priority: `[#A]`, `[#B]`, `[#C]` (A is highest)
- Tags: `:tag1:tag2:` at end of line (no spaces between colons)

## Plain Lists

### Unordered Lists

Use `-`, `+`, or `*` (with space after):

```org
- First item
- Second item
  - Nested item
  - Another nested item
- Third item
```

### Ordered Lists

Use `1.` or `1)` format:

```org
1. First step
2. Second step
3. Third step
```

Or with parentheses:

```org
1) First option
2) Second option
3) Third option
```

### Description Lists

Use `::` to separate term from description:

```org
- Term 1 :: Description of term 1
- Term 2 :: Description of term 2
- Longer term :: This is a longer description
  that can span multiple lines
```

### Checkboxes

Add checkboxes to list items:

```org
- [ ] Incomplete task
- [X] Complete task
- [-] Partially complete (some sub-items done)
  - [X] Sub-task 1
  - [ ] Sub-task 2
```

## Text Emphasis

```org
*bold text*
/italic text/
_underlined text_
=verbatim text=
~code~
+strikethrough+
```

**Important**: Emphasis markers must have no space immediately after opening or before closing.

## Blocks

### Source Code Blocks

```org
#+BEGIN_SRC python
def hello():
    print("Hello, world!")
#+END_SRC
```

With language-specific highlighting:

```org
#+BEGIN_SRC javascript
function greet(name) {
  console.log(`Hello, ${name}!`);
}
#+END_SRC
```

### Example Blocks

For literal text without syntax highlighting:

```org
#+BEGIN_EXAMPLE
This is an example block.
All text is displayed as-is.
No syntax highlighting.
#+END_EXAMPLE
```

### Quote Blocks

```org
#+BEGIN_QUOTE
This is a quotation.
It will be formatted as a quote.
#+END_QUOTE
```

### Verse Blocks

For poetry or text where line breaks matter:

```org
#+BEGIN_VERSE
  Roses are red,
  Violets are blue,
  Org-mode is great,
  And so are you!
#+END_VERSE
```

### Center Blocks

```org
#+BEGIN_CENTER
This text will be centered.
#+END_CENTER
```

## Drawers

Drawers hide metadata and properties:

```org
* Headline with drawer
:PROPERTIES:
:CUSTOM_ID: my-id
:END:

:LOGBOOK:
- State "DONE"       from "TODO"       [2025-12-13 Fri 10:30]
:END:
```

**Format**: `:DRAWER_NAME:` on its own line, `:END:` to close.

Most common drawers:
- `:PROPERTIES:` - For properties (see properties.md)
- `:LOGBOOK:` - For state change logs
- Custom drawers for your own metadata

## Tables

Tables use `|` for columns:

```org
| Name    | Age | City          |
|---------+-----+---------------|
| Alice   |  30 | San Francisco |
| Bob     |  25 | New York      |
| Charlie |  35 | London        |
```

**Auto-alignment**: Press TAB in Emacs to auto-align columns.

Simple table:

```org
| Column 1 | Column 2 | Column 3 |
|----------+----------+----------|
| A        | B        | C        |
| D        | E        | F        |
```

## Comments

### Line Comments

Lines starting with `#` (but not `#+`) are comments:

```org
# This is a comment
This is regular text
# Another comment
```

### Inline Comments

Not supported - org-mode doesn't have inline comments within text.

### Comment Blocks

```org
#+BEGIN_COMMENT
This entire block is commented out.
Multiple lines can be commented.
Useful for drafts or notes.
#+END_COMMENT
```

## Horizontal Rules

Five or more dashes create a horizontal line:

```org
Some text above

-----

Some text below
```

## Special Keywords

Document-level keywords use `#+KEYWORD: value` format:

```org
#+TITLE: My Document Title
#+AUTHOR: John Doe
#+DATE: 2025-12-13
#+EMAIL: john@example.com
#+LANGUAGE: en
#+OPTIONS: toc:2 num:nil
#+STARTUP: overview
#+FILETAGS: :documentation:org:
```

Common keywords:
- `#+TITLE:` - Document title
- `#+AUTHOR:` - Author name
- `#+DATE:` - Document date
- `#+FILETAGS:` - File-level tags
- `#+STARTUP:` - View options (overview, showall, etc.)
- `#+OPTIONS:` - Export options

## Footnotes

```org
This is text with a footnote[fn:1].

Another reference to a footnote[fn:named].

[fn:1] This is the first footnote.
[fn:named] This is a named footnote.
```

Inline footnotes:

```org
This is text with an inline footnote[fn:: This is the footnote text.].
```

## Line Breaks

- Single newline: Treated as space
- Empty line: Creates new paragraph
- `\\` at end of line: Force line break

```org
This is one line\\
This appears on the next line

This is a new paragraph.
```

## Indentation

**Important**: Indentation matters for nested lists but not for text flow.

```org
- First level item
  - Second level item (indented with 2 spaces)
    - Third level item (indented with 4 spaces)
```

Text under headlines:

```org
* Headline
This text belongs to the headline.
It doesn't need special indentation.

** Sub-headline
Text for the sub-headline.
```

## Escaping Special Characters

Use a zero-width space or backslash to escape:

```org
Use \*this\* to show literal asterisks
Use \_this\_ to show literal underscores
```

## Affiliated Keywords

Keywords that apply to the next element:

```org
#+NAME: figure-1
#+CAPTION: This is a caption for the figure
#+ATTR_HTML: :width 500px
[[file:image.png]]
```

Common affiliated keywords:
- `#+NAME:` - Name for reference
- `#+CAPTION:` - Caption for figures/tables
- `#+ATTR_HTML:` - HTML export attributes
- `#+ATTR_LATEX:` - LaTeX export attributes
