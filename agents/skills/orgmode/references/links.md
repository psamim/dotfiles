# Links and References

Org-mode supports various link types for internal and external references.

## Basic Link Syntax

### Link with Description

```org
[[URL][Description]]
```

Example:
```org
[[https://orgmode.org][Org Mode Homepage]]
```

### Link without Description

```org
[[URL]]
```

Example:
```org
[[https://orgmode.org]]
```

The URL itself will be displayed.

## External Links

### Web URLs

```org
[[https://example.com][Example Website]]
[[http://example.com/page][HTTP Link]]
```

### Email Addresses

```org
[[mailto:user@example.com][Email John]]
[[mailto:user@example.com]]
```

### File Paths

Absolute paths:
```org
[[file:/Users/name/Documents/report.pdf][Annual Report]]
[[file:/etc/config.conf][Config File]]
```

Relative paths:
```org
[[file:documents/notes.org][My Notes]]
[[file:../images/photo.jpg][Photo]]
[[file:./data.csv][Data]]
```

### File Links with Line Numbers

```org
[[file:script.py::25][Jump to line 25]]
[[file:code.js::function init][Jump to function]]
```

### File Links with Search

```org
[[file:notes.org::*Headline][Link to headline in file]]
[[file:doc.org::#custom-id][Link to custom ID]]
[[file:data.org::/regex search/][Search in file]]
```

## Internal Links

### Link to Headlines

Link to a headline by its text:

```org
* Introduction
Content here

* Main Content
See the [[Introduction]] section for background.
```

Or with description:
```org
[[Introduction][intro section]]
```

### Link to Custom IDs

If a headline has a CUSTOM_ID property:

```org
* Introduction
:PROPERTIES:
:CUSTOM_ID: intro
:END:

* Later Section
See [[#intro][the introduction]].
```

**Format**: `[[#custom-id][description]]`

### Link to Radio Targets

Define a radio target:
```org
<<<important concept>>>
```

Reference it anywhere:
```org
See the important concept section.
```

The phrase becomes an automatic link.

### Link to Dedicated Targets

Create a target:
```org
<<target-name>>
```

Link to it:
```org
[[target-name][Link to target]]
```

Or simple reference:
```org
[[target-name]]
```

## ID Links

Most stable link type for internal references:

```org
* Source Headline
:PROPERTIES:
:ID: 8f7a3c2e-9b1d-4e5f-a6c7-3d8e9f0a1b2c
:END:

Content here.

* Other Section
See [[id:8f7a3c2e-9b1d-4e5f-a6c7-3d8e9f0a1b2c][the source headline]].
```

**Advantages**:
- Works across file moves/renames
- Unique and stable
- Org-mode can find them automatically

**Format**: `[[id:UUID][description]]`

## Link Abbreviations

Define shortcuts for common link prefixes (file-level):

```org
#+LINK: wiki https://en.wikipedia.org/wiki/
#+LINK: bug https://github.com/org/repo/issues/

[[wiki:Org-mode][Org-mode on Wikipedia]]
[[bug:42][Issue #42]]
```

## Special Link Types

### Shell Links

Execute shell commands (use with caution):

```org
[[shell:ls -la][List files]]
```

### Elisp Links

Execute Emacs Lisp (Emacs only):

```org
[[elisp:(message "Hello")][Run code]]
```

### News Links

```org
[[news:comp.emacs][Emacs newsgroup]]
```

### DOI Links

```org
[[doi:10.1000/xyz123][Paper Title]]
```

### Help Links

```org
[[help:org-mode][Org Mode help]]
[[info:org#Links][Org manual on links]]
```

## Image Links

Display images inline:

```org
[[file:images/diagram.png]]
```

With description (still shows image):
```org
[[file:images/photo.jpg][My Photo]]
```

With attributes:
```org
#+ATTR_HTML: :width 300px
[[file:image.png]]
```

Supported formats: PNG, JPG, GIF, SVG (depends on viewer)

## Link Formatting

### Plain Links

URLs in angle brackets become clickable:

```org
<https://example.com>
<mailto:user@example.com>
```

### Bare URLs

Some viewers auto-detect bare URLs:

```org
Visit https://orgmode.org for more info.
```

**Note**: Not all viewers support this. Better to use `[[URL]]` format.

## Link Examples by Use Case

### Documentation Links

```org
* API Reference
See [[file:docs/api.org::*Authentication][Authentication docs]].

For examples, check [[file:examples/basic.py::10][example line 10]].

Full manual: [[https://docs.example.com][online documentation]].
```

### Cross-References

```org
* Chapter 1: Introduction
:PROPERTIES:
:CUSTOM_ID: ch1
:END:

Content here.

* Chapter 2: Main Content
As explained in [[#ch1][Chapter 1]], we begin with...
```

### File Organization

```org
* Project Files
** Documentation
- [[file:README.org][Project README]]
- [[file:docs/setup.org][Setup Guide]]
- [[file:docs/api.org][API Documentation]]

** Source Code
- [[file:src/main.py][Main Application]]
- [[file:src/utils.py][Utilities]]

** Resources
- [[file:images/architecture.png][Architecture Diagram]]
- [[file:data/config.json][Configuration]]
```

### Bibliography References

```org
* Research Notes
According to Smith et al. [[cite:smith2024][Smith 2024]], the results show...

* References
[[bibliography:~/Documents/references.bib]]
```

## Link Best Practices

### Use ID Links for Stability

**Good** (stable across moves):
```org
* Important Section
:PROPERTIES:
:ID: abc-123-def
:END:

* Reference
See [[id:abc-123-def][important section]].
```

**Avoid** (breaks if headline text changes):
```org
See [[Important Section]].
```

### Use Custom IDs for Human-Readable Links

**Good**:
```org
* Getting Started
:PROPERTIES:
:CUSTOM_ID: getting-started
:END:

See [[#getting-started][setup instructions]].
```

**Better than**:
```org
[[id:8f7a3c2e-9b1d-4e5f-a6c7-3d8e9f0a1b2c][setup instructions]]
```

### Use Relative Paths for Portability

**Good** (portable):
```org
[[file:docs/guide.org][User Guide]]
[[file:../assets/logo.png][Logo]]
```

**Avoid** (not portable):
```org
[[file:/Users/john/project/docs/guide.org][User Guide]]
```

### Provide Descriptive Link Text

**Good**:
```org
Read the [[file:installation.org][installation guide]] for setup instructions.
```

**Avoid**:
```org
Click [[file:installation.org][here]] for more info.
```

### Group Related Links

```org
* Resources
** Official Documentation
- [[https://orgmode.org/manual/][The Org Manual]]
- [[https://orgmode.org/guide/][Compact Guide]]

** Community
- [[https://reddit.com/r/orgmode][r/orgmode Subreddit]]
- [[https://lists.gnu.org/mailman/listinfo/emacs-orgmode][Mailing List]]
```

## Link Validation

**Check internal links**: Ensure Custom IDs and targets exist:

```org
* Target Section
:PROPERTIES:
:CUSTOM_ID: target
:END:

* Linking Section
[[#target][works]] ✓
[[#missing][broken]] ✗
```

**Check file links**: Verify file paths are correct and relative paths work from the org file's location.

**Check external links**: Periodically verify URLs are still valid.

## Breaking Links

Avoid these common issues:

**Spaces in bare links** (broken):
```org
[[file:my file.org][broken]]
```

**Fix with URL encoding** or use valid file names:
```org
[[file:my%20file.org][works]]
[[file:my-file.org][better]]
```

**Missing brackets**:
```org
[file:doc.org][broken]     # Wrong
[[file:doc.org][works]]     # Correct
```

**Mismatched brackets**:
```org
[[file:doc.org[broken]     # Wrong
[[file:doc.org][works]]     # Correct
```
