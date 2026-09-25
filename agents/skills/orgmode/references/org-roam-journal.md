# org-roam journal files (`roam/journal/`)

`roam/journal/<year>.org` uses Org's `file+datetree` **week** format:
a **year → ISO week → day** hierarchy — *not* year → month → day. Get the
heading format wrong and it stops being a valid datetree (Emacs/org-roam
will create a second, parallel tree instead of finding the existing one).

**Do not write month-based headings (`2026-07 July`) into these files.**
Only ISO week headings (`2026-W27`) are correct. If you see month
headings in a journal file, that's a mistake to fix (rename to the
correct ISO week), not a pattern to continue.

## Heading hierarchy and exact format

```org
* 2026
** 2026-W27
*** 2026-07-03 Friday
:PROPERTIES:
:ID:       f43f6528-d1cd-452e-8752-40cbf50febc6
:END:
**** Some entry heading
:PROPERTIES:
:CREATED:  [2026-07-03 Fri 15:12]
:END:
  - content...
```

- **Level 1** — `* <year>`. One per calendar year, one file per year.
- **Level 2** — `** <ISO-year>-W<week>`, e.g. `** 2026-W27`. Week number
  is always 2 digits, zero-padded (`W01`, not `W1`). Near year
  boundaries the ISO week-year can differ from the calendar year (e.g.
  Dec 31 can fall in week 1 of the *next* year) — if placing a day near
  Jan 1/Dec 31, double-check the ISO week-year rather than assuming it
  matches the calendar year.
- **Level 3** — `*** <YYYY-MM-DD> <Weekday>`, using the plain calendar
  date (not the ISO week-year), with a `:PROPERTIES:` drawer holding a
  fresh `:ID:` (from `uuidgen`) — this is what makes the day heading an
  addressable org-roam node.
- **Level 4+** — whatever content goes under that day (e.g. a
  `**** Weekly review (...)` entry). These get their own `:CREATED:`
  timestamp, not `:ID:` (only the day heading is a linkable node).

## Keep chronological order

Headings must stay in date order — oldest first, newest last — at every
level:

- `**` week headings, top to bottom: oldest week first, most recent week
  at the bottom of the file.
- `***` day headings within a week: oldest day first.
- Entries (`****` and below) under a given day: typically append in the
  order they're written, but if backfilling multiple entries out of
  order, still order them so the file reads chronologically.

When inserting a new heading, **find where it belongs by date and insert
there** — do not assume "append at the end of the file" is correct; it's
only correct if the new date is genuinely the newest one present. This
matters most when backfilling a past day/week, or re-running a task for a
date out of the normal cadence. Heading dates sort lexicographically
(`YYYY-Www` and `YYYY-MM-DD` are both zero-padded), so check existing
`**`/`***` headings first and insert between the correct neighbors.

## How to edit

1. **Read the file first** to see existing week/day headings — don't
   guess the structure.
2. **Find or create the year heading** `* <year>` (create the file with
   a `:PROPERTIES:` / `:ID:` drawer above it if it doesn't exist yet,
   matching prior years' file header).
3. **Find or create the week heading** `** <ISO-year>-W<week>` for the
   target date, in the correct chronological position.
4. **Find or create the day heading** `*** <YYYY-MM-DD> <Weekday>` under
   that week, with a fresh `:ID:` via `uuidgen` if newly created, again
   in chronological order relative to sibling days.
5. **Add or update the entry** under the day heading. If an entry with
   the same purpose already exists for that day, update it in place —
   don't duplicate the heading.
6. **Use `Edit`, not a full-file rewrite** — target the specific
   insertion point so untouched parts of the file stay byte-for-byte
   identical.
