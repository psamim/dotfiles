# Timestamps and Scheduling

Org-mode provides powerful timestamp and scheduling features for time tracking and planning.

## Basic Timestamps

### Active Timestamps

Active timestamps appear in agenda views:

```org
<2025-12-13 Fri>
```

With time:
```org
<2025-12-13 Fri 14:30>
```

With time range:
```org
<2025-12-13 Fri 14:30-16:00>
```

**Format**: `<YYYY-MM-DD Day>` or `<YYYY-MM-DD Day HH:MM>`

**Day abbreviations**: Mon, Tue, Wed, Thu, Fri, Sat, Sun

### Inactive Timestamps

Inactive timestamps (for notes/metadata, don't appear in agenda):

```org
[2025-12-13 Fri]
```

With time:
```org
[2025-12-13 Fri 14:30]
```

**Format**: `[YYYY-MM-DD Day]` or `[YYYY-MM-DD Day HH:MM]`

**Use cases**:
- Document creation/modification dates
- Metadata timestamps
- Historical records

## Date and Time Ranges

### Date Range

```org
<2025-12-13 Fri>--<2025-12-15 Sun>
```

### Time Range on Same Day

```org
<2025-12-13 Fri 09:00-17:00>
```

### Multiple Timestamps

```org
* Conference
<2025-06-15 Sun 09:00-10:00>
<2025-06-15 Sun 14:00-15:00>
```

## Scheduled and Deadline

### SCHEDULED

Marks when you plan to start working on a task:

```org
* TODO Write report
SCHEDULED: <2025-12-15 Mon>
```

With time:
```org
* TODO Team meeting
SCHEDULED: <2025-12-15 Mon 10:00>
```

### DEADLINE

Marks when task must be completed:

```org
* TODO Submit proposal
DEADLINE: <2025-12-20 Fri>
```

### Both SCHEDULED and DEADLINE

```org
* TODO Complete project
SCHEDULED: <2025-12-10 Tue>
DEADLINE: <2025-12-20 Fri>
```

**Format**: Must be immediately after headline (or after properties drawer):

```org
* TODO Task
:PROPERTIES:
:ID: abc123
:END:
SCHEDULED: <2025-12-15 Mon>
DEADLINE: <2025-12-20 Fri>

Task description goes here.
```

## Repeating Timestamps

### Basic Repeaters

Repeat every day:
```org
* TODO Daily standup
SCHEDULED: <2025-12-13 Fri 09:00 +1d>
```

Repeat every week:
```org
* TODO Weekly review
SCHEDULED: <2025-12-13 Fri +1w>
```

Repeat every month:
```org
* TODO Monthly report
DEADLINE: <2025-12-31 Wed +1m>
```

Repeat every year:
```org
* TODO Annual review
SCHEDULED: <2025-12-31 Wed +1y>
```

### Repeater Formats

- `+1d` - daily
- `+2d` - every 2 days
- `+1w` - weekly
- `+2w` - bi-weekly
- `+1m` - monthly
- `+3m` - quarterly
- `+1y` - yearly

### Repeater Types

**Cumulative repeater** (`+`): Next occurrence from last completion date
```org
SCHEDULED: <2025-12-13 Fri +1w>
```

**Catch-up repeater** (`++`): Next occurrence from original date
```org
SCHEDULED: <2025-12-13 Fri ++1w>
```

**Restart repeater** (`.+`): Next occurrence from today
```org
SCHEDULED: <2025-12-13 Fri .+1w>
```

**Example differences**:
- If task was due Dec 13 but you complete it on Dec 20:
  - `+1w`: Next date is Dec 20 + 1 week = Dec 27
  - `++1w`: Next date is Dec 13 + 1 week = Dec 20 (catches up)
  - `.+1w`: Next date is today (Dec 20) + 1 week = Dec 27

## Delay and Warning

### Delay

Don't show in agenda until N days before:

```org
* TODO Task
SCHEDULED: <2025-12-20 Fri -3d>
```

Shows in agenda 3 days before (Dec 17).

### Warning Period

Show warning N days before deadline:

```org
* TODO Submit report
DEADLINE: <2025-12-20 Fri -5d>
```

Warning appears 5 days before deadline.

### Combined

```org
* TODO Important deadline
SCHEDULED: <2025-12-10 Tue -2d>
DEADLINE: <2025-12-20 Fri -7d>
```

## Time Duration and Effort

### Effort Estimates

Store in properties:

```org
* TODO Development task
:PROPERTIES:
:EFFORT: 3:30
:END:
```

**Format**: `H:MM` or `HH:MM`

### Clocking Time

Track time spent (Emacs-specific):

```org
* TODO Task
:LOGBOOK:
CLOCK: [2025-12-13 Fri 09:00]--[2025-12-13 Fri 11:30] =>  2:30
CLOCK: [2025-12-13 Fri 14:00]--[2025-12-13 Fri 16:00] =>  2:00
:END:
```

### Time Ranges in Headings

For events:

```org
* Meeting <2025-12-13 Fri 14:00-15:00>
* Conference <2025-06-15 Sun>--<2025-06-17 Tue>
```

## Date/Time Stamp Examples

### Meeting

```org
* TODO Team Sync
SCHEDULED: <2025-12-14 Sat 10:00-11:00>
:PROPERTIES:
:LOCATION: Conference Room A
:END:

Agenda items:
- Project status
- Next sprint planning
```

### Project with Milestones

```org
* PROJECT Website Redesign
SCHEDULED: <2025-01-15 Wed>
DEADLINE: <2025-03-31 Mon>

** TODO Design phase
SCHEDULED: <2025-01-15 Wed>
DEADLINE: <2025-01-31 Fri>

** TODO Development phase
SCHEDULED: <2025-02-01 Sat>
DEADLINE: <2025-03-15 Sat>

** TODO Testing phase
SCHEDULED: <2025-03-16 Sun>
DEADLINE: <2025-03-31 Mon>
```

### Recurring Tasks

```org
* TODO Daily standup
SCHEDULED: <2025-12-13 Fri 09:00 +1d>
:PROPERTIES:
:EFFORT: 0:15
:END:

* TODO Weekly planning
SCHEDULED: <2025-12-16 Mon 09:00 +1w>

* TODO Monthly review
SCHEDULED: <2025-12-31 Wed +1m>

* TODO Quarterly goals
SCHEDULED: <2025-12-31 Wed +3m>
```

### Document Metadata

```org
* Meeting Notes
:PROPERTIES:
:CREATED: [2025-12-13 Fri 14:00]
:LAST_MODIFIED: [2025-12-13 Fri 16:30]
:ATTENDEES: Alice, Bob, Charlie
:END:

Meeting held on <2025-12-13 Fri 14:00-15:30>

Next meeting: <2025-12-20 Fri 14:00>
```

## Timestamp Placement

### After Headline

```org
* Meeting <2025-12-13 Fri 10:00>
```

### After Properties

```org
* TODO Task
:PROPERTIES:
:ID: abc123
:END:
SCHEDULED: <2025-12-15 Mon>

Content here.
```

### In Content

```org
* Project Notes
The project started on <2025-01-15 Wed> and is scheduled to
complete by <2025-03-31 Mon>.

Last reviewed: [2025-12-13 Fri]
```

### In Lists

```org
* Milestones
- [ ] Design completion <2025-01-31 Fri>
- [ ] Development done <2025-03-15 Sat>
- [ ] Launch <2025-03-31 Mon>
```

## Common Patterns

### Task with Reminder

```org
* TODO Submit tax return
DEADLINE: <2025-04-15 Tue -14d>
```

Shows warning 14 days before (Apr 1).

### Flexible Recurring Task

```org
* TODO Exercise
SCHEDULED: <2025-12-13 Fri .+2d>
:PROPERTIES:
:EFFORT: 1:00
:END:
```

Reschedules to 2 days from completion (not original date).

### Event Series

```org
* Course: Org Mode Mastery
<2025-06-01 Sun 10:00 +1w>--<2025-08-24 Sun 11:30 +1w>

Weekly course every Sunday for 12 weeks.
```

### Multiple Deadlines

```org
* TODO Research Paper
** TODO Draft
DEADLINE: <2025-02-15 Sat>

** TODO Peer review
DEADLINE: <2025-02-28 Fri>

** TODO Final submission
DEADLINE: <2025-03-15 Sat>
```

## Timestamp Best Practices

### Use Active vs Inactive Appropriately

**Active** (for scheduling):
```org
* TODO Meeting <2025-12-15 Mon 10:00>
```

**Inactive** (for metadata):
```org
:PROPERTIES:
:CREATED: [2025-12-13 Fri]
:END:
```

### Include Day of Week

Always include day abbreviation:

**Good**: `<2025-12-15 Mon>`
**Avoid**: `<2025-12-15>`

### Use SCHEDULED and DEADLINE Correctly

- **SCHEDULED**: When you plan to *start*
- **DEADLINE**: When it must be *finished*

### Provide Warning for Important Deadlines

```org
* TODO Critical deadline
DEADLINE: <2025-12-31 Wed -7d>
```

### Use Appropriate Repeater Types

- **+**: Most tasks (repeats from completion)
- **++**: Fixed schedules (meetings, appointments)
- **.+**: Flexible habits (exercise, reading)

## Invalid Timestamp Formats

**Avoid these**:

```org
<12-13-2025>              # Wrong date format
<2025/12/13>              # Wrong separators
<2025-12-13 25:00>        # Invalid hour
<2025-13-01>              # Invalid month
<2025-12-32>              # Invalid day
[2025-12-13 Friday]       # Wrong day format (use Fri)
```

**Correct formats**:

```org
<2025-12-13 Fri>
<2025-12-13 Fri 14:30>
<2025-12-13 Fri 14:30-16:00>
[2025-12-13 Fri]
```
