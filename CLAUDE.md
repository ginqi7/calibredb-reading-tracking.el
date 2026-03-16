# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

```sh
make test              # Install dependencies and run ERT tests
make solve-dependencies # Clone dependencies to ~/.emacs.d/lisp/
```

Tests run on Ubuntu with Emacs 28.1, 29.1, 30.1, release-snapshot, snapshot via GitHub Actions.

## Project Architecture

This is an Emacs Lisp package for tracking reading progress of books managed by CalibreDB. It uses SQLite (via EmacSQL) to persist reading logs and tracking data.

### Module Structure

```
calibredb-reading-tracking.el       ; Main entry point, interactive commands (start/finish/toggle log)
calibredb-reading-tracking-obj.el   ; EIEIO classes: crt:book, crt:tracking, crt:log
calibredb-reading-tracking-db.el    ; SQLite database layer with EmacSQL
calibredb-reading-tracking-utils.el ; Utility functions (UUID generation, duration computation)
```

### Dependency Chain

```
calibredb-reading-tracking.el
       └─ calibredb-reading-tracking-obj.el
            └─ calibredb-reading-tracking-db.el
                 └─ calibredb, emacsql, emacsql-sqlite
```

### Core Components

**Classes (crt:obj base class)**:
- `crt:book` - Represents a book with file, page, total-pages, tracking
- `crt:tracking` - Reading progress for a book (book-id, status, page, total-pages, timestamps)
- `crt:log` - Individual reading session (tracking-uuid, started/finished-at, page-from/to)

**Database Tables**:
- `reading-tracking` - Tracks reading progress per book (foreign key to books.id)
- `reading-logs` - Individual reading sessions (foreign key to reading-tracking.uuid)

**Main Interactive Functions**:
- `crt:start-log` - Start a reading session
- `crt:finish-log` - Finish current session
- `crt:toggle-log` - Toggle start/finish based on latest log state

## Development Notes

- Tests are in `tests/tests.el` and use ERT framework
- Dependencies are managed via `dependencies.txt` (one git URL per line) and installed by `dependencies.sh` to `~/.emacs.d/lisp/`
- The test loader (`tests/tests.el`) adds dependencies to load-path via `load-dependencies-path` helper
- Package requires external `calibredb` library (not included in this repo)
- All files use `lexical-binding: t`
