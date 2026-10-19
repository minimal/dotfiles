# `bin/nixpkgs-update-safe` — Code Review Notes

This file records issues identified during code review. Each entry is marked
**Open** (not yet addressed in source) or **Resolved** (fixed in
`bin/nixpkgs-update-safe`). Bead IDs track the follow-up fixes.

The four follow-up items were addressed on 2026-10-19. This was not a full
review of the native progress and JSON logging change in `80ce44c`.

## Resolved

### Commit failure rolls back a successful system switch — _resolved_

- **Location:** End of script — `git add flake.lock` / `git commit -m "$COMMIT_MSG"`
- **Issue:** If `hm-switch` succeeds but `git commit` fails (e.g., GPG signing
  failure, pre-commit hook rejection, missing author identity), the `ERR` trap
  ran `rollback()`. That restored the old `flake.lock`, re-ran `hm-switch` to
  revert the system, and added the new nixpkgs revision to the skip list —
  even though the system was actually working, so a good revision could be
  incorrectly marked bad.
- **Fix applied:** After a successful `hm-switch`, the script now sets
  `APPLIED=1` and disarms the `ERR` trap. A subsequent `git commit` failure
  leaves `flake.lock` staged and prints a manual-retry message instead of
  rolling back the applied system.

### `tee` failure in `run_with_progress` misclassifies success as failure — _resolved_

- **Location:** `run_with_progress()` — `PIPESTATUS` handling
- **Issue:** The return-code loop took the *first* non-zero `PIPESTATUS` entry.
  `PIPESTATUS[1]` is `tee`; if `tee` failed to write its log — exactly the
  "no space left on device" case the script tries to handle — a successful
  `nix flake update` was treated as failed and rolled back.
- **Fix applied:** `rc` is now derived from `pipe_status[0]` (the wrapped
  command) only. A `tee` log-write failure emits a warning but cannot override
  the command's status.

### No `INT`/`TERM`/`EXIT` trap; rollback relied solely on `ERR` — _resolved_

- **Location:** trap setup (formerly only `trap 'rollback; exit 1' ERR`)
- **Issue:** The `ERR` trap does not fire for `SIGINT`/`SIGTERM`, nor during
  the `set +e` window inside `run_with_progress` — precisely the long-running
  `nix flake update` / `hm-switch` stretch where a user is most likely to hit
  Ctrl-C. An interruption there left `flake.lock` modified and the backup file
  lying around with no rollback.
- **Fix applied:** Added `INT`/`TERM` traps and an `EXIT` trap that rolls back
  only when the system was never applied (`APPLIED=0`), rollback hasn't already
  run (`ROLLBACK_DONE=0`), and the backup differs from `flake.lock`.
  `rollback()` is now idempotent via `ROLLBACK_DONE`.

### Very narrow terminal width breaks the progress display — _resolved_

- **Bead:** `dotfiles-kz1`
- **Location:** `run_with_progress()` — `cols=$(($(tput cols 2>/dev/null || echo 80) - 4))`
- **Issue:** If the terminal width is 1–3 columns, `cols` becomes negative.
  Short output lines can cause a "substring expression < 0" error in the
  display pipeline. The update can still succeed despite the display error.
  At width 4, the zero-length substring hides output entirely.
- **Fix applied:** Clamp `cols` to a minimum of 1. Tests cover widths 1–4
  and 80, short and long lines, and successful and failed commands.

### `mktemp -t <template>` is non-portable — _resolved_

- **Bead:** `dotfiles-5ly`
- **Location:** `run_with_progress()`, `run_with_bar()`, and setup
- **Issue:** `mktemp -t TEMPLATE` means different things on different
  platforms. GNU coreutils treats `-t TEMPLATE` as a template in `$TMPDIR`
  (and `-t` is deprecated in favor of `--tmpdir`). On macOS, `-t` takes a
  *prefix*, not a template — the `XXXXXX` becomes a literal part of the
  prefix and macOS appends its own random suffix, yielding names like
  `/tmp/nixpkgs-update-safe.XXXXXX.abCdEf`. The `date -j` fallback in
  `format_date` implies macOS is supported, so this matters. It still
  produces a unique file (no crash), but the `XXXXXX` is misleading and the
  form is deprecated on Linux.
- **Fix applied:** Pass an absolute trailing template for all three log files,
  such as `mktemp "${TMPDIR:-/tmp}/nixpkgs-update-safe.XXXXXX"`. This avoids
  platform-specific flags and respects `TMPDIR`, including paths with spaces.
  Tests exercise GNU and macOS `mktemp`, cleanup, and the `/tmp` fallback.

### No preflight check for `nix` and `jq` — _resolved_

- **Bead:** `dotfiles-3a9`
- **Location:** Preflight checks
- **Issue:** The preflight loop checked `git`, `flake.lock`, and `just`, but
  not `nix` or `jq`, both of which are essential. Missing commands caused
  failures during revision reads or updates after the backup was taken.
- **Fix applied:** Added explicit `command -v` checks before setup. Missing
  commands now produce a clear error before any backup, log, or skip-list change.

### `eval` preflight checks are a maintenance footgun — _resolved_

- **Bead:** `dotfiles-k08`
- **Location:** Preflight checks
- **Issue:** The loop used `eval` on hardcoded strings. The strings contained
  no user input, so this was a maintenance concern, not a demonstrated
  command-injection vulnerability.
- **Fix applied:** Replaced the strings with direct checks and a dependency
  loop. Existing check order and error messages are preserved. The separate
  `eval` calls that restore shell options remain unchanged.

## Open

None of the recorded issues remain open.

## Regression checks

Run the isolated shell suite and lint checks:

```bash
bash scripts/nixpkgs-update-safe-test.sh
shellcheck bin/nixpkgs-update-safe scripts/nixpkgs-update-safe-test.sh
```

On macOS, also exercise the system `mktemp`:

```bash
MKTEMP_BIN=/usr/bin/mktemp bash scripts/nixpkgs-update-safe-test.sh
```

The suite mocks Git, Nix, and Just. It does not update this repository's lock
file, switch the system, or create commits.
