# `bin/nixpkgs-update-safe` — Code Review Notes

This file records issues identified during code review. Each entry is marked
**Open** (not yet addressed in source) or **Resolved** (fixed in
`bin/nixpkgs-update-safe`; see the referenced commit).

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

## Open

### Very narrow terminal width crashes the script — _open, Low_

- **Location:** `run_with_progress()` — `cols=$(($(tput cols 2>/dev/null || echo 80) - 4))`
- **Issue:** If the terminal width is 1–3 columns, `cols` becomes negative.
  Bash then fails on `${line:0:$cols}` with a "substring expression < 0"
  error, which triggers `set -e` and aborts the update.
- **Fix if addressed:** Clamp `cols` to a minimum of 1:

  ```bash
  cols=$(($(tput cols 2>/dev/null || echo 80) - 4))
  (( cols < 1 )) && cols=1
  ```

### `mktemp -t <template>` is non-portable — _open, Low_

- **Location:** `run_with_progress()` and setup — `mktemp -t nixpkgs-update-safe.XXXXXX`
- **Issue:** `mktemp -t TEMPLATE` means different things on different
  platforms. GNU coreutils treats `-t TEMPLATE` as a template in `$TMPDIR`
  (and `-t` is deprecated in favor of `--tmpdir`). On macOS, `-t` takes a
  *prefix*, not a template — the `XXXXXX` becomes a literal part of the
  prefix and macOS appends its own random suffix, yielding names like
  `/tmp/nixpkgs-update-safe.XXXXXX.abCdEf`. The `date -j` fallback in
  `format_date` implies macOS is supported, so this matters. It still
  produces a unique file (no crash), but the `XXXXXX` is misleading and the
  form is deprecated on Linux.
- **Fix if addressed:** Use the trailing-template form with `-p`, which both
  GNU and macOS accept:

  ```bash
  mktemp -p "${TMPDIR:-/tmp}" nixpkgs-update-safe.XXXXXX
  ```

### No preflight check for `nix` and `jq` — _open, Low_

- **Location:** Preflight loop
- **Issue:** The preflight loop checks `git`, `flake.lock`, and `just`, but
  not `nix` or `jq`, both of which are essential. If `jq` is missing,
  `get_nixpkgs_rev` returns empty and the script proceeds with empty revs;
  if `nix` is missing, the failure surfaces only inside `run_with_progress`
  after the backup is already taken. Failing fast with a clear message is
  cheaper and clearer than a rollback cascade.
- **Fix if addressed:** Add `command -v` checks to the preflight loop:

  ```bash
  "command -v nix &>/dev/null || { print_error 'nix not found'; exit 1; }"
  "command -v jq &>/dev/null || { print_error 'jq not found'; exit 1; }"
  ```

### `eval` preflight checks are a maintenance footgun — _open, Minor_

- **Location:** Preflight loop — `for check in ...; do eval "$check"; done`
- **Issue:** The loop uses `eval` on hardcoded strings. There is no current
  command-injection risk because the strings contain no user input, but the
  pattern makes future edits error-prone and easy to misquote. (The code
  review agent explicitly dismissed this as a non-issue since the eval'd
  strings are static literals, but it's recorded here as a style/maintenance
  concern.)
- **Fix if addressed:** Replace the `eval` loop with direct commands or a
  small helper function, e.g.:

  ```bash
  require_git_repo() { git rev-parse --git-dir >/dev/null 2>&1 || { print_error "Not in a git repository"; exit 1; }; }
  require_flake_lock() { [[ -f flake.lock ]] || { print_error "flake.lock not found"; exit 1; }; }
  # ... etc
  ```
