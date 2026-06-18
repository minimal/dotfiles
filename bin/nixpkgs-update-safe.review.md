# `bin/nixpkgs-update-safe` — Code Review Notes

This file records issues identified during code review that were **not** fixed in the source file.

## Low Severity

### Very narrow terminal width crashes the script

- **Location:** `run_with_progress()` — `cols=$(($(tput cols 2>/dev/null || echo 80) - 4))`
- **Issue:** If the terminal width is 1–3 columns, `cols` becomes negative. Bash then fails on `${line:0:$cols}` with a "substring expression < 0" error, which triggers `set -e` and aborts the update.
- **Fix if addressed:** Clamp `cols` to a minimum of 1:

  ```bash
  cols=$(($(tput cols 2>/dev/null || echo 80) - 4))
  (( cols < 1 )) && cols=1
  ```

## Medium Concerns

### Commit failure rolls back a successful system switch

- **Location:** End of script — `git add flake.lock` / `git commit -m "$COMMIT_MSG"`
- **Issue:** If `hm-switch` succeeds but `git commit` fails (e.g., GPG signing failure, pre-commit hook rejection, missing author identity), the `ERR` trap runs `rollback()`. That restores the old `flake.lock`, re-runs `hm-switch` to revert the system, and adds the new nixpkgs revision to the skip list. The system was actually working, so this may incorrectly mark a good revision as bad.
- **Fix if addressed:** Handle commit failure separately from switch failure. After a successful switch, a commit failure could unstage `flake.lock`, warn the user, and leave the system on the new configuration rather than rolling back.

## Minor Concerns

### `eval` preflight checks are a maintenance footgun

- **Location:** Preflight loop — `for check in ...; do eval "$check"; done`
- **Issue:** The loop uses `eval` on hardcoded strings. There is no current command-injection risk because the strings contain no user input, but the pattern makes future edits error-prone and easy to misquote.
- **Fix if addressed:** Replace the `eval` loop with direct commands or a small helper function, e.g.:

  ```bash
  require_git_repo() { git rev-parse --git-dir >/dev/null 2>&1 || { print_error "Not in a git repository"; exit 1; }; }
  require_flake_lock() { [[ -f flake.lock ]] || { print_error "flake.lock not found"; exit 1; }; }
  # ... etc
  ```
