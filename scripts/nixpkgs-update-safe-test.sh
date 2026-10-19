#!/usr/bin/env bash
set -euo pipefail

SCRIPT=$(cd "$(dirname "$0")/.." && pwd)/bin/nixpkgs-update-safe
export REAL_MKTEMP=${MKTEMP_BIN:-$(command -v mktemp)}
TEST_ROOT=$(mktemp -d "${TMPDIR:-/tmp}/nixpkgs-update-safe-test.XXXXXX")
trap 'rm -rf "$TEST_ROOT"' EXIT

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    cat "$work/output" >&2
    exit 1
}

prepare() {
    work="$TEST_ROOT/$1"
    mkdir -p "$work/bin" "$work/tmp files"
    export MOCK_CALLS="$work/calls" MOCK_GIT_STATE=normal MOCK_WIDTH=80
    export MOCK_UPDATE_RC=0 MOCK_UNCHANGED=0 MOCK_UNSET_TMPDIR=0
    export MOCK_TEMP_FILES="$work/temp-files"
    : > "$MOCK_CALLS"
    : > "$MOCK_TEMP_FILES"
    local cmd
    for cmd in jq cp touch tee cat grep tail tr date rm diff sed cmp; do
        ln -s "$(command -v "$cmd")" "$work/bin/$cmd"
    done
    ln -s "$BASH" "$work/bin/bash"
    cat > "$work/flake.lock" <<'JSON'
{"nodes":{"root":{"inputs":{"nixpkgs":"nixpkgs"}},"nixpkgs":{"locked":{"rev":"old-nixpkgs"}},"home-manager":{"locked":{"rev":"old-home-manager"}}}}
JSON
    cp "$work/flake.lock" "$work/original.lock"
    cat > "$work/bin/git" <<'SH'
#!/usr/bin/env bash
printf 'git %s\n' "$*" >> "$MOCK_CALLS"
case "$*" in
    'rev-parse --git-dir') [[ "$MOCK_GIT_STATE" != outside ]] ;;
    'diff --quiet flake.lock')
        [[ "$MOCK_GIT_STATE" != dirty ]] && cmp -s original.lock flake.lock ;;
    'diff --cached --quiet flake.lock') [[ "$MOCK_GIT_STATE" != staged ]] ;;
    'add flake.lock'|'restore --staged flake.lock'|'commit -m '*) exit 0 ;;
    'rev-parse --short HEAD') printf 'test-revision\n' ;;
    *) exit 99 ;;
esac
SH
    cat > "$work/bin/nix" <<'SH'
#!/usr/bin/env bash
printf 'nix %s\n' "$*" >> "$MOCK_CALLS"
printf '%s\n' x 'a longer output line for the progress display'
if [[ "$MOCK_UNCHANGED" == 0 ]]; then
    sed 's/old-/new-/g' original.lock > flake.lock
fi
exit "$MOCK_UPDATE_RC"
SH
    cat > "$work/bin/just" <<'SH'
#!/usr/bin/env bash
printf 'just %s\n' "$*" >> "$MOCK_CALLS"
case "$1" in
    nixpkgs-update-flake) exec nix flake update nixpkgs home-manager ;;
    hm-switch)
        if [[ -n "${NIX_CONFIG:-}" ]]; then
            printf '{"action":"msg","msg":"build complete"}\n' > "${NIX_CONFIG##*json-log-path = }"
        fi ;;
    *) exit 99 ;;
esac
SH
    cat > "$work/bin/tput" <<'SH'
#!/usr/bin/env bash
printf '%s\n' "$MOCK_WIDTH"
SH
    cat > "$work/bin/mktemp" <<'SH'
#!/usr/bin/env bash
file=$("$REAL_MKTEMP" "$@") || exit $?
printf '%s\n' "$file" >> "$MOCK_TEMP_FILES"
printf '%s\n' "$file"
SH
    chmod +x "$work/bin/git" "$work/bin/nix" "$work/bin/just" "$work/bin/tput" "$work/bin/mktemp"
}

run_script() {
    if (
        cd "$work"
        export PATH="$work/bin"
        if [[ "$MOCK_UNSET_TMPDIR" == 1 ]]; then
            unset TMPDIR
        else
            export TMPDIR="$work/tmp files"
        fi
        "$BASH" "$SCRIPT" "$@"
    ) > "$work/output" 2>&1; then
        rc=0
    else
        rc=$?
    fi
}

assert_preflight_failure() {
    [[ $rc -ne 0 ]] || fail "preflight accepted $1"
    grep -Fq "$1" "$work/output" || fail "missing diagnostic: $1"
    if [[ -e "$work/original.lock" ]]; then
        cmp -s "$work/original.lock" "$work/flake.lock" || fail 'preflight changed flake.lock'
    else
        [[ ! -e "$work/flake.lock" ]] || fail 'preflight created flake.lock'
    fi
    [[ ! -e "$work/.nix-skip-commits" ]] || fail 'preflight touched the skip list'
    [[ -z "$(find "$work" -name 'flake.lock.backup.*' -o -path "$work/tmp files/*")" ]] \
        || fail 'preflight created a backup or log'
    if grep -Eq '^(nix|just|git (add|commit|restore)) ' "$MOCK_CALLS"; then
        fail 'preflight ran an update, switch, or git mutation'
    fi
}

assert_temp_cleanup() {
    local file count=0
    while IFS= read -r file; do
        case "$file" in
            "$1/nixpkgs-update-safe."??????|\
            "$1/nixpkgs-update-safe-cmd."??????|\
            "$1/nixpkgs-update-safe-json."??????) ;;
            *) fail "unexpected temporary-file path: $file" ;;
        esac
        [[ ! -e "$file" ]] || fail "temporary file was not removed: $file"
        count=$((count + 1))
    done < "$MOCK_TEMP_FILES"
    [[ $count -eq 3 ]] || fail "expected three distinct log files, got $count"
    [[ $(sort -u "$MOCK_TEMP_FILES" | wc -l) -eq 3 ]] || fail 'log paths were reused'
}

for mode in both nixpkgs-only; do
    set --
    [[ "$mode" != nixpkgs-only ]] || set -- --nixpkgs-only
    for missing in nix jq; do
        prepare "missing-$missing-$mode"
        rm "$work/bin/$missing"
        run_script "$@"
        assert_preflight_failure "$missing not found"
    done

    prepare "success-$mode"
    run_script "$@"
    [[ $rc -eq 0 ]] || fail "$mode update failed"
    grep -q 'new-nixpkgs' "$work/flake.lock" || fail 'update was not applied'
    grep -q '^just hm-switch$' "$MOCK_CALLS" || fail 'system switch was not called'
    grep -q '^git commit -m ' "$MOCK_CALLS" || fail 'commit was not called'
    if [[ "$mode" == nixpkgs-only ]]; then
        grep -q '^nix flake update nixpkgs$' "$MOCK_CALLS" || fail 'wrong update mode'
    else
        grep -q '^just nixpkgs-update-flake$' "$MOCK_CALLS" || fail 'wrong update mode'
    fi
    assert_temp_cleanup "$work/tmp files"
done

for check in outside missing-lock dirty missing-just staged; do
    prepare "preflight-$check"
    case "$check" in
        outside)
            export MOCK_GIT_STATE=outside
            expected='Not in a git repository' ;;
        missing-lock)
            rm "$work/flake.lock" "$work/original.lock"
            expected='flake.lock not found' ;;
        dirty)
            export MOCK_GIT_STATE=dirty
            expected='flake.lock has uncommitted changes' ;;
        missing-just)
            rm "$work/bin/just"
            expected='just not found' ;;
        staged)
            export MOCK_GIT_STATE=staged
            expected='flake.lock is staged. Please unstage it first.' ;;
    esac
    run_script
    assert_preflight_failure "$expected"
done

prepare unchanged
export MOCK_UNCHANGED=1
run_script
[[ $rc -eq 0 ]] || fail 'unchanged lock failed'
grep -q 'flake.lock unchanged, nothing to do' "$work/output" || fail 'unchanged lock not detected'
if grep -Eq '^(just hm-switch|git (add|commit))' "$MOCK_CALLS"; then
    fail 'unchanged lock was applied or committed'
fi

prepare unset-tmpdir
export MOCK_UNSET_TMPDIR=1
run_script --nixpkgs-only
[[ $rc -eq 0 ]] || fail 'update failed without TMPDIR'
assert_temp_cleanup /tmp

for width in 1 2 3 4 80; do
    for status in 0 7; do
        prepare "width-$width-status-$status"
        export MOCK_WIDTH=$width MOCK_UPDATE_RC=$status
        run_script --nixpkgs-only
        if grep -q 'substring expression' "$work/output"; then
            fail "substring error at width $width"
        fi
        grep -Fq $'\r\033[K  x' "$work/output" || fail "short output missing at width $width"
        grep -Fq $'\r\033[K  a' "$work/output" || fail "long output missing at width $width"
        if [[ $width -eq 80 ]]; then
            grep -Fq $'\r\033[K  a longer output line for the progress display' "$work/output" \
                || fail 'normal-width output was truncated'
        fi
        if [[ $status -eq 0 ]]; then
            [[ $rc -eq 0 ]] || fail "successful update failed at width $width"
        else
            [[ $rc -ne 0 ]] || fail "failed update succeeded at width $width"
            grep -q 'Command failed (exit 7)' "$work/output" || fail 'command status was lost'
            cmp -s "$work/original.lock" "$work/flake.lock" || fail 'rollback did not restore the lock'
            if grep -q '^git commit ' "$MOCK_CALLS"; then
                fail 'failed update was committed'
            fi
        fi
    done
done

printf 'PASS: nixpkgs-update-safe\n'
