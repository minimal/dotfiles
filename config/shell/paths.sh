#!/bin/sh
# Personal bin directories, sourced by zsh profile and pi's bash tool.
# Keep this file POSIX-sh compatible so both bash and zsh can source it.

# Idempotent prepend: add dir to PATH only if not already present.
_prepend_path() {
    case ":${PATH}:" in
        *:"$1":*) ;;
        *) export PATH="$1:${PATH}" ;;
    esac
}

_prepend_path "$HOME/bin"
_prepend_path "$HOME/.local/bin"
_prepend_path "$HOME/.cargo/bin"
_prepend_path "$HOME/.emacs.d/bin"
_prepend_path "$HOME/.krew/bin"
_prepend_path "$HOME/.babashka/bbin/bin"
_prepend_path "$HOME/.bun/bin"
_prepend_path "$HOME/.local/share/pnpm/bin"
_prepend_path "$HOME/.npm-global/bin"
_prepend_path "/opt/homebrew/bin"
_prepend_path "/opt/homebrew/sbin"

unset -f _prepend_path 2>/dev/null || true
