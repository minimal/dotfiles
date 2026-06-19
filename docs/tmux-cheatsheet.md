# Tmux Cheatsheet

**Prefix key:** `C-h` (Ctrl+h)

---

## Key

| Symbol | Meaning |
|---|---|
| `C-` | Ctrl |
| `S-` | Shift |
| `M-` | Alt |
| `prefix` | `C-h` |

---

## Session Management

| Keys | Action |
|---|---|
| `prefix` + `o` | **Sessionx** — fuzzy-find / create sessions |
| Within sessionx: | |
| `C-x` | Browse `~/code` directories for a new session |
| `C-r` | Rename selected session |
| `C-d` | Kill selected session |
| `C-w` | Browse windows within a session |
| `C-t` | Tree view (sessions + windows) |
| `?` | Toggle preview pane |
| `Esc` / `C-c` | Close sessionx |
| `prefix` + `s` | Built-in session list |
| `prefix` + `d` | Detach from session |
| `prefix` + `(` / `)` | Previous / next session |
| `tmux new -s name` | Create new session from shell |

---

## Pane Management (Vim-style)

| Keys | Action |
|---|---|
| `prefix` + `h` | Move to pane left |
| `prefix` + `j` | Move to pane down |
| `prefix` + `k` | Move to pane up |
| `prefix` + `l` | Move to pane right |
| `prefix` + `Tab` | Toggle last active pane |
| `prefix` + `"` | Split window horizontally (top/bottom) |
| `prefix` + `%` | Split window vertically (left/right) |
| `prefix` + `z` | Zoom pane fullscreen (toggle) |
| `prefix` + `x` | Kill current pane (confirm) |
| `prefix` + `!` | Break pane into new window |
| `prefix` + `q` | Show pane numbers |

### Resize

| Keys | Action |
|---|---|
| `prefix` + `H` | Resize pane left 5 cells |
| `prefix` + `J` | Resize pane down 5 cells |
| `prefix` + `K` | Resize pane up 5 cells |
| `prefix` + `L` | Resize pane right 5 cells |

---

## Window Management

| Keys | Action |
|---|---|
| `S-Left` | Previous window |
| `S-Right` | Next window |
| `prefix` + `c` | Create new window |
| `prefix` + `,` | Rename current window |
| `prefix` + `&` | Kill current window |
| `prefix` + `n` | Next window |
| `prefix` + `p` | Previous window |
| `prefix` + `0-9` | Switch to window by number |
| `prefix` + `w` | List windows |

---

## Copy Mode (Scrolling)

Enter copy mode with `prefix` + `[` or just scroll with the **mouse wheel**.

Mouse also works for clicking panes and dragging borders.

| Keys | Action |
|---|---|
| `j` | Scroll down one line |
| `k` | Scroll up one line |
| `C-d` | Scroll down half page |
| `C-u` | Scroll up half page |
| `C-f` | Scroll down full page |
| `C-b` | Scroll up full page |
| `g` | Go to top |
| `G` | Go to bottom |
| `v` | Begin text selection |
| `C-v` | Toggle rectangular block selection |
| `y` | Yank (copy) selection and exit copy mode |
| `Enter` | Exit copy mode |
| `q` | Exit copy mode |
| `prefix` + `y` | Paste buffer |

---

## Misc

| Keys | Action |
|---|---|
| `prefix` + `r` | Reload tmux config |
| `prefix` + `h` | Send `C-h` to the app (e.g. vim) |
| `prefix` + `:` | Enter tmux command mode |
| `prefix` + `?` | List all key bindings |
| `prefix` + `t` | Show clock |

---

## Cursor Shapes (zsh-vi-mode)

| Mode | Cursor |
|---|---|
| Normal (Esc) | Block |
| Insert (i) | Beam / pipe |

---

## Status Bar

```
[green]hostname [magenta]sessionname  |  0:zsh  1:bash  |  [cyan]uptime
```

- **Current window** highlighted in cyan
- **Activity** on other windows flashes green

---

## Config File

`~/code/dotfiles/config/tmux.conf` — reload with `prefix` + `r`.
