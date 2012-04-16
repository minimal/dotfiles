# A few aliases

# ls aliases
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias lh='ls -lh'
alias ldot='ls -ld .*'
#alias ls='ls -G' # osx/bsd

alias rdirs='dirs -v'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias rgrep='rgrep --color=auto'

#alias ack='ack-grep'
alias ackpy='ack --python'
alias p-ack="ps ax | ack"

alias rmpyc='rm **/*pyc'


alias dud='du --max-depth=1 -h'

alias ltmux="if tmux has; then tmux attach; else tmux new; fi"

# more aliases
# debian/ubuntu shortcuts
alias apt-install='sudo apt-get install'
alias apt-remove='sudo apt-get remove'
# suse
alias zinstall='sudo zypper install' 
alias zearch='zypper se'
alias zrefresh='sudo zypper refresh'
alias open='kde-open'

alias ppjson='python -c "import simplejson; import sys; print simplejson.dumps(simplejson.loads(sys.stdin.read()), indent=4)" | pygmentize -f console -l js'

alias gita="git archive --format=zip `git reflog | grep 'HEAD@{0}' | cut -d \" \" -f1 | sed 's/[.]*//g'` > archive.zip"
alias gka="gitk --all&"

# start a Python HTTP server with webshare
alias webshare='python -c "import SimpleHTTPServer;SimpleHTTPServer.test()"'

# open in running emacs from  cmdline w/o waiting
alias em="emacsclient --no-wait"
