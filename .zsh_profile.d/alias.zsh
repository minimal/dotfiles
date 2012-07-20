# A few aliases

# ls aliases
if [ "$kernel" = 'Linux' ]; then
    alias ls='ls --color=auto'
fi

alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias lh='ls -lh'
alias ldot='ls -ld .*'
#alias ls='ls -G' # osx/bsd

# ls++ http://github.com/trapd00r/ls--
if (cmd_exists ls++); then
    alias ll='ls++'
    alias lla='ls++ -a'
    alias llo='ll --potsf'
fi

alias rdirs='dirs -v'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias rgrep='rgrep --color=auto'

#alias ack='ack-grep'
if (cmd_exists ack); then
    alias ackpy='ack --python'
    alias p-ack="ps ax | ack"
fi

# use the fastest searcher for grepper
if (cmd_exists ag); then
    export GREPPER='ag'
    alias agpy='ag -G "\.py$"'
    alias p-ag="ps ax | ag"

elif (cmd_exists ack); then
    export GREPPER='ack'
else
    export GREPPER='grep'
fi


alias rmpyc='rm **/*pyc'
alias pyclean='rmpyc'
alias rm-git-turds='rm **/(*.orig|*(LOCAL|BASE|REMOTE|BACKUP)*)'

alias dud='du --max-depth=1 -h'
alias duds='du -h --max-depth=1 . | sort -h'

alias ltmux="if tmux has; then tmux attach; else tmux new; fi"

# more aliases
# debian/ubuntu shortcuts
if [ "$kernel" = 'Linux' ]; then
    alias apt-install='sudo apt-get install'
    alias apt-remove='sudo apt-get remove'
# suse
    alias zinstall='sudo zypper install'
    alias zearch='zypper se'
    alias zrefresh='sudo zypper refresh'
    alias open='kde-open'
fi

alias ppjson='python -c "import simplejson; import sys; print simplejson.dumps(simplejson.loads(sys.stdin.read()), indent=4)" | pygmentize -f console -l js'

alias gita="git archive --format=zip `git reflog | grep 'HEAD@{0}' | cut -d \" \" -f1 | sed 's/[.]*//g'` > archive.zip"
alias gka="gitk --all&"
alias g="git"

# start a Python HTTP server with webshare
alias webshare='python -c "import SimpleHTTPServer;SimpleHTTPServer.test()"'

# open in running emacs from  cmdline w/o waiting
if [ "$kernel" = 'Darwin' ]; then
    alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
fi


alias e="emacsclient --no-wait"
