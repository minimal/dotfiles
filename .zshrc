#!/bin/zsh
# Lines configured by zsh-newuser-install

export ZSH_HISTORY_PATH=$HOME/.histfile


REPORTTIME=10  # if cmd takes longer than n seconds report the time
setopt autocd extendedglob 
setopt interactivecomments
function cmd_exists() {
    command -v "$1" >/dev/null 2>&1;
}

function has_virtualenv() {
    if [ -e .venv ]; then
        workon `cat .venv`
    fi
}

unalias cd 2> /dev/null 
function venv_cd () {
    cd "$@" && has_virtualenv
}
alias cd="venv_cd"

function h () {
    cd ~/$1
}

[ -s "/home/chris/.scm_breeze/scm_breeze.sh" ] && source "/home/chris/.scm_breeze/scm_breeze.sh"

function c () {
    git_index $1
}

# alias c="git_index"
fpath=(~/.zsh_profile.d/functions $fpath)

# includes
for zsh_source in $HOME/.zsh_profile.d/*.zsh; do
    source $zsh_source
done

bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/chris/.zshrc'
#fpath=(/usr/local/share/zsh/4.3.10/functions)
# End of lines added by compinstall


autoload -U promptinit && promptinit
#prompt adam2

export PIP_DOWNLOAD_CACHE=$HOME/.pip_download_cache
export PATH="/home/chris/Komodo-Edit-6/bin:$PATH"

export GREP_OPTIONS='--color=auto --exclude=*.pyc --exclude-dir=.git --exclude-dir=.svn'

export JAVA_HOME=/usr/lib64/jvm/jre-1.6.0-sun  # suse
export RHINO_HOME=/home/chris/share/rhino1_7R3/

# daryl
# export DARYL_PATH="/home/chris/dev/daryl"
# export DARYL_ENV="LOCAL"
#export DARYL_KEY=""
# export FASTFLIGHTS_ENV="UNITTEST"
# alias unosetests="DARYL_ENV=TEST nosetests"
# alias anosetests="DARYL_ENV=ATEST nosetests"

# key bindings - for cygwin compatiblity
#press control-v then key to find its string.
bindkey -e
bindkey '^?' backward-delete-char
bindkey '^[[3~' delete-char
bindkey -s '^[[1~' '^A'
bindkey -s '^[[4~' '^E'
#bindkey -s '^[[C' '^[f'
#bindkey -s '^[[D' '^[b'

# version control
autoload -Uz vcs_info


git-pickaxe() {
    git log -S$1 -p
}

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export LESS="-R"           # raw mode for correct colors
export EDITOR="emacsclient -nw"  # no window mode
export TERM=xterm-256color;      # nice colours

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1        ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1        ;;
            *.tbz2)      tar xjf $1      ;;
            *.tgz)       tar xzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

compress () {
   FILE=$1
   shift
   case $FILE in
      *.tar.bz2) tar cjf $FILE $*  ;;
      *.tar.gz)  tar czf $FILE $*  ;;
      *.tgz)     tar czf $FILE $*  ;;
      *.zip)     zip $FILE $*      ;;
      *.rar)     rar $FILE $*      ;;
      *)         echo "Filetype not recognized" ;;
   esac
}



#virtualenv setup
export WORKON_HOME="$HOME/Envs"
#source $HOME/bin/virtualenvwrapper_bashrc
source /usr/local/bin/virtualenvwrapper.sh


#PS1="$PS1\w\$(parse_git_branch) $ "

# git prompt
setopt prompt_subst
autoload colors
colors

autoload -Uz vcs_info

# set some colors
for COLOR in RED GREEN YELLOW WHITE BLACK CYAN; do
eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
PR_RESET="%{${reset_color}%}";

# set formats
# %b - branchname
# %u - unstagedstr (see below)
# %c - stangedstr (see below)
# %a - action (e.g. rebase-i)
# %R - repository path
# %S - path in the repository
FMT_BRANCH="${PR_GREEN}%b%u%c${PR_RESET}" # e.g. master¹²
FMT_ACTION="(${PR_CYAN}%a${PR_RESET}%)" # e.g. (rebase-i)
FMT_PATH="%R${PR_YELLOW}/%S" # e.g. ~/repo/subdir

# check-for-changes can be really slow.
# you should disable it, if you work with large repositories
zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr '¹' # display ¹ if there are unstaged changes
zstyle ':vcs_info:*:prompt:*' stagedstr '²' # display ² if there are staged changes
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}//" "${FMT_PATH}"
zstyle ':vcs_info:*:prompt:*' formats "${FMT_BRANCH}//" "${FMT_PATH}"
zstyle ':vcs_info:*:prompt:*' nvcsformats "" "%~"

function precmd {
    vcs_info 'prompt'
}

function lprompt {
    local brackets=$1
    local color1=$2
    local color2=$3

    local bracket_open="${color1}${brackets[1]}${PR_RESET}"
    local bracket_close="${color1}${brackets[2]}"

    local git='$vcs_info_msg_0_'
    local cwd="${color2}%B%1~%b"
    local smiley='%(?.%{${fg[green]}%}:).%{${fg[red]}%}:()%{${fg[default]}%}'
    local newline=$'\n'
    local timestamp='%B%{${fg[blue]}%}[%T]%{${fg[default]}%}%b'
    PROMPT="${bracket_open}${git}${cwd}${bracket_close}${PR_RESET}${newline}${smiley} ${timestamp} %(!.#.$) ${PR_RESET}"
}

function rprompt {
    local brackets=$1
    local color1=$2
    local color2=$3

    local bracket_open="${color1}${brackets[1]}${PR_RESET}"
    local bracket_close="${color1}${brackets[2]}${PR_RESET}"
    local colon="${color1}:"
    local at="${color1}@${PR_RESET}"

    local user_host="${color2}%n${at}${color2}%m"
    local vcs_cwd='${${vcs_info_msg_1_%%.}/$HOME/~}'
    local cwd="${color2}%B%20<..<${vcs_cwd}%<<%b"
    local inner="${user_host}${colon}${cwd}"
    local newline=$'\n'
    RPROMPT="${PR_RESET}${bracket_open}${inner}${bracket_close}${PR_RESET}"
}

lprompt '[]' $BR_BRIGHT_BLACK $PR_WHITE
rprompt '()' $BR_BRIGHT_BLACK $PR_WHITE


function gitbr {
    for k in `git branch|sed s/^..//`;do echo -e `git log -1 \
    --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" "$k"`\\t"$k";done|sort
}


# hubot
# export HUBOT_IRC_NICK="hubot"
# export HUBOT_IRC_ROOMS="#dev"
# export HUBOT_IRC_SERVER="irc.site.com"
# export HUBOT_IRC_PASSWORD=""


# setup fasd https://github.com/clvv/fasd
eval "$(fasd --init auto)"
alias em="f -e 'emacsclient --no-wait'"


# edit current line
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

export PERL_LOCAL_LIB_ROOT="/home/chris/perl5";
export PERL_MB_OPT="--install_base /home/chris/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/chris/perl5";
export PERL5LIB="/home/chris/perl5/lib/perl5/x86_64-linux-thread-multi:/home/chris/perl5/lib/perl5";
export PATH="/home/chris/perl5/bin:$PATH";


# settitle() {
#     printf "\033k$1\033\\"
# }
# ssh() {
#     settitle "$*"
#     command ssh "$@"
# }

# export settitle=settitle
