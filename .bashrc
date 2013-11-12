# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# There are 3 different types of shells in bash: the login shell, normal shell
# and interactive shell. Login shells read ~/.profile and interactive shells
# read ~/.bashrc; in our setup, /etc/profile sources ~/.bashrc - thus all
# settings made here will also take effect in a login shell.
#
# NOTE: It is recommended to make language settings in ~/.profile rather than
# here, since multilingual X sessions would not work properly if LANG is over-
# ridden in every subshell.

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
#export EDITOR=/usr/bin/vim
#export EDITOR=/usr/bin/mcedit

# For some news readers it makes sense to specify the NEWSSERVER variable here
#export NEWSSERVER=your.news.server

# If you want to use a Palm device with Linux, uncomment the two lines below.
# For some (older) Palm Pilots, you might need to set a lower baud rate
# e.g. 57600 or 38400; lowest is 9600 (very slow!)
#
#export PILOTPORT=/dev/pilot
#export PILOTRATE=115200

test -s ~/.alias && . ~/.alias || true

[ -s "/home/chris/.scm_breeze/scm_breeze.sh" ] && source "/home/chris/.scm_breeze/scm_breeze.sh"

PS1="\[\e[32;1m\]\[\e[37;1m\]\`if [ \$? = 0 ]; then echo \[\e[33m\] :\)\[\e[0m\]; else echo \[\e[31m\] X\(\[\e[0m\]; fi\`\[\e[32;1m\] \[\e[32;1m\](\[\e[37;1m\]\h:\u\[\e[32;1m\])-(\[\e[37;1m\]\j\[\e[32;1m\])-(\D{%Y-%m-%d %H:%M:%S})-(\[\e[37;1m\]\w\[\e[32;1m\])\n\[\e[0m\] $ "

export GREP_OPTIONS='--color=auto --exclude=*.pyc --exclude-dir=.git --exclude-dir=.svn'
