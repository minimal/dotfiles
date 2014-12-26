#!/bin/zsh

# Dynamically build the $PATH variable
if [ "$kernel" = 'Linux' ]; then
    for dircomponent in $path     /sw/bin /sw/sbin /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin     /compat/linux/bin /compat/linux/sbin /compat/linux/usr/bin /compat/linux/usr/sbin     /usr/games /usr/X11R6/bin /usr/X11R6/sbin ~/bin ~/local/bin
    do
        if [[ -e $dircomponent ]]; then
            path=($path $dircomponent)
        fi
    done
    typeset -U path
    unset dircomponent

else
    # path=($path ~/bin)
    export PATH=/usr/local/bin:/usr/local/sbin:/bin:/usr/sbin:/sbin:/usr/bin:/usr/local/share/python:sud/usr/X11/bin:/Users/Chris/bin
fi
