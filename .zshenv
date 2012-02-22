#!/bin/zsh

# Dynamically build the $PATH variable
for dircomponent in $path     /sw/bin /sw/sbin /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin     /compat/linux/bin /compat/linux/sbin /compat/linux/usr/bin /compat/linux/usr/sbin     /usr/games /usr/X11R6/bin /usr/X11R6/sbin ~/bin ~/crossover/bin
  do
  if [[ -e $dircomponent ]]; then
      path=($path $dircomponent)
  fi
done
typeset -U path
unset dircomponent
