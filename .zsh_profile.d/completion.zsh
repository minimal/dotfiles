# expand functions in the prompt
setopt prompt_subst

autoload -Uz compinit
compinit


# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end

# nosecomplete
autoload -U bashcompinit
bashcompinit

_nosetests()
{
    cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=(`nosecomplete ${cur} 2>/dev/null`)
}
complete -o nospace -F _nosetests nosetests


# fabric experimental

# _fab()
# {
#     local cur
#     COMPREPLY=()
#     # Variable to hold the current word
#     cur="${COMP_WORDS[COMP_CWORD]}"
 
#     # Build a list of the available tasks using the command 'fab -l'
#     local tags=$(fab -l 2>/dev/null | grep "^    " | awk '{print $1;}')
 
#     # Generate possible matches and store them in the
#     # array variable COMPREPLY
#     COMPREPLY=($(compgen -W "${tags}" $cur))
#     echo $tags
# }
 
# # Assign the auto-completion function _fab for our command fab.
# complete -F _fab fab
