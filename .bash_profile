export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8
export PATH="$PATH:/Users/kpetrov/.local/bin:/Users/kpetrov/Library/Python/3.6/bin"

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

export PS1="\[\033[48;5;22m\]\h\[$(tput sgr0)\]\[\033[48;5;-1m\]:\w\\$ \[$(tput sgr0)\]"

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

alias c='clear'

## Colorize the ls output ##
alias ls='ls -G'
 
## Use a long listing format ##
alias ll='ls -la' 
 
## Show hidden files ##
alias l.='ls -d .* -G'

## get rid of command not found ##
alias cd..='cd ..' 
 
## a quick way to get out of current directory ##
alias ..='cd ..' 
alias ...='cd ../../../' 
alias ....='cd ../../../../' 
alias .....='cd ../../../../' 
alias .4='cd ../../../../' 
alias .5='cd ../../../../..'

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

## Create parent directories on demand
alias mkdir='mkdir -pv'

alias cn='cal -NA 2'
