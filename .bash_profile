export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8
export PGDATA=/usr/local/var/postgres/
export PATH="/usr/local/opt/node@6/bin:$PATH:/Users/kpetrov/.local/bin:/Users/kpetrov/Library/Python/3.6/bin"
if [ -f $(brew --prefix)/etc/bash_completion ]; then
	  . $(brew --prefix)/etc/bash_completion
  fi

export PS1="\h:\w\\$ \[$(tput sgr0)\]"

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

## Slidy alias for asciidoc
alias slidy='asciidoc --backend slidy'

alias cn='cal -NA 2'

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/kpetrov/.sdkman"
[[ -s "/Users/kpetrov/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/kpetrov/.sdkman/bin/sdkman-init.sh"
export PATH="/usr/local/opt/node@8/bin:$PATH"

alias t='todo.sh'
alias j='jrnl'
alias w='jrnl work'
