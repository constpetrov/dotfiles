export PATH=$PATH:/Users/kpetrov/.local/bin
if [ -f $(brew --prefix)/etc/bash_completion ]; then
	  . $(brew --prefix)/etc/bash_completion
  fi
export PS1='[\u@mbp \w$(__git_ps1)]\$ '
