source /usr/local/etc/bash_completion.d/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1="\[\033[38;5;11m\]\u\[$(tput sgr0)\]\[\033[38;5;15m\]@\[$(tput sgr0)\]\[\033[38;5;10m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\]:[\[$(tput bold)\]\[$(tput sgr0)\]\[\033[38;5;45m\]\w\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\]]\[$(tput sgr0)\]\[\033[38;5;213m\] $(__git_ps1)\\$ \[$(tput sgr0)\]"
