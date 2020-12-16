zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' menu select=5
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt notify
setopt histignoredups
setopt promptsubst
unsetopt beep
#bindkey -v
ttyctl -f
REPORTTIME=10

bindkey '\e[3~' delete-char
bindkey "^?" backward-delete-char
#bindkey '^R' history-incremental-search-backward
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey "^[[5~" history-beginning-search-backward
bindkey "^[[6~" history-beginning-search-forward


# Prompt {{{

autoload -U promptinit
promptinit
autoload -U colors
colors

build-prompt() {
	local exitcode=$?
	local prompt=''

	local rst='%{\e[0m%}'    # Reset
	local blk='%{\e[0;30m%}' # Black
	local red='%{\e[0;31m%}' # Red
	local grn='%{\e[0;32m%}' # Green
	local ylw='%{\e[0;33m%}' # Yellow
	local blu='%{\e[0;34m%}' # Blue
	local mag='%{\e[0;35m%}' # Magenta
	local cyn='%{\e[0;36m%}' # Cyan
	local wht='%{\e[0;37m%}' # White
	local gry='%{\e[0;90m%}' # Grey (high intensity black)

	# exitcode
	if [[ $exitcode -eq 0 ]]; then
		prompt+="$grn%{✔%G%}$rst"
	else
		prompt+="$red%{✘%G%}$rst"
	fi

	# username
	prompt+=" $grn%n$rst"

	# hostname
	prompt+=" ${gry}at$rst $cyn%M$rst"

	# current working directory
	prompt+=" ${gry}in$rst $ylw%~$rst"

	# git branch
	local branch
	if branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); then
		if [[ "$branch" == "HEAD" ]]; then
			branch=$(git describe --exact-match HEAD 2>/dev/null)
			if [[ $? -ne 0 ]]; then
				branch=$(git rev-parse --short HEAD 2>/dev/null)
			fi
		fi
		prompt+=" ${gry}on$rst git:$cyn($branch)$rst"
	fi

	prompt+=$'\n'

	prompt+="$rst\$ "

	echo -n $prompt
}

PROMPT='$(build-prompt)'

function zle-keymap-select {
    zle reset-prompt
}

zle -N zle-keymap-select
export KEYTIMEOUT=1

# }}}


export EDITOR=vim
unset SSH_ASKPASS


alias ls='ls --color=auto -F'
alias ll='ls -l'
alias grep='grep --color=auto'
alias less='less -R'
alias ulimit='ulimit -S'
alias makepkg='makepkg -Cc'
alias octave='octave --no-gui'

alias git-clone-shallow='git clone --depth 1'
alias offlineimap-sync-now='killall -USR1 offlineimap'
alias octave-gui='octave --force-gui'

diffc() { colordiff -u "$@" | diff-highlight | less -FR }

qpdf-merge() {
	out="$1";	shift
	qpdf --linearize --empty --pages "$@" -- $out }

[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local


true # To start the shell with a zero exit code
