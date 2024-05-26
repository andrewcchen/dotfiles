#
# ~/.bashrc
#

umask 022

[[ $PATH =~ (^|:)$HOME/.local/bin($|:) ]] || export PATH="$HOME/.local/bin:${PATH}"
[[ $PATH =~ (^|:)$HOME/bin($|:) ]] || export PATH="$HOME/bin:${PATH}"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=vim

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias less='less -R'

source /usr/share/git/completion/git-completion.bash
source /usr/share/git/completion/git-prompt.sh

PS1='\[\e[32m\]\u@\h\[\e[m\] \[\e[33m\]\w\[\e[m\]$(__git_ps1 " \[\e[35m\](%s)\[\e[m\]")\n\$ '

HISTSIZE=100000
HISTFILESIZE=100000
HISTCONTROL=ignoreboth
HISTIGNORE='?:??'
