umask 027

[[ $PATH == *"/usr/lib/distcc/bin"* ]] || export PATH="/usr/lib/distcc/bin:${PATH}"
[[ $PATH == "$HOME/bin"* ]] || export PATH="$HOME/bin:${PATH}"

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local
