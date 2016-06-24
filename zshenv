umask 027

[[ $PATH == *"/usr/lib/distcc/bin"* ]] || export PATH="/usr/lib/distcc/bin:${PATH}"
[[ $PATH == "$HOME/bin"* ]] || export PATH="$HOME/bin:${PATH}"

[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local
