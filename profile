umask 022

[[ $PATH =~ (^|:)$HOME/.local/bin($|:) ]] || export PATH="$HOME/.local/bin:${PATH}"
[[ $PATH =~ (^|:)$HOME/bin($|:) ]] || export PATH="$HOME/bin:${PATH}"

true
