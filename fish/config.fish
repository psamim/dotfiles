# fish path {{{
set -x FISH ~/.config/fish
# }}}
# binary files are here {{{
set -x PATH ~/Workspace/dotfiles/bin
set -x PATH $PATH ~/Workspace/dotfiles/bin/local
set -x PATH $PATH /usr/local/bin
set -x PATH $PATH /usr/bin
set -x PATH $PATH /bin
set -x PATH $PATH /usr/sbin
set -x PATH $PATH /sbin
# }}}
# bin path in home {{{
set -x HOME_BIN_PATH ~/bin
# }}}

# sudo
alias s='sudo'
alias yum='sudo yum'
# python
alias p2='python2'
alias p3='python3'
# wget
alias wget='wget -c'
