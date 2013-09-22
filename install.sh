#!/bin/bash
# This scripts installs all conf files
# by creating appropriate symbolic links
# cron jobs are not included.

# aria2/
mkdir -p "/home/$USER/.aria2/"
ln -sf "$PWD/aria2/aria2.conf" "/home/$USER/.aria2/.aria2.conf"

# bashthisia iakskasj a/
ln -sf "$PWD/bash/bashrc" "/home/$USER/.bashrc"

# bin/
ln -sf "$PWD/bin" "/home/$USER/.local/bin"

# fish/
ln -sf "$PWD/fish" "/home/$USER/.config/fish"

# git/
ln -sf "$PWD/git/config" "/home/$USER/.gitconfig"
ln -sf "$PWD/git/ignore" "/home/$USER/.gitignore"
git config --global core.excludesfile '~/.gitignore'

# hg/
ln -sf "$PWD/hg/hgrc" "/home/$USER/.hg"

# komodo/
mkdir -p "/home/$USER/.komodoedit/8.0"
ln -sf "$PWD/komodo" "/home/$USER/.komodoedit/8.0"

# tmux/
ln -sf "$PWD/tmux/tmux.conf" "/home/$USER/.tmux.conf"
ln -sf "$PWD/tmux/tmux.conf" "/home/$USER/.byobu/.tmux.conf"
ln -sf "$PWD/tmux" "/home/$USER/.tmux"

# vim/
rm -rf "/home/$USER/.vim"
ln -sf "$PWD/vim" "/home/$USER/.vim"
rm -f "/home/$USER/.vimrc"
ln -sf "$PWD/vim/vimrc" "/home/$USER/.vimrc"

# Zsh/
rm -rf "/home/$USER/.zsh"
ln -sf "$PWD/zsh" "/home/$USER/.zsh"
ln -sf "$PWD/zsh/prezto" "/home/$USER/.zprezto"
rm -f "/home/$USER/.zshrc"
ln -sf "$PWD/zsh/zshrc" "/home/$USER/.zshrc"
ln -sf "$PWD/zsh/prezto/runcoms/zlogin" "/home/$USER/.zlogin"
ln -sf "$PWD/zsh/prezto/runcoms/zlogout" "/home/$USER/.zlogout"
ln -sf "$PWD/zsh/prezto/runcoms/zpreztorc" "/home/$USER/.zpreztorc"
ln -sf "$PWD/zsh/prezto/runcoms/zprofile" "/home/$USER/.zprofile"
ln -sf "$PWD/zsh/prezto/runcoms/zshenv" "/home/$USER/.zshenv"
ln -sf "$PWD/zsh/prezto/runcoms/zshrc" "/home/$USER/.zshrc"
