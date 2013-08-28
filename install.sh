#!/bin/bash
# This scripts installs all conf files
# by creating appropriate symbolic links
# cron jobs are not included.

# aria2/
mkdir -p "/home/$USER/.aria2/"
ln -sf "$PWD/aria2/aria2.conf" "/home/$USER/.aria2/.aria2.conf"

# bash/
ln -sf "$PWD/bash/bashrc" "/home/$USER/.bashrc"

# bin/
ln -sf "$PWD/bin" "/home/$USER/.local/bin"

# fish/
mkdir -p "/home/$USER/.config/"
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

# vim/
ln -sf "$PWD/vim" "/home/$USER/.vim"
ln -sf "$PWD/vim/vimrc" "/home/$USER/.vimrc"
