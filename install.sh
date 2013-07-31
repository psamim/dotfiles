#!/bin/bash
# This scripts installs all conf files
# by creating appropriate symbolic links
# cron jobs are not included.

# aria2/
ln -sf "/home/$USER/dotfiles/aria2/aria2.conf" "/home/$USER/.aria2/.aria2.conf"

# bash/
ln -sf "/home/$USER/dotfiles/bash/bashrc" "/home/$USER/.bashrc"

# bin/
ln -sf "/home/$USER/dotfiles/bin" "/home/$USER/.local/bin"

# fish/
ln -sf "/home/$USER/dotfiles/fish" "/home/$USER/.config/fish/"

# git/
ln -sf "/home/$USER/dotfiles/git/config" "/home/$USER/.gitconfig"
ln -sf "/home/$USER/dotfiles/git/ignore" "/home/$USER/.gitignore"
git config --global core.excludesfile '~/.gitignore'

# hg/
ln -sf "/home/$USER/dotfiles/hg/hgrc" "/home/$USER/.hg"

# komodo/
ln -sf "/home/$USER/dotfiles/komodo" "/home/$USER/.komodoedit/8.0"

# tmux/
ln -sf "/home/$USER/dotfiles/tmux/tmux.conf" "/home/$USER/.tmux.conf"

# vim/
ln -sf "/home/$USER/dotfiles/vim" "/home/$USER/.vim"
