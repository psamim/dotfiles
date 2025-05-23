#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

# aria2
mkdir -p "$HOME/.aria2"
ln -sf $PWD/aria2/* $HOME/.aria2/

# bin
rm -rf "$HOME/.bin"
ln -sf "$PWD/bin" "$HOME/.bin"

# git
ln -sf "$PWD/git/config" "$HOME/.gitconfig"
ln -sf "$PWD/git/ignore" "$HOME/.gitignore"
git config --global core.excludesfile '~/.gitignore'

# tmux
ln -sf "$PWD/tmux/tmux.conf" "$HOME/.tmux.conf"
rm -rf "$HOME/.tmux"
ln -sf "$PWD/tmux" "$HOME/.tmux"
mkdir -p $HOME/.tmux/plugins
ln -sf $PWD/tpm $HOME/.tmux/plugins/tpm

# emacs
rm -rf "$HOME/.emacs.d"
rm -rf "$HOME/.doom.d"
ln -sf "$PWD/doom" "$HOME/.doom.d"

# zsh
ln -sf "$PWD/zsh/zshrc" "$HOME/.zshrc"

# Vim
rm -rf "$HOME/.vimrc"
rm -rf "$HOME/.config/nvim"
ln -sf "$PWD/astronvim" "$HOME/.config/nvim"

# ideavim
ln -sf $PWD/ideavim/ideavimrc $HOME/.ideavimrc

# alacritty
rm -rf $HOME/.config/alacritty
ln -sf $PWD/alacritty $HOME/.config/alacritty

# Karabiner
rm -rf $HOME/.config/karabiner
ln -sf $PWD/karabiner $HOME/.config/karabiner

# Code
ln -sf $PWD/vscode/keybindings.json "$HOME/Library/Application Support/Code/User/keybindings.json"
ln -sf $PWD/vscode/settings.json "$HOME/Library/Application Support/Code/User/settings.json"
