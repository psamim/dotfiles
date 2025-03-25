#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

# aria2
mkdir -p "$HOME/.aria2"
ln -sf $PWD/aria2/* $HOME/.aria2/

# tridactyl
rm -f "$HOME/.tridactylrc"
ln -sf $PWD/tridactyl/tridactylrc $HOME/.tridactylrc 

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
# ln -sf "$PWD/emacs.d" "$HOME/.emacs.d"
# ln -sf "$PWD/spacemacs/exclude" "$PWD/.git/modules/emacs.d/info/exclude"
# ln -sf "$PWD/spacemacs/spacemacs" "$HOME/.spacemacs"
# rm -rf "$HOME/.emacs.d/private"
# ln -sf "$PWD/spacemacs/private" "$HOME/.emacs.d/private"
ln -sf "$PWD/doom" "$HOME/.doom.d"

# vrapperrc
ln -sf "$PWD/vrapper/vrapperrc.vim" "$HOME/.vrapperrc"

# zsh
ln -sf "$PWD/zsh/zshrc" "$HOME/.zshrc"

# Vimperator
rm -rf "$HOME/.vimperator"
ln -sf "$PWD/vimperator" "$HOME/.vimperator"
ln -sf "$PWD/vimperator/vimperatorrc" "$HOME/.vimperatorrc"

# Vim
rm -rf "$HOME/.vimrc"
ln -sf "$PWD/vim/vimrc" "$HOME/.vimrc"
rm -rf "$HOME/.config/nvim"
ln -sf "$PWD/nvim" "$HOME/.config/nvim"

# Install vim plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

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
