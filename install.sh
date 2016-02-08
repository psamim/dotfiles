#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

# aria2
rm -rf "$HOME/.aria2"
ln -sf "$PWD/aria2" "$HOME/.aria2"

# bin
rm -rf "$HOME/.bin"
ln -sf "$PWD/bin" "$HOME/.bin"

# git
rm -rf "$HOME/.gitconfig"
rm -rf "$HOME/.gitignore"
ln -sf "$PWD/git/config" "$HOME/.gitconfig"
ln -sf "$PWD/git/ignore" "$HOME/.gitignore"
git config --global core.excludesfile '~/.gitignore'

# tmux
rm -rf "$HOME/.tmux"
rm -rf "$HOME/.tmux.conf"
ln -sf "$PWD/tmux/tmux.conf" "$HOME/.tmux.conf"
ln -sf "$PWD/tmux" "$HOME/.tmux"

# vim
# rm -rf "$HOME/.vim"
# ln -sf "$PWD/vim" "$HOME/.vim"
# rm -f "$HOME/.vimrc"
# ln -sf "$PWD/vim/vimrc" "$HOME/.vimrc"

# emacs
rm -rf "$HOME/.emacs.d"
ln -sf "$PWD/emacs.d" "$HOME/.emacs.d"
ln -sf "$PWD/spacemacs/exclude" "$PWD/.git/modules/emacs.d/info/exclude"
rm -rf "$HOME/.spacemacs"
ln -sf "$PWD/spacemacs/spacemacs" "$HOME/.spacemacs"
rm -rf "$HOME/.emacs.d/private"
ln -sf "$PWD/spacemacs/private" "$HOME/.emacs.d/private"

# vrapperrc
rm -rf "$HOME/.vrapperrc"
ln -sf "$PWD/vrapper/vrapperrc.vim" "$HOME/.vrapperrc"

# zsh
rm -f "$HOME/.zshrc"
ln -sf "$PWD/zsh/zshrc" "$HOME/.zshrc"

# Vimperator
rm -rf "$HOME/.vimperator"
ln -sf "$PWD/vimperator" "$HOME/.vimperator"
rm -rf "$HOME/.vimperatorrc"
ln -sf "$PWD/vimperator/vimperatorrc" "$HOME/.vimperatorrc"

# Muttator
rm -rf "$HOME/.muttator"
ln -sf "$PWD/muttator" "$HOME/.muttator"
rm -rf "$HOME/.muttatorrc"
ln -sf "$PWD/muttator/muttatorrc" "$HOME/.muttatorrc"

# i3
rm -rf "$HOME/.i3"
ln -sf "$PWD/i3" "$HOME/.i3"
rm -rf "$HOME/.i3status.conf"
ln -sf "$PWD/i3/i3status.conf" "$HOME/.i3status.conf"
rm -rf "$HOME/.i3blocks.conf"
ln -sf "$PWD/i3/i3blocks.conf" "$HOME/.i3blocks.conf"

# compton
rm -rf "$HOME/.compton"
ln -sf "$PWD/compton/compton.conf" "$HOME/.compton"

# X
rm -rf "$HOME/.Xresources"
ln -sf "$PWD/X/Xresources" "$HOME/.Xresources"
rm -rf "$HOME/.xinitrc"
ln -sf "$PWD/X/xinitrc" "$HOME/.xinitrc"

# dunst
rm -rf "$HOME/.config/dunst"
ln -sf "$PWD/dunst" "$HOME/.config/dunst"

# phpcs
# rm -rf "$HOME/.phpcs"
# ln -sf "$PWD/phpcs" "$HOME/.phpcs"
# ~/.phpcs/commands.sh

# password-store
rm -rf "$HOME/.password-store"
ln -sf "$PWD/password-store" "$HOME/.password-store"

# termite
rm -rf "$HOME/.config/termite"
ln -sf "$PWD/termite" "$HOME/.config/termite"

# gtk-3.0
rm -rf "$HOME/.config/gtk-3.0"
ln -sf "$PWD/termite" "$HOME/.config/gtk-3.0"

# systemd
mkdir -p "$HOME/.config/systemd/user/"
ln -sf $PWD/systemd/* $HOME/.config/systemd/user/
