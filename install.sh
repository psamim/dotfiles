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

# Vimperator
rm -rf "$HOME/.vimperator"
ln -sf "$PWD/vimperator" "$HOME/.vimperator"
ln -sf "$PWD/vimperator/vimperatorrc" "$HOME/.vimperatorrc"

# Vim
rm -rf "$HOME/.vimrc"
rm -rf "$HOME/.config/nvim"
ln -sf "$PWD/astronvim" "$HOME/.config/nvim"

# X
ln -sf "$PWD/X/Xresources" "$HOME/.Xresources"
ln -sf "$PWD/X/xinitrc" "$HOME/.xinitrc"
ln -sf "$PWD/X/Xmodmap" "$HOME/.Xmodmap"

# systemd
rm -rf "$HOME/.config/systemd"
mkdir -p "$HOME/.config/systemd/"
cp -r "$PWD/systemd/user" "$HOME/.config/systemd/user"
cp -r "$PWD/systemd/system/*" "/etc/systemd/system"

# beets
mkdir -p "$HOME/.config/beets"
cp -r "$PWD/beets/config.yaml" "$HOME/.config/beets/config.yaml"

# arbtt
mkdir -p "$HOME/.arbtt/"
ln -sf "$PWD/arbtt/categorize.cfg" "$HOME/.arbtt/categorize.cfg"

# tint2
mkdir -p "$HOME/.config/tint2/"
ln -sf $PWD/tint2/tint2rc $HOME/.config/tint2/tint2rc

# vimfx
echo "vimfx: Remember to change config_file_directory prop in Firefox"
rm -rf "$HOME/.config/vimfx"
ln -sf "$PWD/vimfx" "$HOME/.config/vimfx"

# gpg
ln -sf $PWD/pam_environment $HOME/.pam_environment
ln -sf $PWD/gpg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf

# eslint
ln -sf $PWD/eslintrc.js $HOME/.eslintrc.js

# pacman
rm -rf $HOME/.config/pacman
ln -sf $PWD/pacman $HOME/.config/pacman

# ledger-autosync
rm -rf $HOME/.config/ledger-autosync
ln -sf $PWD/ledger-autosync $HOME/.config/ledger-autosync

# ideavim
ln -sf $PWD/ideavim/ideavimrc $HOME/.ideavimrc

# desktop
mkdir -p  $HOME/.local/share/applications/
for f in $PWD/desktop-files/*.desktop; do
    ln -sf $f $HOME/.local/share/applications/
done

kwriteconfig5 --file ~/.config/kwinrc --group Windows --key BorderlessMaximizedWindows true

# rofi
rm -rf $HOME/.config/rofi
ln -sf $PWD/rofi $HOME/.config/rofi

# alacritty
rm -rf $HOME/.config/alacritty
ln -sf $PWD/alacritty $HOME/.config/alacritty

# autorandr
rm -rf $HOME/.config/autorandr
ln -sf $PWD/autorandr $HOME/.config/autorandr

rm -rf "$HOME/.config/fontconfig"
ln -sf $PWD/fontconfig $HOME/.config/fontconfig

rm -rf "$HOME/.config/paru"
ln -sf "$PWD/paru" "$HOME/.config/paru"

rm -rf "$HOME/.config/pacmanfile"
ln -sf "$PWD/pacmanfile" "$HOME/.config/pacmanfile"
