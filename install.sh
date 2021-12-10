#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

# aria2
mkdir -p "$HOME/.aria2"
ln -sf $PWD/aria2/* $HOME/.aria2/

# tridactyl
rm -f "$HOME/.tridactylrc"
ln -sf $PWD/tridactyl/tridactylrc $HOME/.tridactylrc 

# roffi pass
mkdir -p "$HOME/.config/rofi-pass"
ln -sf $PWD/rofi-pass/config $HOME/.config/rofi-pass/config

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

# Muttator
rm -rf "$HOME/.muttator"
ln -sf "$PWD/muttator" "$HOME/.muttator"
ln -sf "$PWD/muttator/muttatorrc" "$HOME/.muttatorrc"

# i3
rm -rf "$HOME/.i3"
ln -sf "$PWD/i3" "$HOME/.i3"
ln -sf "$PWD/i3/i3blocks.conf" "$HOME/.i3blocks.conf"

# picomm
mkdir -p "$HOME/.config/picom"
ln -sf $PWD/picom/picom.conf $HOME/.config/picom/picom.conf

# X
ln -sf "$PWD/X/Xresources" "$HOME/.Xresources"
ln -sf "$PWD/X/xinitrc" "$HOME/.xinitrc"

# dunst
rm -rf "$HOME/.config/dunst"
ln -sf "$PWD/dunst" "$HOME/.config/dunst"

# phpcs
# rm -rf "$HOME/.phpcs"
# ln -sf "$PWD/phpcs" "$HOME/.phpcs"
# ~/.phpcs/commands.sh

# password-store
# rm -rf "$HOME/.password-store"
# ln -sf "$PWD/password-store" "$HOME/.password-store"

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

# yaourt
rm -rf $HOME/.config/yaourt
ln -sf $PWD/yaourt $HOME/.config/yaourt

# ideavim
ln -sf $PWD/ideavim/ideavimrc $HOME/.ideavimrc

# desktop
mkdir -p  $HOME/.local/share/applications/
for f in $PWD/desktop-files/*.desktop; do
    ln -sf $f $HOME/.local/share/applications/
done


kwriteconfig5 --file ~/.config/kwinrc --group Windows --key BorderlessMaximizedWindows true

# xmonad
mkdir -p "$HOME/.xmonad"
ln -sf $PWD/xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs
ln -sf $PWD/xmobar/xmobarrc $HOME/.xmobarrc

# rofi
rm -rf $HOME/.config/rofi
ln -sf $PWD/rofi $HOME/.config/rofi

# alacritty
rm -rf $HOME/.config/alacritty
ln -sf $PWD/alacritty $HOME/.config/alacritty

# autorandr
rm -rf $HOME/.config/autorandr
ln -sf $PWD/autorandr $HOME/.config/autorandr

rm -rf $HOME/.mbsyncrc
ln -sf $PWD/mbsync/mbsyncrc $HOME/.mbsyncrc

rm -rf $HOME/.config/astroid
ln -sf $PWD/astroid $HOME/.config/astroid

rm -rf $HOME/.notmuch-config
ln -sf $PWD/notmuch/notmuch-config $HOME/.notmuch-config

rm -rf "$HOME/.config/fontconfig"
ln -sf $PWD/fontconfig $HOME/.config/fontconfig

rm -rf "$HOME/.config/polybar"
ln -sf $PWD/polybar $HOME/.config/polybar

rm -rf "$HOME/.config/kmonad"
ln -sf $PWD/kmonad $HOME/.config/kmonad

mkdir -p "$HOME/.config/awesome"
ln -sf $PWD/awesomewm/rc.lua $HOME/.config/awesome/rc.lua

rm -rf "$HOME/.config/awesome"
ln -sf "$PWD/awesomewm" "$HOME/.config/awesome"

rm -rf "$HOME/.config/paru"
ln -sf "$PWD/paru" "$HOME/.config/paru"
