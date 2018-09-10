#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

# aria2
mkdir -p "$HOME/.aria2"
ln -sf $PWD/aria2/* $HOME/.aria2/

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
ln -sf "$PWD/emacs.d" "$HOME/.emacs.d"
ln -sf "$PWD/spacemacs/exclude" "$PWD/.git/modules/emacs.d/info/exclude"
ln -sf "$PWD/spacemacs/spacemacs" "$HOME/.spacemacs"
rm -rf "$HOME/.emacs.d/private"
ln -sf "$PWD/spacemacs/private" "$HOME/.emacs.d/private"

# vrapperrc
ln -sf "$PWD/vrapper/vrapperrc.vim" "$HOME/.vrapperrc"

# zsh
ln -sf "$PWD/zsh/zshrc" "$HOME/.zshrc"

# Vimperator
rm -rf "$HOME/.vimperator"
ln -sf "$PWD/vimperator" "$HOME/.vimperator"
ln -sf "$PWD/vimperator/vimperatorrc" "$HOME/.vimperatorrc"

# Muttator
rm -rf "$HOME/.muttator"
ln -sf "$PWD/muttator" "$HOME/.muttator"
ln -sf "$PWD/muttator/muttatorrc" "$HOME/.muttatorrc"

# i3
rm -rf "$HOME/.i3"
ln -sf "$PWD/i3" "$HOME/.i3"
ln -sf "$PWD/i3/i3blocks.conf" "$HOME/.i3blocks.conf"

# compton
ln -sf "$PWD/compton/compton.conf" "$HOME/.compton"

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
rm -rf "$HOME/.password-store"
ln -sf "$PWD/password-store" "$HOME/.password-store"

# systemd
rm -rf "$HOME/.config/systemd"
mkdir -p "$HOME/.config/systemd/"
cp -r "$PWD/systemd" "$HOME/.config/systemd/user"

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
