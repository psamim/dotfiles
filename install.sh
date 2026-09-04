#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

git submodule update --init --recursive --force
git submodule foreach --recursive git clean -ffdx

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
if [ ! -f "$HOME/.gitconfig.local" ]; then
  cp "$PWD/git/config.local.example" "$HOME/.gitconfig.local"
  echo "Created ~/.gitconfig.local from example — edit it for this machine."
fi

# tmux
ln -sf "$PWD/tmux/tmux.conf" "$HOME/.tmux.conf"
rm -rf "$HOME/.tmux"
ln -sf "$PWD/tmux" "$HOME/.tmux"
mkdir -p $HOME/.tmux/plugins
ln -sfn $PWD/tpm $HOME/.tmux/plugins/tpm
$HOME/.tmux/plugins/tpm/bin/install_plugins

# emacs
rm -rf "$HOME/.emacs.d"
rm -rf "$HOME/.doom.d"
ln -sf "$PWD/doom" "$HOME/.doom.d"

# zsh
ln -sf "$PWD/zsh/zshrc" "$HOME/.zshrc"
ln -sf "$PWD/zsh/p10k.zsh" "$HOME/.p10k.zsh"

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

# gpg
ln -sf $PWD/pam_environment $HOME/.pam_environment
ln -sf $PWD/gpg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf

# eslint
ln -sf $PWD/eslintrc.js $HOME/.eslintrc.js

# claude
mkdir -p "$HOME/.claude"
ln -sf "$PWD/agents/agents.md" "$HOME/.claude/CLAUDE.md"

# pacman
rm -rf $HOME/.config/pacman
ln -sf $PWD/pacman $HOME/.config/pacman

# ideavim
ln -sf $PWD/ideavim/ideavimrc $HOME/.ideavimrc

# desktop
mkdir -p  $HOME/.local/share/applications/
for f in $PWD/desktop-files/*.desktop; do
    ln -sf $f $HOME/.local/share/applications/
done

kwriteconfig5 --file ~/.config/kwinrc --group Windows --key BorderlessMaximizedWindows true

# alacritty
rm -rf $HOME/.config/alacritty
ln -sf $PWD/alacritty $HOME/.config/alacritty

# lazygit
mkdir -p $HOME/.config/lazygit
ln -sf $PWD/lazygit/config.yml $HOME/.config/lazygit/config.yml

# yazi
rm -rf $HOME/.config/yazi
ln -sf $PWD/yazi $HOME/.config/yazi
ya pkg install

# ghostty
mkdir -p $HOME/.config/ghostty
ln -sf $PWD/ghostty/config $HOME/.config/ghostty/config
rm -rf $HOME/.config/ghostty/shaders
ln -sf $PWD/ghostty/shaders $HOME/.config/ghostty/shaders

# workmux
mkdir -p "$HOME/.config/workmux"
ln -sf "$PWD/workmux/config.yaml" "$HOME/.config/workmux/config.yaml"

rm -rf "$HOME/.config/fontconfig"
ln -sf $PWD/fontconfig $HOME/.config/fontconfig

rm -rf "$HOME/.config/paru"
ln -sf "$PWD/paru" "$HOME/.config/paru"

rm -rf "$HOME/.config/pacmanfile"
ln -sf "$PWD/pacmanfile" "$HOME/.config/pacmanfile"

# tuicr
mkdir -p $HOME/.config/tuicr
ln -sf $PWD/tuicr/config.toml $HOME/.config/tuicr/config.toml
