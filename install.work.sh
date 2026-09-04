#!/usr/bin/env bash
# This scripts installs all conf files
# by creating appropriate symbolic links

git submodule update --init --recursive --force
git submodule foreach --recursive git checkout -- .

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

# ideavim
ln -sf $PWD/ideavim/ideavimrc $HOME/.ideavimrc

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

# Karabiner
rm -rf $HOME/.config/karabiner
ln -sf $PWD/karabiner $HOME/.config/karabiner

# workmux
mkdir -p "$HOME/.config/workmux"
ln -sf "$PWD/workmux/config.yaml" "$HOME/.config/workmux/config.yaml"

# emacs-plus
mkdir -p "$HOME/.config/emacs-plus"
ln -sf "$PWD/emacs-plus/build.yml" "$HOME/.config/emacs-plus/build.yml"

# Code
ln -sf $PWD/vscode/keybindings.json "$HOME/Library/Application Support/Code/User/keybindings.json"
ln -sf $PWD/vscode/settings.json "$HOME/Library/Application Support/Code/User/settings.json"

# claude
mkdir -p "$HOME/.claude"
ln -sf "$PWD/agents/agents.md" "$HOME/.claude/CLAUDE.md"

# eslint
ln -sf $PWD/eslintrc.js $HOME/.eslintrc.js

# ghostty
mkdir -p $HOME/.config/ghostty
ln -sf $PWD/ghostty/config $HOME/.config/ghostty/config
rm -rf $HOME/.config/ghostty/shaders
ln -sf $PWD/ghostty/shaders $HOME/.config/ghostty/shaders

# tuicr
mkdir -p $HOME/.config/tuicr
ln -sf $PWD/tuicr/config.toml $HOME/.config/tuicr/config.toml
