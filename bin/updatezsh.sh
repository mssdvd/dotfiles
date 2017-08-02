#! /bin/sh

echo "zsh-autosuggestions"
cd ~/.zsh/zsh-autosuggestions
git pull --rebase

echo "zsh-completions"
cd ~/.zsh/zsh-completions
git pull --rebase

echo "zsh-syntax-highlighting"
cd ~/.zsh/zsh-syntax-highlighting
git pull --rebase

echo "zsh-history-substring-search"
cd ~/.zsh/zsh-history-substring-search
git pull --rebase

echo "zsh-completion-generator"
cd ~/.zsh/zsh-completion-generator
git pull --rebase
