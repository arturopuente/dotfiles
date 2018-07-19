alias vi=vim

function editor -d "the editor of choice. Edit on demand with ealias"
  vim $argv
end

alias l="ls -lah"
alias cdpm="cd ~/dev/projects/me"
alias cdp="cd ~/dev/projects/able"
alias cdd="cd ~/Downloads"

function reload -d "reload aliases and env"
  . ~/dev/dotfiles/fish/aliases.fish
  . ~/dev/dotfiles/fish/env.fish
end

alias epath="editor ~/dev/dotfiles/fish/env.fish"
alias ealias="editor ~/dev/dotfiles/fish/aliases.fish"

function take
  mkdir $argv[1]
  cd $argv[1]
end

# Ruby
alias b="bundle exec"
alias bu="bundle update"
alias rage="bundle exec rake"

# Git
alias gs="git status"
alias gd="git diff"
alias gl="git lg"

function prebase -d "adds a dummy commit and rebases it"
  git add .
  git commit -m "wip"
  git rebase -i $argv[1]
end
