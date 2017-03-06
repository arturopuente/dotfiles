alias vi=vim

function editor -d "the editor of choice. Edit on demand with ealias"
  code $argv
end

function .. -d ".."
  cd ..
end

function pack
  set revision $argv[1]
  git diff $revision --name-only | tar -jcvf archive_$revision.tar.bz2 -T -
end

alias l="ls -lah"
alias cdpm="cd ~/dev/projects/me"
alias cdp="cd ~/dev/projects/srburns"
alias cdd="cd ~/Downloads"

function reload -d "reload aliases and env"
  . ~/dev/dotfiles/fish/aliases.fish
  . ~/dev/dotfiles/fish/env.fish
end

alias epath="editor ~/dev/dotfiles/fish/env.fish"
alias ealias="editor ~/dev/dotfiles/fish/aliases.fish"

function pathremove -d "Remove all files with a given pattern"
  find . -name $argv[1] -type f -delete
end

function extremove -d "Remove all files with a given extension"
  find . -name "*.$argv[1]" -type f -delete
end

function wmount -d "Mounts a Windows shared folder"
  sudo mount -t vboxsf $argv[1] /media/windows-shared/
end

function take
  mkdir $argv[1]
  cd $argv[1]
end

# Ruby
alias b="bundle exec"
alias bu="bundle update"
alias rage="bundle exec rake"
alias gemi="gem install --no-rdoc --no-ri"

# Trash
function trash -d "send a file to the trash"
  mv $argv[1] ~/.Trash
end

# Git
alias gs="git status"
alias gd="git diff"

function prebase -d "adds a dummy commit and rebases it"
  git add .
  git commit -m "wip"
  git rebase -i $argv[1]
end
