alias wow="git status"
alias such=git
alias very=git

alias vi=vim

function editor -d "the editor of choice. Edit on demand with ealias"
  atom $argv
end

function - -d "Goes back to the previous directory"
  cd -
end


function .. -d ".."
  cd ..
end

alias l="ls -lah"
alias cdp="cd ~/dev/projects/platanus"
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

function ddos
  if [ (count $argv) -gt 1 ]
    set times $argv[2]
  else
    set times 1000
  end

	for i in (seq $times)
		curl "$argv[1]"
	end
end

function extract
  if [ -f $argv[1] ]
    switch $argv[1]
      case '*.tar.bz2'
        tar xjf $argv[1]
      case '*.tar.gz'
        tar xzf $argv[1]
      case '*.bz2'
        bunzip2 $argv[1]
      case '*.rar'
        rar x $argv[1]
      case '*.gz'
        gunzip $argv[1]
      case '*.tar'
        tar xf $argv[1]
      case '*.tbz2'
        tar xjf $argv[1]
      case '*.tgz'
        tar xzf $argv[1]
      case '*.zip'
        unzip $argv[1]
      case '*.Z'
        uncompress $argv[1]
      case '*'
        echo "'$argv[1]' cannot be extracted via extract()" ;;
    end
  else
    echo "'$argv[1]' is not a valid file"
  end
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
alias gc="git checkout"
alias push="git push"
alias pusho="git push origin"

function pushb -d "git: pushes the current branch to the matching upstream"
  git push $argb[1]
end

function gac -d "git: add and commit with a message"
  git add -A
  git commit -m $argv[1]
end

function gacp -d "git: add and commit with a message, then pushes it"
  git add -A
  git commit -m $argv[1]
  git push
end

function gacb -d "git: add and commit with a message, then pushes it to a specific branch"
  git add -A
  git commit -m $argv[2]
  git push origin $argv[1]
end

function gap -d "git: adds, commits, pushes. accepts the branch as the first parameter"
  git add -A
  if [ (count $argv) -gt 1 ]
    git commit -m $argv[2]
    git push origin $argv[1]
  else
    git commit -m $argv[1]
    git push
  end
end

function gco -d "git: commit with a message"
  git commit -m $argv[1]
end

function prebase -d "adds a dummy commit and rebases it"
  git add .
  git commit -m "wip"
  git rebase -i $argv[1]
end

# Simulate Slow Connection
function makeslow
  sudo ipfw pipe 1 config bw 2500Kbit/s delay 30ms
  sudo ipfw add 1 pipe 1 src-port 80
  sudo ipfw add 2 pipe 1 dst-port 80
end

# Restore Normal Connection Speed
function makefast
  sudo ipfw delete 1
  sudo ipfw delete 2
end

function openrepo -d "open a repository location in github using the name of the remote"
  set repo (getrepo $argv[1])
  if [ $repo ]
    open $repo
  end
end

function getrepo -d "get url of this current repository in github based in the remote name"
  if [ (count $argv) -lt 1 ]
    set expected_remote_name origin
  else
    set expected_remote_name $argv[1]
  end

  set remotes (git remote -v)

  for remote in $remotes
    set remote_data_string (echo $remote | sed 's/\s+/ /g' | sed 's/(/ /g' | sed 's/)/ /g')
    eval "set remote_data $remote_data_string"
    set remote_name $remote_data[1]

    if [ $expected_remote_name = $remote_name ]
      set remote_address $remote_data[2]

      set http_part (echo $remote_address | grep -E "http:\/\/|https:\/\/")
      set github_http_part (echo $remote_address | grep -e "@github")

      if [ $http_part ]
        echo $remote_address
      else
        if [ $github_http_part ]
          echo (github_web_from_git_address $remote_address)
        end
      end

      break
    end
  end

end

function sudo!!
    eval sudo $history[1]
end
