function git
  /usr/local/bin/git $argv; 
end

alias wow="git status"
alias such=git
alias very=git

function editor -d "the editor of choice. Edit on demand with ealias"
  subl $argv
end

function .. -d ".."
  cd ..
end

alias l="ls -lah"
alias cdp="cd ~/dev/projects"
alias cdd="cd ~/Downloads"

function fixscreen -d "Fixes my screen resolution"
  xrandr --newmode "1360x706_60"   77.25  1360 1424 1560 1760  706 709 719 734 -hsync +vsync
  xrandr --addmode VBOX0 1360x706_60
  xrandr --output VBOX0 1360x706_60
end

function reload -d "reload aliases and env"
  . ~/dev/dotfiles/fish/aliases.fish
  . ~/dev/dotfiles/fish/env.fish
end

alias epath="editor ~/dev/dotfiles/fish/env.fish"
alias ealias="editor ~/dev/dotfiles/fish/aliases.fish"

function patremove -d "Remove all files with a given pattern"
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
alias bi="bundle install"
alias bu="bundle update"
alias rage="bundle exec rake"
alias gemi="gem install --no-rdoc --no-ri"
alias ru="rackup config.ru"
alias pryrails="pry -r ./config/environment"
alias ra="rails server"
alias rae="rails server -e production"
alias rac="rails console"

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

function gao -d "git: add, commit with a message and push to the default origin master"
  git add -A
  git commit -m $argv[1]
  git push
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

function gco -d "git: commit with a message"
  git commit -m $argv[1]
end

function url_final_part -d "get the final part of a string separated by /"
  set str_parts (echo $argv[1] | sed 's/\//\ /g')
  eval "set parts $str_parts"
  echo $parts[-1..-1]
end

function download -d "download a file in the current dir from the url $1"
  curl -o (url_final_part $argv[1]) $argv[1]
end

function get_jquery -d "jquery"
  download http://code.jquery.com/jquery.min.js
end

function get_underscore -d "underscore.js"
  download http://underscorejs.org/underscore.js
end

function get_backbone -d "backbone.js"
  download http://documentcloud.github.io/backbone/backbone.js
end

function get_es5 -d "es5shims.js"
  download https://raw.github.com/kriskowal/es5-shim/master/es5-shim.min.js
end

function get_normalize -d "normalize.css"
  download http://necolas.github.io/normalize.css/2.1.2/normalize.css
end

# Finder
function finder -d "open in Finder"
  open -a 'Finder' $argv[1]
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

# Browsers
function safari -d "open in Safari"
  open -a "Safari" $argv[1]
end

function chrome -d "open in Chrome"
  open -a "Google Chrome" $argv[1]
end

alias rdb="bundle exec rake db:migrate"
alias rdba="bundle exec rake db:migrate; env RAILS_ENV=test bundle exec rake db:migrate"

# ok, this is super lazy
alias py="python"
alias rb="ruby"

function localhost -d "open localhost in a given port"
  open "http://localhost:$argv[1]"
end

function lt -d "open LightTable"
  open -a "/Applications/LightTable.app/" $argv[1]
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

function github_web_from_git_address -d "transforms a github git address to a https url"
  set address_parts_string (echo $argv[1] | sed 's/:/ /g')
  eval "set address_parts $address_parts_string"
  set repo_data $address_parts[2]
  
  set repo_parts_string (echo $repo_data | sed 's/\// /g')
  eval "set repo_parts $repo_parts_string"
  
  set username $repo_parts[1]
  set reponame_with_extension $repo_parts[2]
  
  set reponame_parts_string (echo $reponame_with_extension | sed 's/\./ /g')
  eval "set reponame_parts $reponame_parts_string"
  set reponame $reponame_parts[1]
  
  printf "https://github.com/%s/%s" $username $reponame
end

# mosh
function moshi
  mosh dev
end
