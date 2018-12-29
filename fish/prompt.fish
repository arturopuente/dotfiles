. ~/dev/dotfiles/fish/ext/git_prompt.fish

# Prompt
set normal (set_color normal)
set yellow (set_color yellow)

function prompt_pwd --description 'Print the current working directory, shortend to fit the prompt'
  echo $PWD | sed -e "s|^$HOME|~|"
end

function fish_prompt
  # pwd
  set_color yellow --bold
  printf '%s' (prompt_pwd)

  # git
  printf ' '
  __git_prompt

  set_color normal
  set_color yellow
  printf '> '
end
