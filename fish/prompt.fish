. ~/dev/dotfiles/fish/ext/git_prompt.fish

function fish_prompt
  # pwd
  set_color yellow --bold
  printf '%s' (echo $PWD | sed -e "s|^$HOME|~|")

  # git
  printf ' '
  __git_prompt

  set_color normal
  set_color yellow
  printf '> '
end
