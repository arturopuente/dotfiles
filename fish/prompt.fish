#. ~/dev/arturo/dotfiles/fish/ext/git_prompt.fish

function fish_prompt
  # pwd
  set_color red --bold

  set -g fish_prompt_pwd_dir_length 0
  set -g __fish_git_prompt_show_informative_status 1
  set -g __fish_git_prompt_showcolorhints 1

  printf "%s%s" (prompt_pwd) (fish_git_prompt)

  set_color normal
  printf "\n\$ "
end

function fish_title
  set -q argv[1]; or set argv fish
  set -g fish_prompt_pwd_dir_length 0

  echo (prompt_pwd) $argv;
end

function find_file
    set -q argv[1]; or set argv[1] "."
    vterm_cmd find-file (realpath "$argv")
end
