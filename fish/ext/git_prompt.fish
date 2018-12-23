set -gx fish_color_git_clean green
set -gx fish_color_git_staged yellow
set -gx fish_color_git_dirty red

function __git_prompt --description 'Write out the git prompt'
  set -l branch (git rev-parse --abbrev-ref HEAD ^/dev/null)
  if test -z $branch
    return
  end

  set -l index (git status --porcelain ^/dev/null|cut -c 1-2|sort -u)

  if test -z "$index"
    set_color $fish_color_git_clean
    echo -n $branch' '
    set_color normal
    return
  end

  set -l staged

  for i in $index
    if echo $i | grep '^[AMRCD]' >/dev/null
      set staged 1
    end
  end

  if set -q staged[1]
    set_color $fish_color_git_staged
  else
    set_color $fish_color_git_dirty
  end

  echo -n $branch' '

  set_color normal
end
