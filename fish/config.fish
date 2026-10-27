. ~/dev/arturo/dotfiles/fish/env.fish
. ~/dev/arturo/dotfiles/fish/aliases.fish
. ~/dev/arturo/dotfiles/fish/prompt.fish
. ~/dev/arturo/dotfiles/fish/theme.fish

# Machine-specific / work config, not tracked here
test -f ~/.config/fish/local.fish; and source ~/.config/fish/local.fish

# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init2.fish 2>/dev/null || :
