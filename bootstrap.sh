#!/usr/bin/env bash
# Set up a fresh Mac from this repo. Run with: bash bootstrap.sh
# Safe to re-run: existing files are moved to *.bak.<timestamp>, never deleted.
set -euo pipefail

DOTFILES="$HOME/dev/arturo/dotfiles"
TS=$(date +%s)

# 1. Prerequisites ----------------------------------------------------------
xcode-select -p >/dev/null 2>&1 || { xcode-select --install; echo "Re-run after Xcode CLT finishes."; exit 1; }
if ! command -v brew >/dev/null; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# 2. Repo -------------------------------------------------------------------
# HTTPS so this works before an SSH key is set up on the new machine.
[ -d "$DOTFILES" ] || git clone https://github.com/arturopuente/dotfiles.git "$DOTFILES"

# 3. Packages ---------------------------------------------------------------
brew bundle --file="$DOTFILES/Brewfile" || echo "!! Some Brewfile entries failed; see above."

# Private files (licensed fonts, app data) live in iCloud Drive, not this repo.
ICLOUD_SETUP="$HOME/Library/Mobile Documents/com~apple~CloudDocs/mac-setup"

# Unzip an iCloud zip into a directory, downloading it from iCloud first if needed.
icloud_unzip() {
  local zip="$ICLOUD_SETUP/$1" dest="$2"
  if [ -e "$zip" ] || brctl download "$zip" 2>/dev/null; then
    for _ in $(seq 30); do unzip -tq "$zip" >/dev/null 2>&1 && break; sleep 2; done
    mkdir -p "$dest"
    unzip -oq "$zip" -d "$dest" && echo "restored $1 from iCloud" && return 0
    echo "!! $1 not ready yet; re-run bootstrap once iCloud finishes syncing."
  else
    echo "!! $zip not found; sign in to iCloud Drive or copy it manually."
  fi
  return 1
}

icloud_unzip fonts.zip "$HOME/Library/Fonts" || true

# FluidVoice: settings + custom vocabulary + history. Models re-download on first launch.
FV_TMP=$(mktemp -d)
if ! pgrep -xq FluidVoice && icloud_unzip fluidvoice.zip "$FV_TMP"; then
  defaults import com.FluidApp.app "$FV_TMP/com.FluidApp.app.plist"
  mkdir -p "$HOME/Library/Application Support/FluidVoice"
  cp -n "$FV_TMP"/FluidVoice/* "$HOME/Library/Application Support/FluidVoice/" 2>/dev/null || true
fi
rm -rf "$FV_TMP"

# iTerm2 + Magnet: exported preference plists. Import before the apps first run.
PREFS_TMP=$(mktemp -d)
if icloud_unzip app-prefs.zip "$PREFS_TMP"; then
  for plist in "$PREFS_TMP"/*.plist; do
    domain=$(basename "$plist" .plist)
    case "$domain" in
      com.googlecode.iterm2) app=iTerm2 ;;
      com.crowdcafe.windowmagnet) app=Magnet ;;
      *) app="" ;;
    esac
    if [ -n "$app" ] && pgrep -xq "$app"; then echo "!! $app is running; skipped its settings"; continue; fi
    defaults import "$domain" "$plist" && echo "imported $domain"
  done
fi
rm -rf "$PREFS_TMP"

# Alfred: preferences bundle (workflows, hotkeys, themes) + Powerpack license.
# Clipboard history is deliberately not transferred.
ALFRED_DIR="$HOME/Library/Application Support/Alfred"
if ! pgrep -xq Alfred && [ ! -d "$ALFRED_DIR/Alfred.alfredpreferences" ]; then
  icloud_unzip alfred.zip "$ALFRED_DIR" || true
fi

# brew doesn't put Emacs.app in /Applications on its own
EMACS_APP=/opt/homebrew/opt/emacs-plus@31/Emacs.app
[ -d "$EMACS_APP" ] && [ ! -e /Applications/Emacs.app ] && ln -s "$EMACS_APP" /Applications/Emacs.app || true

# Switchboard's notifications: terminal-notifier stays silent until macOS has
# asked to allow it, and only launching it as an app brings up that prompt.
NOTIFIER_APP=/opt/homebrew/opt/terminal-notifier/terminal-notifier.app
if [ -d "$NOTIFIER_APP" ] && ! terminal-notifier -diagnose 2>/dev/null | grep -q 'authorization *authorized'; then
  open -W -a "$NOTIFIER_APP" --args -title Switchboard -message "Notifications are on" || true
  echo "Allow terminal-notifier's notifications if macOS asks."
fi

# 4. Symlinks ---------------------------------------------------------------
link() {
  local src="$DOTFILES/$1" dest="$HOME/$2"
  mkdir -p "$(dirname "$dest")"
  if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$src" ]; then return; fi
  [ -e "$dest" ] || [ -L "$dest" ] && mv "$dest" "$dest.bak.$TS"
  ln -s "$src" "$dest"
  echo "linked $dest"
}

link gitconfig                   .gitconfig
link fish/config.fish            .config/fish/config.fish
link fish/fish_plugins           .config/fish/fish_plugins
link .config/doom                .config/doom
link .config/zed/settings.json   .config/zed/settings.json
link .config/helix/config.toml   .config/helix/config.toml
link .config/git/ignore          .config/git/ignore
link .config/ghostty/config      "Library/Application Support/com.mitchellh.ghostty/config"
link vscode/settings.json        "Library/Application Support/Code/User/settings.json"
link claude/settings.json        .claude/settings.json

# 5. fish as login shell ----------------------------------------------------
FISH=/opt/homebrew/bin/fish
if [ -x "$FISH" ]; then
  grep -qx "$FISH" /etc/shells || echo "$FISH" | sudo tee -a /etc/shells >/dev/null
  [ "$(dscl . -read ~/ UserShell | awk '{print $2}')" = "$FISH" ] || chsh -s "$FISH"
  # fisher, then everything listed in fish_plugins
  "$FISH" -c 'type -q fisher || curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source; fisher update'
  "$FISH" -c 'set -U nvm_default_version v24; nvm install v24'
fi

# 6. Ruby ------------------------------------------------------------------
[ -d "$HOME/.rubies/ruby-4.0.9" ] || ruby-install --no-reinstall ruby 4.0.9
[ -f "$HOME/.ruby-version" ] || echo "4.0.9" > "$HOME/.ruby-version"

# 7. macOS settings ----------------------------------------------------------
bash "$DOTFILES/macos.sh"

echo
echo "Done (log out/in for keyboard + trackpad). Next: Doom Emacs setup is in README.org; SSH/AWS/kube creds are not in this repo."
