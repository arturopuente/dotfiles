#!/usr/bin/env bash
# macOS settings, captured from the old machine. Run with: bash macos.sh
# Some changes need a logout to fully apply.
set -euo pipefail

# Keyboard / typing ----------------------------------------------------------
defaults write -g ApplePressAndHoldEnabled -bool false        # key repeat instead of accent popup
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false
defaults write -g NSAutomaticCapitalizationEnabled -bool false
defaults write -g NSAutomaticPeriodSubstitutionEnabled -bool false

# Caps Lock -> Control on every keyboard. The System Settings remap is stored
# per keyboard ID, so the new laptop's keyboard wouldn't pick it up.
AGENT="$HOME/Library/LaunchAgents/local.capslock-to-control.plist"
mkdir -p "$(dirname "$AGENT")"
cat > "$AGENT" <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>local.capslock-to-control</string>
  <key>ProgramArguments</key>
  <array>
    <string>/usr/bin/hidutil</string><string>property</string><string>--set</string>
    <string>{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x7000000E4}]}</string>
  </array>
  <key>RunAtLoad</key><true/>
</dict>
</plist>
EOF
launchctl bootout "gui/$(id -u)" "$AGENT" 2>/dev/null || true
launchctl bootstrap "gui/$(id -u)" "$AGENT"

# Trackpad -------------------------------------------------------------------
defaults write com.apple.AppleMultitouchTrackpad Clicking -bool true     # tap to click
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write -g com.apple.mouse.tapBehavior -int 2
defaults write -g com.apple.swipescrolldirection -bool false             # "natural" scrolling off
defaults write -g com.apple.trackpad.scaling -float 1.5

# Appearance -----------------------------------------------------------------
defaults write -g AppleShowScrollBars -string WhenScrolling
defaults write -g NSTableViewDefaultSizeMode -int 2                      # medium sidebar icons

# Dock -----------------------------------------------------------------------
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0.5
defaults write com.apple.dock tilesize -int 128
defaults write com.apple.dock magnification -bool false
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock mru-spaces -bool false                     # don't reorder Spaces
defaults write com.apple.dock wvous-br-corner -int 1                     # no Quick Note hot corner

# Finder ---------------------------------------------------------------------
defaults write com.apple.finder ShowPathbar -bool true
defaults write com.apple.finder ShowStatusBar -bool true
defaults write com.apple.finder FXPreferredViewStyle -string Nlsv       # list view
defaults write com.apple.finder NewWindowTarget -string PfHm            # new windows open ~
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool false

# Window manager -------------------------------------------------------------
defaults write com.apple.WindowManager EnableStandardClickToShowDesktop -bool false
defaults write com.apple.WindowManager EnableTiledWindowMargins -bool false

killall Dock Finder SystemUIServer 2>/dev/null || true
echo "macOS settings applied. Log out and back in for keyboard/trackpad changes."
