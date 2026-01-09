#!/usr/bin/osascript

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title iTerm2+SSH+tmux: cake
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🍰

tell application "iTerm2"
    set newWindow to (create window with profile "tmux-cake")
end tell
