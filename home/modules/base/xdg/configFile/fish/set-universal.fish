set solarized_base03    "002b36"
set solarized_base02    "073642"
set solarized_base01    "586e75"    # emphasized text
set solarized_base00    "657b83"    # standard text
set solarized_base0     "839496"
set solarized_base1     "93a1a1"    # shadowed comments
set solarized_base2     "eee8d5"    # background highlights
set solarized_base3     "fdf6e3"    # background
set solarized_yellow    "b58900"
set solarized_orange    "cb4b16"
set solarized_red       "dc322f"
set solarized_magenta   "d33682"
set solarized_violet    "6c71c4"
set solarized_blue      "268bd2"
set solarized_cyan      "2aa198"
set solarized_green     "859900"

set --export --universal FZF_DEFAULT_OPTS "
    --cycle
    --layout=reverse
    --border
    --height=90%
    --preview-window=wrap
    --bind='ctrl-p:toggle-preview'
    --color=fg:#$solarized_base00
    --color=fg+:#$solarized_base01
    --color=bg:#$solarized_base3
    --color=bg+:#$solarized_base2
    --color=hl:#$solarized_base02
    --color=hl+:#$solarized_base03
    --color=info:#$solarized_base1
    --color=prompt:#$solarized_green
    --color=pointer:#$solarized_blue
    --color=marker:#$solarized_magenta
    --color=spinner:#$solarized_cyan
    --color=header:#$solarized_base1
"

set --universal fish_color_autosuggestion $solarized_base1
set --universal fish_color_cancel -r
set --universal fish_color_command $solarized_base01
set --universal fish_color_comment $solarized_base1
set --universal fish_color_cwd $solarized_green
set --universal fish_color_cwd_root $solarized_red
set --universal fish_color_end $solarized_blue
set --universal fish_color_error $solarized_red
set --universal fish_color_escape $solarized_base03
set --universal fish_color_history_current --bold
set --universal fish_color_host normal
set --universal fish_color_match --background=$solarized_cyan
set --universal fish_color_normal normal
set --universal fish_color_operator $solarized_base03
set --universal fish_color_param $solarized_base00
set --universal fish_color_quote $solarized_base0
set --universal fish_color_redirection $solarized_violet
set --universal fish_color_search_match $solarized_yellow --background=$solarized_base2
set --universal fish_color_selection $solarized_base3 --bold --background=$solarized_base03
set --universal fish_color_user $solarized_cyan
set --universal fish_color_valid_path --underline
set --universal fish_greeting
set --universal fish_pager_color_completion $solarized_green
set --universal fish_pager_color_description $solarized_yellow
set --universal fish_pager_color_prefix $solarized_cyan --underline
set --universal fish_pager_color_progress $solarized_base3 --background=$solarized_cyan
