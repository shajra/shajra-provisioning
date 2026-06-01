set --export --universal FZF_DEFAULT_OPTS "
    --cycle
    --layout=reverse
    --border
    --height=90%
    --preview='@preview_file@ {}'
    --preview-window=wrap
    --bind='ctrl-p:toggle-preview'
    --color=fg:#@theme_foreground@
    --color=fg+:#@theme_foreground_em@
    --color=bg:#@theme_background@
    --color=bg+:#@theme_background_hl@
    --color=hl:#@theme_inv_background_hl@
    --color=hl+:#@theme_inv_background@
    --color=info:#@theme_foreground_sh@
    --color=prompt:#@theme_green@
    --color=pointer:#@theme_blue@
    --color=marker:#@theme_magenta@
    --color=spinner:#@theme_cyan@
    --color=header:#@theme_foreground_sh@
"
set --universal fish_greeting
