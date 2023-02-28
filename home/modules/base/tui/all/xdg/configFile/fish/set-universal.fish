set --export --universal FZF_DEFAULT_OPTS "
    --cycle
    --layout=reverse
    --border
    --height=90%
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

set --universal fish_color_autosuggestion @theme_foreground_sh@
set --universal fish_color_cancel -r
set --universal fish_color_command @theme_foreground_em@
set --universal fish_color_comment @theme_foreground_sh@
set --universal fish_color_cwd @theme_green@
set --universal fish_color_cwd_root @theme_red@
set --universal fish_color_end @theme_blue@
set --universal fish_color_error @theme_red@
set --universal fish_color_escape @theme_inv_background@
set --universal fish_color_history_current --bold
set --universal fish_color_host normal
set --universal fish_color_match --background=@theme_cyan@
set --universal fish_color_normal normal
set --universal fish_color_operator @theme_inv_background@
set --universal fish_color_param @theme_foreground@
set --universal fish_color_quote @theme_inv_foreground@
set --universal fish_color_redirection @theme_violet@
set --universal fish_color_search_match @theme_yellow@ --background=@theme_background_hl@
set --universal fish_color_selection @theme_background@ --bold --background=@theme_inv_background@
set --universal fish_color_user @theme_cyan@
set --universal fish_color_valid_path --underline
set --universal fish_greeting
set --universal fish_pager_color_completion @theme_green@
set --universal fish_pager_color_description @theme_yellow@
set --universal fish_pager_color_prefix @theme_cyan@ --underline
set --universal fish_pager_color_progress @theme_background@ --background=@theme_cyan@
