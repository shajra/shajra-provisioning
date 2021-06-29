set --universal fish_color_autosuggestion 93a1a1
set --universal fish_color_cancel -r
set --universal fish_color_command 586e75
set --universal fish_color_comment 93a1a1
set --universal fish_color_cwd green
set --universal fish_color_cwd_root red
set --universal fish_color_end 268bd2
set --universal fish_color_error dc322f
set --universal fish_color_escape 00a6b2
set --universal fish_color_history_current --bold
set --universal fish_color_host normal
set --universal fish_color_match --background=brblue
set --universal fish_color_normal normal
set --universal fish_color_operator 00a6b2
set --universal fish_color_param 657b83
set --universal fish_color_quote 839496
set --universal fish_color_redirection 6c71c4
set --universal fish_color_search_match bryellow --background=white
set --universal fish_color_selection white --bold --background=brblack
set --universal fish_color_user brgreen
set --universal fish_color_valid_path --underline
set --universal fish_greeting
set --universal fish_pager_color_completion green
set --universal fish_pager_color_description B3A06D
set --universal fish_pager_color_prefix cyan --underline
set --universal fish_pager_color_progress brwhite --background=cyan

for d in ~/src/live/*/bin
    fish_add_path $d
end
