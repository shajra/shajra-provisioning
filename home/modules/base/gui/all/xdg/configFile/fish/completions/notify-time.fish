complete --no-files notify-time
complete notify-time \
    -d Command \
    -a '(__fish_complete_subcommand -- -o --ou tput -f --format)'
