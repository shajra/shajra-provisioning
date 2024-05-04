function __jj_changes
    jj log -r :: --no-graph \
        -T 'change_id.shortest() ++ "\t" ++  description.first_line() ++ "\n"'
end

function __jj_branches
    jj branch list -a | string split : -f 1
end

complete -f -c jj -s r -r -d 'Revision' -ka '( __jj_changes )'
complete -f -c jj -n '__fish_seen_subcommand_from show' -ka '(__jj_changes)'
complete -f -c jj -n '__fish_seen_subcommand_from branch set' -ka '(__jj_branches)'
complete -f -c jj -n '__fish_seen_subcommand_from branch track' -ka '(__jj_branches)'
complete -f -c jj -n '__fish_seen_subcommand_from new' -ka '(__jj_branches; __jj_changes)'
complete -f -c jj -n '__fish_seen_subcommand_from new' -s b -ka '(__jj_branches)'
