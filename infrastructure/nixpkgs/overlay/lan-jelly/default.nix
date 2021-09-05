self: super:

let
    progName = "lan-jelly";
    meta.description = "Change routing for home server when home";
in

self.nix-project-lib.writeShellCheckedExe progName
{
    inherit meta;
    path = with self; [
        bind.host
        gnugrep
        iproute
        iptables
        nettools
    ];
}
''
set -eu
set -o pipefail


JELLY_NAME=jelly.hajra.xyz
JELLY_LOCAL=192.168.1.2
ROUTER_IP=192.168.1.1
ROUTER_MAC=ac:3b:77:45:fe:b0


main()
{
    if is_online && at_home
    then
        echo "INFO: at home, routing Jelly locally"
        iptables_rule_add
    else
        echo "INFO: not at home, routing Jelly remotely"
        iptables_rule_remove
    fi
}

is_online()
(
    routes="$(ip route show scope global)"
    test -n "$routes"
)

at_home()
{
    arp -a "$ROUTER_IP" | grep -q "$ROUTER_MAC"
}

iptables_rule_add()
{
    if ! iptables_rule -C >/dev/null 2>&1
    then
        iptables_rule -A
        echo "INFO: iptables modified"
    fi
}

iptables_rule_remove()
{
    while iptables_rule -C >/dev/null 2>&1
    do
        iptables_rule -D
        echo "INFO: iptables modified"
    done
}

iptables_rule()
{
    iptables \
        -t nat \
        "$1" OUTPUT \
        -d "$(jelly_remote)"/32 \
        -p tcp \
        -j DNAT \
        --to-destination "$JELLY_LOCAL"
}

jelly_remote()
(
    ip="$(host -4 "$JELLY_NAME")"
    if ! [ "''${ip#;;}" = "$ip" ]
    then die "$JELLY_NAME IP lookup failed"
    fi
    echo "''${ip##* }"
)

die()
(
    msg="$1"
    echo "ERROR: $msg" >&2
    exit 1
)


main "$@"
''
