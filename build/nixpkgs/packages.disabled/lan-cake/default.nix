{
  nix-project-lib,
  bind,
  gnugrep,
  iproute2,
  iptables,
  nettools,
}:

let
  progName = "lan-cake";
  meta.description = "Change routing for home server when home";
in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    pathPackages = [
      bind.host
      gnugrep
      iproute2
      iptables
      nettools
    ];
  }
  ''
    set -eu
    set -o pipefail


    CAKE_NAME=cake.hajra.xyz
    CAKE_LOCAL=192.168.1.2
    ROUTER_IP=192.168.1.1
    ROUTER_MAC=ac:3b:77:45:fe:b0


    main()
    {
        if [ "''${1:-}" = home ]
        then
            echo "INFO: routing Cake locally"
            iptables_rule_add
        elif [ "''${1:-}" = remote ]
        then
            echo "INFO: routing Cake remotely"
            iptables_rule_remove
        elif is_online && at_home
        then
            echo "INFO: at home, routing Cake locally"
            iptables_rule_add
        else
            echo "INFO: not at home, routing Cake remotely"
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
            -d "$(cake_remote)"/32 \
            -p tcp \
            -j DNAT \
            --to-destination "$CAKE_LOCAL"
    }

    cake_remote()
    (
        ip="$(host -4 "$CAKE_NAME")"
        if ! [ "''${ip#;;}" = "$ip" ]
        then die "$CAKE_NAME IP lookup failed"
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
