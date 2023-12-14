{
    functions = {
        sft-tunnel = {
            description = "Tunnel using ScaleFT";
            body = ''
                sft ssh --local-port-forward $argv[1]:localhost:$argv[1] (whoami)
            '';
        };
    };

    shellAliases = {
        tunnel-grafana = ''sft-tunnel 4000'';
        tunnel-nike    = ''sft-tunnel 8428'';
    };
}
