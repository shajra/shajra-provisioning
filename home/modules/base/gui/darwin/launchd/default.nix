config: pkgs:

let

    format = pkgs.lib.colors.format "0xff%R%G%B";
    colors = pkgs.lib.colors.transformColors format config.theme.colors;

in {
    enable = true;
    agents.sketchybar-cpu = {
        enable = true;
        config = {
            EnvironmentVariables = {
                "COLOR_EXTREME" = colors.semantic.urgent;
                "COLOR_HIGH" = colors.semantic.warning;
                "COLOR_NORMAL" = colors.semantic.info;
                "COLOR_LOW" = colors.semantic.good;
            };
            ProgramArguments = [
                "${pkgs.sketchybar-cpu}/bin/sketchybar-cpu"
                "git.felix.cpu"
            ];
        };
    };
}
