config: pkgs:

let

    fish  = "${config.programs.fish.package}/bin/fish";
    makeLink = config.lib.file.mkOutOfStoreSymlink;
    darwinFiles =  {
        "Library/Application Support/jj".source =
            makeLink "${config.xdg.configHome}/jj";
    };
    darwinConfig =
        if pkgs.stdenv.hostPlatform.isDarwin
        then darwinFiles
        else {};

in

{
    ".kshrc".text = import ksh/kshrc.nix fish;
    ".nix-channels".source = ./nix-channels;
    ".haskeline".text = "editMode: Vi";
} // darwinConfig
