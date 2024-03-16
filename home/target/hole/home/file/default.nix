config: pkgs: userConfig:

let
    makeLink = config.lib.file.mkOutOfStoreSymlink;
in

{
    ".unison/default.prf".text = import unison/default.prf.nix userConfig;
    ".moneydance/Documents/tnks.moneydance".source = makeLink
        "${config.home.homeDirectory}/doc/finance/moneydance";
}
