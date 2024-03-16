config: pkgs: userConfig:

let
    makeLink = config.lib.file.mkOutOfStoreSymlink;
in

{
    ".moneydance/Documents/tnks.moneydance".source = makeLink
        "${config.home.homeDirectory}/doc/finance/moneydance";
}
