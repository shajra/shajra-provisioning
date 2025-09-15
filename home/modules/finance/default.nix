{ build, config, ... }:

let
  makeLink = config.lib.file.mkOutOfStoreSymlink;
in

{
  imports = [ ../ubiquity ];
  home.extraPackages = build.pkgs.lists.finance;
  home.file.".moneydance/Documents/tnks.moneydance".source =
    makeLink "${config.home.homeDirectory}/doc/finance/moneydance";
}
