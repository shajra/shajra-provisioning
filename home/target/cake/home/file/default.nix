config: pkgs: userConfig:

let
    makeLink = link: pkgs.runCommand "makeLink" {} "ln -s ${link} $out";
in

{
    ".moneydance/Documents/tnks.moneydance".source = makeLink
        "${config.home.homeDirectory}/doc/shared/sensitive/finance/moneydance";
}
