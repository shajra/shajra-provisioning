config: pkgs:

let
    makeLink = link: pkgs.runCommand "makeLink" {} "ln -s ${link} $out";
in

{
    ".unison/default.prf".source = unison/default.prf;
    ".moneydance/Documents/tnks.moneydance".source = makeLink
        "${config.home.homeDirectory}/doc/shared/sensitive/finance/moneydance";
}
