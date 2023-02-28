config: pkgs: userConfig:

let
    makeLink = link: pkgs.runCommand "makeLink" {} "ln -s ${link} $out";
in

{
    ".unison/default.prf".text = import unison/default.prf.nix userConfig;
    ".moneydance/Documents/tnks.moneydance".source = makeLink
        "${config.home.homeDirectory}/doc/shared/sensitive/finance/moneydance";
}
