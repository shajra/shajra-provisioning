config: pkgs: shared:

let
    makeLink = link: pkgs.runCommand "makeLink" {} "ln -s ${link} $out";
in

{
    ".unison/default.prf".text = import unison/default.prf.nix shared;
    ".moneydance/Documents/tnks.moneydance".source = makeLink
        "${config.home.homeDirectory}/doc/shared/sensitive/finance/moneydance";
}
