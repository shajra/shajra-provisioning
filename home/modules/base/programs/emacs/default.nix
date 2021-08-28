pkgs:

{
    enable = true;

    package = pkgs.emacsGcc;

    extraPackages = epkgs: with epkgs; [
        emacsql
        emacsql-sqlite
        vterm
    ];
}
