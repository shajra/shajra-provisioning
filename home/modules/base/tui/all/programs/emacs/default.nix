{
    enable = true;
    extraPackages = epkgs: with epkgs; [
        emacsql
        emacsql-sqlite
        vterm
    ];
}
