{
    enable = true;
    extraPackages =  tpkgs: {
        inherit (tpkgs)
        capt-of
        latexmk
        scheme-medium
        wrapfig
        ;
    };
}
