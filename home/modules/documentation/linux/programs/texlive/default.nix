{
    enable = true;
    extraPackages =  tpkgs: {
        inherit (tpkgs)
        capt-of
        catchfile
        environ
        framed
        fvextra
        harveyballs
        latexmk
        light-latex-make
        listings
        minted
        moreverb
        scheme-medium
        tcolorbox
        todonotes
        txfonts
        upquote
        wrapfig
        xstring
        ;
    };
}
