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
        kpfonts
        kpfonts-otf
        lastpage
        latexmk
        light-latex-make
        listings
        minted
        moreverb
        newlfm
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
