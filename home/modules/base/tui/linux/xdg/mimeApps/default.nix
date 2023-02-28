rec {
    enable = true;
    associations.added = defaultApplications;
    defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";

        "application/x-extension-html"  = "microsoft-edge.desktop";
        "application/x-extension-htm"   = "microsoft-edge.desktop";
        "application/x-extension-shtml" = "microsoft-edge.desktop";
        "application/x-extension-xht"   = "microsoft-edge.desktop";
        "application/x-extension-xhtml" = "microsoft-edge.desktop";
        "application/xhtml+xml"         = "microsoft-edge.desktop";
        "text/html"                     = "microsoft-edge.desktop";
        "x-scheme-handler/chrome"       = "microsoft-edge.desktop";
        "x-scheme-handler/http"         = "microsoft-edge.desktop";
        "x-scheme-handler/https"        = "microsoft-edge.desktop";
        "x-scheme-handler/mailto"       = "microsoft-edge.desktop";

        "text/plain"  = ["emacsclient.desktop" "nvim.desktop"];
        "text/x-lisp" = ["emacsclient.desktop" "nvim.desktop"];
    };
}
