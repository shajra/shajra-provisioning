rec {
    enable = true;
    associations.added = defaultApplications;
    defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";

        "application/x-extension-html"  = "microsoft-edge-dev.desktop";
        "application/x-extension-htm"   = "microsoft-edge-dev.desktop";
        "application/x-extension-shtml" = "microsoft-edge-dev.desktop";
        "application/x-extension-xht"   = "microsoft-edge-dev.desktop";
        "application/x-extension-xhtml" = "microsoft-edge-dev.desktop";
        "application/xhtml+xml"         = "microsoft-edge-dev.desktop";
        "text/html"                     = "microsoft-edge-dev.desktop";
        "x-scheme-handler/chrome"       = "microsoft-edge-dev.desktop";
        "x-scheme-handler/http"         = "microsoft-edge-dev.desktop";
        "x-scheme-handler/https"        = "microsoft-edge-dev.desktop";
        "x-scheme-handler/mailto"       = "microsoft-edge-dev.desktop";

        "text/plain"  = ["emacsclient.desktop" "nvim.desktop"];
        "text/x-lisp" = ["emacsclient.desktop" "nvim.desktop"];
    };
}
