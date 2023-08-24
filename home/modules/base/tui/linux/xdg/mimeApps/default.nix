rec {
    enable = true;
    associations.added = defaultApplications;
    defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";

        "application/x-extension-html"  = "microsoft-edge-beta.desktop";
        "application/x-extension-htm"   = "microsoft-edge-beta.desktop";
        "application/x-extension-shtml" = "microsoft-edge-beta.desktop";
        "application/x-extension-xht"   = "microsoft-edge-beta.desktop";
        "application/x-extension-xhtml" = "microsoft-edge-beta.desktop";
        "application/xhtml+xml"         = "microsoft-edge-beta.desktop";
        "text/html"                     = "microsoft-edge-beta.desktop";
        "x-scheme-handler/chrome"       = "microsoft-edge-beta.desktop";
        "x-scheme-handler/http"         = "microsoft-edge-beta.desktop";
        "x-scheme-handler/https"        = "microsoft-edge-beta.desktop";
        "x-scheme-handler/mailto"       = "microsoft-edge-beta.desktop";

        "text/plain"  = ["emacsclient.desktop" "nvim.desktop"];
        "text/x-lisp" = ["emacsclient.desktop" "nvim.desktop"];
    };
}
