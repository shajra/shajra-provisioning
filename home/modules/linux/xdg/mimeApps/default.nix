rec {
    enable = true;
    associations.added = defaultApplications;
    defaultApplications = {
        "text/plain"  = ["emacsclient.desktop" "nvim.desktop"];
        "text/x-lisp" = ["emacsclient.desktop" "nvim.desktop"];
        "x-scheme-handler/http"         = "firefox.desktop";
        "x-scheme-handler/https"        = "firefox.desktop";
        "x-scheme-handler/chrome"       = "firefox.desktop";
        "text/html"                     = "firefox.desktop";
        "application/x-extension-htm"   = "firefox.desktop";
        "application/x-extension-html"  = "firefox.desktop";
        "application/x-extension-shtml" = "firefox.desktop";
        "application/xhtml+xml"         = "firefox.desktop";
        "application/x-extension-xhtml" = "firefox.desktop";
        "application/x-extension-xht"   = "firefox.desktop";
    };
}
