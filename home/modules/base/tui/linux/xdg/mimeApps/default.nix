rec {
    enable = true;
    associations.added = defaultApplications;
    defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";

        "application/x-extension-html"  = "firefox-beta.desktop";
        "application/x-extension-htm"   = "firefox-beta.desktop";
        "application/x-extension-shtml" = "firefox-beta.desktop";
        "application/x-extension-xht"   = "firefox-beta.desktop";
        "application/x-extension-xhtml" = "firefox-beta.desktop";
        "application/xhtml+xml"         = "firefox-beta.desktop";
        "text/html"                     = "firefox-beta.desktop";
        "x-scheme-handler/chrome"       = "firefox-beta.desktop";
        "x-scheme-handler/http"         = "firefox-beta.desktop";
        "x-scheme-handler/https"        = "firefox-beta.desktop";
        "x-scheme-handler/mailto"       = "firefox-beta.desktop";

        "text/plain"  = ["emacs.desktop" "nvim.desktop"];
        "text/x-lisp" = ["emacs.desktop" "nvim.desktop"];
    };
}
