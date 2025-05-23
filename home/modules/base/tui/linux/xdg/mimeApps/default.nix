rec {
    enable = true;
    associations.added = defaultApplications;
    defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";

        "application/x-extension-html"  = "vivaldi.desktop";
        "application/x-extension-htm"   = "vivaldi.desktop";
        "application/x-extension-shtml" = "vivaldi.desktop";
        "application/x-extension-xht"   = "vivaldi.desktop";
        "application/x-extension-xhtml" = "vivaldi.desktop";
        "application/xhtml+xml"         = "vivaldi.desktop";
        "text/html"                     = "vivaldi.desktop";
        "x-scheme-handler/chrome"       = "vivaldi.desktop";
        "x-scheme-handler/http"         = "vivaldi.desktop";
        "x-scheme-handler/https"        = "vivaldi.desktop";
        "x-scheme-handler/mailto"       = "vivaldi.desktop";

        "text/plain"  = ["emacs.desktop" "nvim.desktop"];
        "text/x-lisp" = ["emacs.desktop" "nvim.desktop"];
    };
}
