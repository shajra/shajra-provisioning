rec {
  enable = true;
  associations.added = defaultApplications;
  defaultApplications = {
    "application/pdf" = "org.pwmt.zathura.desktop";

    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "text/html" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
    "x-scheme-handler/mailto" = "firefox.desktop";

    "text/plain" = [
      "emacs.desktop"
      "nvim.desktop"
    ];
    "text/x-lisp" = [
      "emacs.desktop"
      "nvim.desktop"
    ];
  };
}
