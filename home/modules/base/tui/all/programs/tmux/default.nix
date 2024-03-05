{
    enable = true;
    extraConfig = ''
        set -g  allow-passthrough on
        set -ga update-environment TERM
        set -ga update-environment TERM_PROGRAM
        set -ga update-environment PREVIEW_FILE_AS
    '';
    terminal = "tmux-256color";
}
