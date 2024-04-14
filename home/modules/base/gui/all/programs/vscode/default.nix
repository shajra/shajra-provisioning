config: pkgs:

let fonts = config.theme.fonts;
in {
    enable = true;
    enableExtensionUpdateCheck = false;
    enableUpdateCheck = false;
    extensions = [
        pkgs.vscode-marketplace.tamasfe.even-better-toml
        pkgs.vscode-marketplace.znck.grammarly
        pkgs.vscode-marketplace.kahole.magit
        pkgs.vscode-marketplace.bbenoist.nix
        pkgs.vscode-marketplace.vscode-org-mode.org-mode
        pkgs.vscode-marketplace.ms-vscode.remote-explorer
        pkgs.vscode-marketplace-release.ms-vscode-remote.remote-ssh
        pkgs.vscode-marketplace-release.rust-lang.rust-analyzer
        pkgs.vscode-marketplace.timonwong.shellcheck
        pkgs.vscode-marketplace.trond-snekvik.simple-rst
        pkgs.vscode-marketplace.gruntfuggly.todo-tree
        pkgs.vscode-marketplace.vscodevim.vim
    ];

    # DESIGN: home.activation script will make this mutable
    userSettings = {
        "editor.fontFamily" = fonts.monospaced.code.name;
        "editor.fontLigatures" = true;
        "editor.fontSize" = 12;
        "editor.minimap.enabled" = false;
        "files.trimTrailingWhitespace" = true;
        "git.openRepositoryInParentFolders" = "always";
        "grammarly.files.include" = [
            "**/readme.md"
              "**/README.md"
              "**/*.txt"
              "**/*.org"
        ];
        "problems.sortOrder" = "position";
        "remote.SSH.remotePlatform".cake = "linux";
        "remote.SSH.remotePlatform".shajra = "linux";
        "remote.SSH.useLocalServer" = false;
        "terminal.integrated.fontFamily" = fonts.monospaced.code.name;
        "terminal.integrated.fontSize" = 12;
        "vim.easymotion" = true;
        "vim.enableNeovim" = true;
        "vim.normalModeKeyBindingsNonRecursive" = [
          {
              "before" = [ "<space>" ];
              "commands" = [ "vspacecode.space" ];
          }
          {
              "before" = ["," ];
              "commands" = [
                  "vspacecode.space"
                  {
                    "command" = "whichkey.triggerKey";
                    "args" = "m";
                  }
              ];
          }
        ];
        "vim.useSystemClipboard" = true;
        "vim.visualModeKeyBindingsNonRecursive" = [
            {
               "before" = [ "<space>" ];
                "commands" = [ "vspacecode.space" ];
            }
            {
                "before" = [ "," ];
                "commands" = [
                    "vspacecode.space"
                    {
                        "command" = "whichkey.triggerKey";
                        "args" = "m";
                    }
                ];
            }
            {
                "before" = [ ">" ];
                "commands" = [ "editor.action.indentLines" ];
            }
            {
                "before" = [ "<" ];
                "commands" = [ "editor.action.outdentLines" ];
            }
        ];
        "whichkey.delay" = 350;
        "window.menuBarVisibility" = "toggle";
        "workbench.colorTheme" = "Solarized Light";
    };
}
