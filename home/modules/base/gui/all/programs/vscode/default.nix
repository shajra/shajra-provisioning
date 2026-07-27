config: pkgs:

let
  inherit (config.theme) fonts;
in
{
  enable = true;
  profiles.default = {
    enableExtensionUpdateCheck = false;
    enableUpdateCheck = false;
    extensions = [
      pkgs.vscode-marketplace."4ops".terraform
      pkgs.vscode-marketplace.bbenoist.nix
      pkgs.vscode-marketplace.bierner.markdown-mermaid
      pkgs.vscode-marketplace.bodil.file-browser
      pkgs.vscode-marketplace.dbaeumer.vscode-eslint
      pkgs.vscode-marketplace.editorconfig.editorconfig
      pkgs.vscode-marketplace.effectful-tech.effect-vscode
      pkgs.vscode-marketplace.github.vscode-github-actions
      pkgs.vscode-marketplace.golang.go
      pkgs.vscode-marketplace.googlecloudtools.cloudcode
      pkgs.vscode-marketplace.googlecloudtools.datacloud
      pkgs.vscode-marketplace.google.geminicodeassist
      pkgs.vscode-marketplace.haskell.haskell
      pkgs.vscode-marketplace.jdinhlife.gruvbox
      pkgs.vscode-marketplace.jj-view.jj-view
      pkgs.vscode-marketplace.jnoortheen.nix-ide
      # REVISIT: 2026-08-01: BLOCKED: Not found any more
      #pkgs.vscode-marketplace.justusadam.language-haskell
      pkgs.vscode-marketplace.kahole.magit
      pkgs.vscode-marketplace.marp-team.marp-vscode
      pkgs.vscode-marketplace.mermaidchart.vscode-mermaid-chart
      pkgs.vscode-marketplace.mkhl.direnv
      pkgs.vscode-marketplace.ms-azuretools.vscode-containers
      pkgs.vscode-marketplace.ms-azuretools.vscode-docker
      pkgs.vscode-marketplace.ms-kubernetes-tools.vscode-kubernetes-tools
      pkgs.vscode-marketplace.ms-python.debugpy
      pkgs.vscode-marketplace.ms-python.python
      pkgs.vscode-marketplace.ms-python.vscode-pylance
      pkgs.vscode-marketplace.ms-vscode.cmake-tools
      pkgs.vscode-marketplace.ms-vscode.extension-test-runner
      pkgs.vscode-marketplace.ms-vscode.makefile-tools
      pkgs.vscode-marketplace.ms-vscode.remote-explorer
      pkgs.vscode-marketplace.ms-vscode-remote.remote-containers
      pkgs.vscode-marketplace.ms-vscode-remote.remote-ssh
      pkgs.vscode-marketplace.ms-vscode-remote.remote-ssh-edit
      pkgs.vscode-marketplace.ms-toolsai.jupyter
      #pkgs.vscode-marketplace.ocamllabs.ocaml-platform
      pkgs.vscode-marketplace.opentofu.vscode-opentofu
      pkgs.vscode-marketplace.redhat.java
      pkgs.vscode-marketplace.redhat.vscode-xml
      pkgs.vscode-marketplace.redhat.vscode-yaml
      pkgs.vscode-marketplace.rust-lang.rust-analyzer
      pkgs.vscode-marketplace.scala-lang.scala
      pkgs.vscode-marketplace.scalameta.metals
      pkgs.vscode-marketplace.tamasfe.even-better-toml
      pkgs.vscode-marketplace.timonwong.shellcheck
      pkgs.vscode-marketplace.trond-snekvik.simple-rst
      pkgs.vscode-marketplace.visualjj.visualjj
      pkgs.vscode-marketplace.vscjava.vscode-gradle
      pkgs.vscode-marketplace.vscjava.vscode-java-debug
      pkgs.vscode-marketplace.vscjava.vscode-java-dependency
      pkgs.vscode-marketplace.vscjava.vscode-java-pack
      pkgs.vscode-marketplace.vscjava.vscode-java-test
      pkgs.vscode-marketplace.vscjava.vscode-maven
      pkgs.vscode-marketplace.vscodevim.vim
      pkgs.vscode-marketplace.vspacecode.vspacecode
      pkgs.vscode-marketplace.vspacecode.whichkey
      pkgs.vscode-marketplace.zardoy.move-statement
    ];

    # DESIGN: home.activation script will make this mutable
    userSettings = {
      "editor.fontFamily" = fonts.monospaced.code.name;
      "editor.fontLigatures" = true;
      "editor.fontSize" = 12;
      "editor.minimap.enabled" = false;
      "files.exclude" = {
        "**/.direnv" = true;
      };
      "files.trimTrailingWhitespace" = true;
      "files.watcherExclude" = {
        "**/.bloop" = true;
        "**/.metals" = true;
      };
      "git.openRepositoryInParentFolders" = "always";
      "haskell.formattingProvider" = "none";
      "haskell.manageHLS" = "PATH";
      "jj-view.suppressGitColocationWarning" = true;
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
          "before" = [ "," ];
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
      "metals.enableBestEffort" = true;
      "metals.javaVersion" = "21";
    };
  };
}
