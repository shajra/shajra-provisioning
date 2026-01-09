{
  build = {
    dev = false; # true, to skip Haskell.nix build for a faster dev cycle
  };

  infrastructure = {
    hackage.version = {
      apply-refact = "latest";
      fast-tags = "latest";
      ghc-events = "latest";
      ghcid = "latest";
      haskdogs = "latest";
      hasktags = "latest";
      hlint = "latest";
      hoogle = "latest";
      hp2pretty = "latest";
      stylish-haskell = "latest";
      threadscope = "latest";
    };
    haskell-nix = {
      checkMaterialization = false;
      platformSensitive = [
        "ghcid"
      ];
      # DESIGN: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix
      hackage.index = {
        state = "2026-01-09T00:00:00Z";
        sha256 = "982faa69fa92973d89dc96acfcbf5b06d326a6940f12369c3225d00846990f7b";
      };
      nixpkgs-pin = "nixpkgs-unstable";
    };
    nixpkgs = {
      config.allowUnfree = true;
      masterPkgsOverUnstable =
        # DESIGN: inherit temporarily when a fix is still only on master
        _masterPkgs: # with masterPkgs;
        {
          # inherit ...;
        };
    };
  };

  # Options for nixos-rebuild, darwin-rebuild, home-manager provisioning
  provision = {
    # Which version of Nixpkgs passed to NixOS-style modules as 'pkgs'.
    # Specific versions are specificed in external/sources.json.
    # Options:
    #     - "stable-darwin": nixpkgs-$RELEASE_VERSION-darwin
    #     - "stable-linux": nixos-$RELEASE_VERSION
    #     - "unstable": nixpkgs-unstable
    pkgs = {
      system = {
        darwin = "stable-darwin";
        linux = "stable-linux";
      };
      home = {
        darwin = "unstable";
        linux = "unstable";
      };
    };
    user = {
      bagel = rec {
        homeDirectory = "/Users/${username}";
        username = "shajra";
      };
      cake = rec {
        homeDirectory = "/home/${username}";
        username = "tnks";
      };
      lemon = rec {
        homeDirectory = "/Users/${username}";
        username = "tnks";
      };
      shajra = rec {
        homeDirectory = "/home/${username}";
        username = "shajra";
      };
    };
  };
}
