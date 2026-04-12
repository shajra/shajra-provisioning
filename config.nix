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
        state = "2026-04-20T00:00:00Z";
        sha256 = "d446d8e2bf73bf591fb972a94267177fd5c96d92a5cef13b306f4002a952cb4b";
      };
      nixpkgs-pin = "nixpkgs-unstable";
    };
    nixpkgs = {
      config.allowUnfree = true;
      masterPkgsOverUnstable =
        # DESIGN: inherit temporarily when a fix is still only on master
        masterPkgs: with masterPkgs; {
          inherit
            code-cursor # REVISIT: BROKEN: 2026-04-12
            ;
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
      shajra-93PF7L4 = rec {
        homeDirectory = "/Users/${username}";
        username = "sukant.hajra";
      };
      cake = rec {
        homeDirectory = "/home/${username}";
        username = "tnks";
      };
      lemon = rec {
        homeDirectory = "/Users/${username}";
        username = "tnks";
      };
      saas-perflab = rec {
        homeDirectory = "/home/${username}";
        username = "ec2-user";
      };
    };
  };
}
