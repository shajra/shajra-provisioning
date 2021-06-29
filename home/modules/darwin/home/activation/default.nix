config: lib: nixpkgs:

let
    home = config.home.homeDirectory;
in

{
    copyApplications =
        let apps = nixpkgs.buildEnv {
                name = "home-manager-applications";
                paths = config.home.packages;
                pathsToLink = "/Applications";
            };
        in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            base_dir="${home}/Applications/Home Manager Apps"
            if [ -d "$base_dir" ]; then
              rm -rf "$base_dir"
            fi
            mkdir -p "$base_dir"
            for appFile in ${apps}/Applications/*; do
              target="$base_dir/$(basename "$appFile")"
              $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$base_dir"
              $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
            done
        '';
}
