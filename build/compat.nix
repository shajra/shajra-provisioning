# A standard way to allow non-flake users to use this project with a standard
# default.nix file using edolstra's flake-compat project.
#
# Flake-compat has been reliably stable for some time, so you shouldn't have to
# edit this file.
#
# Note that we augment the upstream `defaultNix` attribute with a new
# `currentSystem` attribute that selects out packages for the detected current
# system.  So if we're on an x86-64 Linux box, we can use
#
#     currentSystem.packages.my-app
#
# instead of
#
#     packages.x86_64-linux.my-app
#
#  Otherwise, `defaultNix` includes includes all the flakes outputs.

let
    lock = builtins.fromJSON (builtins.readFile ../flake.lock);
    compatUrlBase = "https://github.com/edolstra/flake-compat/archive";
    url = "${compatUrlBase}/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
    flake-compat = import (fetchTarball { inherit url sha256; });
    compat = flake-compat { src = ../.; };
    defaultNix = compat.defaultNix;
    defNixOutNames = builtins.attrNames defaultNix;
    augmentDefNix = acc: name:
        let output = defaultNix.${name};
            found = output.${builtins.currentSystem} or null;
            outputName = if found != null then name else null;
        in { ${outputName} = found; } // acc;
    currentSystem = builtins.foldl' augmentDefNix {} defNixOutNames;
in compat // {
    defaultNix = compat.defaultNix // { inherit currentSystem; };
}
