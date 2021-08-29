self: super:

let

    build = import ../../../.. {};
    sources = build.sources;
    src = sources."bluos-controller.dmg";
    pname = "bluos-controller";
    version = "3.14.0";
    name = "${pname}-${version}";

    meta.description = "BluOS Controller ${version} (non-free)";
    meta.platforms = super.lib.platforms.linux;

    undmg = super.stdenv.mkDerivation {
        inherit version src;
        pname = "${pname}-undmg";
        nativeBuildInputs = [ super.undmg ];
        unpackPhase = ''
            undmg "$src"
        '';
        sourceRoot = "BluOS Controller.app/Contents";
        installPhase = ''
            mkdir "$out"
            cp -r . "$out"
        '';
    };

    unasar = patches: super.stdenv.mkDerivation {
        pname = "${pname}-appimage";
        inherit version patches;
        src = undmg;
        nativeBuildInputs = with super.nodePackages; [
            asar
            js-beautify
        ];
        phases = ["unpackPhase" "patchPhase" "installPhase"];
        unpackPhase = ''
            asar extract "$src/Resources/app.asar" .
            js-beautify -r www/app.js
        '';
        installPhase = ''
            mkdir "$out"
            cp -r . "$out"
        '';
    };

    unasar-patched = unasar [ ./patch ];
    unasar-unpatched = unasar [];

    app = import ./wrapper.nix { inherit self pname meta unasar-patched; };

# DESIGN: can be useful for regenerating the patch file
#in { inherit app unasar-patched unasar-unpatched; }
in app
