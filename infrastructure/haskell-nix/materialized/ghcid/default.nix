{
  pkgs = hackage:
    {
      packages = {
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "extra".revision = (((hackage."extra")."1.7.8").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "lifted-async".revision = (((hackage."lifted-async")."0.10.1.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "cmdargs".revision = (((hackage."cmdargs")."0.10.20").revisions).default;
        "cmdargs".flags.testprog = false;
        "cmdargs".flags.quotation = true;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "enclosed-exceptions".revision = (((hackage."enclosed-exceptions")."1.0.3").revisions).default;
        "tasty".revision = (((hackage."tasty")."1.4").revisions).default;
        "tasty".flags.clock = true;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.16.1.0").revisions).default;
        "optparse-applicative".flags.process = true;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "constraints".revision = (((hackage."constraints")."0.12").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.19.1").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.template-haskell = true;
        "lifted-base".revision = (((hackage."lifted-base")."0.2.3.12").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.6").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.2.0").revisions).default;
        "terminal-size".revision = (((hackage."terminal-size")."0.3.2.1").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "shelly".revision = (((hackage."shelly")."1.9.0").revisions).default;
        "shelly".flags.build-examples = false;
        "shelly".flags.lifted = false;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.0").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "wcwidth".revision = (((hackage."wcwidth")."0.0.2").revisions).default;
        "wcwidth".flags.split-base = true;
        "wcwidth".flags.cli = false;
        "hinotify".revision = (((hackage."hinotify")."0.4.1").revisions).default;
        "type-equality".revision = (((hackage."type-equality")."1").revisions).default;
        "text".revision = (((hackage."text")."1.2.3.2").revisions).default;
        "base".revision = (((hackage."base")."4.14.1.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "tasty-hunit".revision = (((hackage."tasty-hunit")."0.10.0.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "unbounded-delays".revision = (((hackage."unbounded-delays")."0.1.1.1").revisions).default;
        "unix-compat".revision = (((hackage."unix-compat")."0.5.2").revisions).default;
        "unix-compat".flags.old-time = false;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "fsnotify".revision = (((hackage."fsnotify")."0.3.0.1").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.2").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.3").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        };
      compiler = {
        version = "8.10.2";
        nix-name = "ghc8102";
        packages = {
          "exceptions" = "0.10.4";
          "binary" = "0.8.8.0";
          "ghc-prim" = "0.6.1";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "directory" = "1.3.6.0";
          "template-haskell" = "2.16.0.0";
          "containers" = "0.6.2.1";
          "bytestring" = "0.10.10.0";
          "text" = "1.2.3.2";
          "base" = "4.14.1.0";
          "time" = "1.9.3";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.9.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.10.2";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          };
        };
      };
  extras = hackage:
    { packages = { ghcid = ./.plan.nix/ghcid.nix; }; };
  modules = [ ({ lib, ... }: { packages = { "ghcid" = { flags = {}; }; }; }) ];
  }