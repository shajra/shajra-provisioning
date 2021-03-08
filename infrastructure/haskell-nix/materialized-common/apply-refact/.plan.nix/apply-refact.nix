{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "apply-refact"; version = "0.9.3.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "matthewtpickering@gmail.com";
      author = "Matthew Pickering";
      homepage = "https://github.com/mpickering/apply-refact";
      url = "";
      synopsis = "Perform refactorings specified by the refact library.";
      description = "Perform refactorings specified by the refact library. It is primarily used with HLint's --refactor flag.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGELOG"
        "README.md"
        "tests/examples/*.hs"
        "tests/examples/*.hs.refact"
        "tests/examples/*.hs.expected"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."refact" or (errorHandler.buildDepError "refact"))
          (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."filemanip" or (errorHandler.buildDepError "filemanip"))
          (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
          ];
        buildable = true;
        modules = [
          "Refact/Utils"
          "Refact/Apply"
          "Refact/Fixity"
          "Refact/Internal"
          "Refact/Compat"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "refactor" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."refact" or (errorHandler.buildDepError "refact"))
            (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."filemanip" or (errorHandler.buildDepError "filemanip"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
            ];
          buildable = true;
          modules = [
            "Paths_apply_refact"
            "Refact/Apply"
            "Refact/Compat"
            "Refact/Fixity"
            "Refact/Internal"
            "Refact/Options"
            "Refact/Run"
            "Refact/Utils"
            ];
          hsSourceDirs = [ "src" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."refact" or (errorHandler.buildDepError "refact"))
            (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."filemanip" or (errorHandler.buildDepError "filemanip"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
            ];
          buildable = true;
          modules = [
            "Paths_apply_refact"
            "Refact/Apply"
            "Refact/Compat"
            "Refact/Fixity"
            "Refact/Internal"
            "Refact/Options"
            "Refact/Run"
            "Refact/Utils"
            ];
          hsSourceDirs = [ "tests" "src" ];
          mainPath = [ "Test.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }