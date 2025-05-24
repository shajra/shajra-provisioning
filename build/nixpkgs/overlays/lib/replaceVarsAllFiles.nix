{ pkgs }:

# Takes a set of arguments similar to substituteAllFiles but uses replaceVars under the hood
# Args:
#   src: Path to source directory
#   files: List of file paths relative to src to process
#   ...: All other arguments are passed as substitution variables
args@{ src, files, ... }:

let
  # Remove src and files from the args to get just the substitutions
  substitutions = builtins.removeAttrs args [ "src" "files" ];

  # Process each file with replaceVars
  processedFiles = map
    (file: pkgs.replaceVars (src + "/${file}") substitutions)
    files;

in
  # Join all processed files into a single derivation
  pkgs.symlinkJoin {
    name = "replaced-files";
    paths = processedFiles;
  }