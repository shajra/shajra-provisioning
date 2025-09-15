{ ... }:

{
  imports = [
    ../../ubiquity
    ../all
  ];
  programs.texlive = import programs/texlive;
}
