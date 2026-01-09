{ ... }:

let
  hostname = "lemon";
in
{
  imports = [
    ../../modules/ubiquity
    ../../modules/darwin/personal
  ];
  networking.hostName = hostname;
}
