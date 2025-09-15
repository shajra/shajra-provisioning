{ ... }:

let
  hostname = "lemon";
in
{
  imports = [
    ../../modules/ubiquity
    ../../modules/darwin
  ];
  networking.hostName = hostname;
}
