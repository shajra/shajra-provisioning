{ ... }:

let
  hostname = "shajra-93PF7L4";
in
{
  imports = [
    ../../modules/ubiquity
    ../../modules/darwin/work
  ];
  networking.hostName = hostname;
}
