{ ... }:

let
  hostname = "shajra-93PF7L4";
in
{
  imports = [
    ../../modules/ubiquity
    ../../modules/darwin/work
  ];
  ids.gids.nixbld = 352;
  ids.uids.nixbld = 352;
  networking.hostName = hostname;
}
