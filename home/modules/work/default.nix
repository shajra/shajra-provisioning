{ lib, ... }:

let workEmail = lib.mkForce "shajra@groq.com";
in {
    programs.git.userEmail = workEmail;
    programs.jujutsu.settings.user.email = workEmail;
}
