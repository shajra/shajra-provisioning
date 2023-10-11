config:

let gpg = config.programs.gpg.package;
in

{
    ".chrome-remote-desktop-session".text = ''
        systemctl --user start --wait gpg-agent.service
        systemctl --user start --wait clipmenu.service
        export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"
        export SSH_AUTH_SOCK="$("${gpg}/bin/gpgconf" --list-dirs agent-ssh-socket)"
        . ~/.xsession
    '';
}
