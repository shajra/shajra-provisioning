config:

let gpg = config.programs.gpg.package;
in

{
    ".chrome-remote-desktop-session".text = ''
        systemctl --user start --wait gpg-agent.service
        systemctl --user start --wait clipmenu.service
        export XDG_RUNTIME_DIR="/run/user/$(id -u)"
        export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
        export SSH_AUTH_SOCK="$("${gpg}/bin/gpgconf" --list-dirs agent-ssh-socket)"
        . ~/.xsession
    '';
}
