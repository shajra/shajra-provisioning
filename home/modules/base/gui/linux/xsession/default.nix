config: pkgs: lib:

{
    enable = true;
    windowManager.i3 = import windowManager/i3 config pkgs lib;
}
