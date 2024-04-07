{
    enable = true;
    settings = {
        user = {
            name = "Sukant Hajra";
            email = "dev.sukant@hajra.xyz";
        };
        ui.default-command = "l";
        revset-aliases = {
            "interesting()" = "ancestors(mine() ~ ::trunk(), 2) | trunk() | @";
        };
        aliases = {
            l = ["log" "-r" "interesting()"];
        };
    };
}
