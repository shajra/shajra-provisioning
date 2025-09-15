{
  enable = true;
  settings = {
    user = {
      name = "Sukant Hajra";
      email = "dev.sukant@hajra.xyz";
    };
    ui.default-command = "l";
    revset-aliases = {
      "interesting" = "interesting(2)";
      "interesting(last_n)" = "ancestors(author(\"Sukant Hajra\") ~ ::trunk(), last_n) | trunk() | @";
    };
    aliases = {
      l = [
        "log"
        "-r"
        "interesting"
      ];
    };
  };
}
