config:

{
  enable = true;
  settings = {
    user = {
      name = "Sukant Hajra";
      email = config.programs.git.settings.user.email;
    };
    ui.default-command = "l";
    revset-aliases = {
      "interesting" = "interesting(2)";
      "interesting(last_n)" =
        "ancestors(author(substring:\"Sukant Hajra\") ~ ::trunk(), last_n) | trunk() | @";
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
