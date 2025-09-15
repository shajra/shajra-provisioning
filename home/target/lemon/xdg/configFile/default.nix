lib:

let
  scripts = [
    raycast-shajra/tmux-cake.applescript
  ];

  toConfig = path: {
    "raycast-shajra/${baseNameOf path}" = {
      source = path;
      executable = true;
    };
  };

in
lib.attrsets.mergeAttrsList (map toConfig scripts)
