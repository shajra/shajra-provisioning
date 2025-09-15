lib:

let
  scripts = [
    raycast-shajra/tmux-cake.applescript
    raycast-shajra/tmux-shajra.applescript
    raycast-shajra/tmux-lab-srv38.applescript
  ];

  toConfig = path: {
    "raycast-shajra/${baseNameOf path}" = {
      source = path;
      executable = true;
    };
  };

in
lib.attrsets.mergeAttrsList (map toConfig scripts)
