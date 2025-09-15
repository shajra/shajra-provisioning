lib: mod:

let

  directions = {
    "h" = "left";
    "Left" = "left";
    "j" = "down";
    "Down" = "left";
    "k" = "up";
    "Up" = "up";
    "l" = "right";
    "Right" = "right";
  };

  # DESIGN: Sometimes it's easier to keep the mod-key depressed.
  bindingsForDirection = key: dir: {
    "${key}" = "resize grow   ${dir} 5 px or 1 ppt";
    "${mod}+${key}" = "resize grow   ${dir} 5 px or 1 ppt";
    "Shift+${key}" = "resize shrink ${dir} 5 px or 1 ppt";
    "${mod}+Shift+${key}" = "resize shrink ${dir} 5 px or 1 ppt";
  };

  directionBindings =
    let
      unmerged = lib.mapAttrsToList bindingsForDirection directions;
    in
    lib.foldl' (a: b: a // b) { } unmerged;

in
{

  resize = directionBindings // {
    # DESIGN: This is a hack that balances a split when it's a split, but
    # otherwise probably will do something surprising.
    "equal" = "floating toggle, floating toggle";
    "${mod}+equal" = "floating toggle, floating toggle";

    "${mod}+Escape" = "mode \"default\"";
    "${mod}+Shift+Escape" = "mode \"default\"";
    "${mod}+s" = "mode \"default\"";
    "Escape" = "mode \"default\"";
    "Return" = "mode \"default\"";
  };

  passthrough = {
    "${mod}+Escape" = "mode \"default\"";
    "${mod}+Shift+Escape" = "mode \"default\"";
  };
}
