{ lib }:

let

  digit.h2d =
    h:
    {
      "0" = 0;
      "1" = 1;
      "2" = 2;
      "3" = 3;
      "4" = 4;
      "5" = 5;
      "6" = 6;
      "7" = 7;
      "8" = 8;
      "9" = 9;
      a = 10;
      b = 11;
      c = 12;
      d = 13;
      e = 14;
      f = 15;
    }
    ."${h}" or (throw "not hexadecimal digit: ${h}");

  digit.d2h =
    d:
    if d == 0 then
      "0"
    else if d == 1 then
      "1"
    else if d == 2 then
      "2"
    else if d == 3 then
      "3"
    else if d == 4 then
      "4"
    else if d == 5 then
      "5"
    else if d == 6 then
      "6"
    else if d == 7 then
      "7"
    else if d == 8 then
      "8"
    else if d == 9 then
      "9"
    else if d == 10 then
      "a"
    else if d == 11 then
      "b"
    else if d == 12 then
      "c"
    else if d == 13 then
      "d"
    else if d == 14 then
      "e"
    else if d == 15 then
      "f"
    else
      throw "not decimal digit: ${d}";

  h2d = hex: lib.foldl' (acc: x: acc * 16 + digit.h2d x) 0 (lib.stringToCharacters hex);

  d2h =
    dec:
    let
      sixteens = dec / 16;
      ones = dec - (sixteens * 16);
      stop = sixteens == 0;
      step = digit.d2h ones;
      recurse = "${d2h sixteens}${step}";
    in
    if stop then step else recurse;

  newFromDecWithAlpha =
    r: g: b: a:
    let
      bounded = x: lib.min 255 (lib.max 0 x);
    in
    {
      red = bounded r;
      green = bounded g;
      blue = bounded b;
      alpha = bounded a;
    };

  newFromHex =
    r: g: b:
    newFromDecWithAlpha (h2d r) (h2d g) (h2d b) 255;
  newFromDec =
    r: g: b:
    newFromDecWithAlpha r g b 255;

  withAlphaHex = a: c: {
    inherit (c)
      type
      red
      green
      blue
      ;
    alpha = h2d a;
  };

  withAlphaDec = a: c: {
    inherit (c)
      type
      red
      green
      blue
      ;
    alpha = a;
  };

  mix =
    num: dem: c1: c2:
    let
      red = (c1.red * num + c2.red * (dem - num)) / dem;
      green = (c1.green * num + c2.green * (dem - num)) / dem;
      blue = (c1.blue * num + c2.blue * (dem - num)) / dem;
      alpha = (c1.alpha * num + c2.alpha * (dem - num)) / dem;
    in
    newFromDecWithAlpha red green blue alpha;

  average = mix 1 2;

  darkenByDec =
    amount: color:
    let
      red = color.red - amount;
      green = color.green - amount;
      blue = color.blue - amount;
    in
    newFromDecWithAlpha red green blue color.alpha;

  lightenByDec =
    amount: color:
    let
      red = color.red + amount;
      green = color.green + amount;
      blue = color.blue + amount;
    in
    newFromDecWithAlpha red green blue color.alpha;

  palettes.solarized = rec {

    monotone =
      l:
      let
        black = newFromDec 0 0 0;
        white = newFromDec 255 255 255;
        makeInputs = cLo: lLo: cHi: lHi: {
          inherit
            cLo
            lLo
            cHi
            lHi
            ;
        };
        inputs =
          if l <= 15 then
            makeInputs black 0 base03 15
          else if l <= 20 then
            makeInputs base03 15 base02 20
          else if l <= 45 then
            makeInputs base02 20 base01 45
          else if l <= 50 then
            makeInputs base01 45 base00 50
          else if l <= 60 then
            makeInputs base00 50 base0 60
          else if l <= 65 then
            makeInputs base0 60 base1 65
          else if l <= 92 then
            makeInputs base1 65 base2 92
          else if l <= 97 then
            makeInputs base2 92 base3 97
          else
            makeInputs base3 97 white 100;
        color =
          let
            x = inputs;
          in
          c: (l - x.lLo) * (x.cHi."${c}" - x.cLo."${c}") / (x.lHi - x.lLo) + x.cLo."${c}";
      in
      newFromDec (color "red") (color "green") (color "blue");

    foregroundFor = _x: base3;

    base03 = newFromHex "00" "2b" "36";
    base02 = newFromHex "07" "36" "42";
    base01 = newFromHex "58" "6e" "75"; # emphasized text
    base00 = newFromHex "65" "7b" "83"; # standard text
    base0 = newFromHex "83" "94" "96";
    base1 = newFromHex "93" "a1" "a1"; # shadowed comments
    base2 = newFromHex "ee" "e8" "d5"; # background highlights
    base3 = newFromHex "fd" "f6" "e3"; # background

    blue = newFromHex "26" "8b" "d2";
    cyan = newFromHex "2a" "a1" "98";
    green = newFromHex "85" "99" "00";
    magenta = newFromHex "d3" "36" "82";
    orange = newFromHex "cb" "4b" "16";
    red = newFromHex "dc" "32" "2f";
    violet = newFromHex "6c" "71" "c4";
    yellow = newFromHex "b5" "89" "00";

    base025 = average base02 base03; # 94.5
    base015 = average base01 base02; # 78.5
    base005 = average base00 base01; # 62.5
    base = average base0 base00; # 55
    base05 = average base1 base0; # 47.5
    base15 = average base2 base1; # 32.5
    base25 = average base3 base2; # 17.5

    light.background = base3;
    light.background_highlighted = base2;
    light.foreground_shadowed = base1;
    light.foreground = base00;
    light.foreground_emphasized = base01;

    dark.background = base03;
    dark.background_highlighted = base02;
    dark.foreground_shadowed = base01;
    dark.foreground = base0;
    dark.foreground_emphasized = base1;

  };

  format =
    fmtStr: color:
    let
      raw = with color; [
        red
        green
        blue
        alpha
      ];
      dec = builtins.map builtins.toString raw;
      hexPad = x: if x < 10 then "0" else "";
      hex = builtins.map (x: hexPad x + d2h x) raw;
    in
    lib.replaceStrings [
      "%r"
      "%g"
      "%b"
      "%a"
      "%R"
      "%G"
      "%B"
      "%A"
    ] (dec ++ hex) fmtStr;

  isColor =
    x:
    with builtins;
    isAttrs x && hasAttr "red" x && hasAttr "green" x && hasAttr "blue" x && hasAttr "alpha" x;

  transformColors =
    f: palette:
    if isColor palette then
      f palette
    else
      lib.listToAttrs (
        lib.concatMap (
          name:
          let
            v = palette."${name}";
          in
          if lib.isAttrs v then [ (lib.nameValuePair name (transformColors f v)) ] else [ ]
        ) (lib.attrNames palette)
      );

  formatColors = fmtStr: transformColors (format fmtStr);

  colorEqual =
    c1: c2:
    isColor c1
    && isColor c2
    && c1.red == c2.red
    && c1.blue == c2.blue
    && c1.green == c2.green
    && c1.alpha == c2.alpha;

  colorName =
    c: palette:
    let
      names = lib.attrNames palette;
      acc = null;
      find =
        acc: name:
        let
          value = palette."${name}";
        in
        if builtins.isNull acc then
          (
            if colorEqual value c then
              name
            else
              (if builtins.isAttrs value && !isColor value then colorName c value else null)
          )
        else
          acc;
    in
    builtins.foldl' find acc names;

in
{
  inherit
    newFromHex
    newFromDec
    withAlphaHex
    withAlphaDec
    mix
    average
    darkenByDec
    lightenByDec
    format
    transformColors
    formatColors
    colorEqual
    colorName
    palettes
    ;
}
