final: prev:

let
  newPackages = with final.python3Packages; [
    pygobject3
    xcffib
  ];
  removeOld = builtins.filter (p: (p.pname or "") != "pygobject");
  addNew = ps: ps ++ newPackages;
in

prev.hplipWithPlugin.overridePythonAttrs (old: {
  buildInputs = old.buildInputs ++ [ final.xorg.libxcb ];
  pythonPath = addNew (removeOld old.pythonPath);
})
