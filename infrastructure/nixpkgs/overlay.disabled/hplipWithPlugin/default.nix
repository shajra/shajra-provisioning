self: super:

let 
    newPackages = with self.python3Packages; [ pygobject3 xcffib ];
    removeOld = builtins.filter (p: (p.pname or "") != "pygobject");
    addNew = ps: ps ++ newPackages;
in

super.hplipWithPlugin.overridePythonAttrs (old: {
    buildInputs = old.buildInputs ++ [self.xorg.libxcb];
    pythonPath = addNew (removeOld old.pythonPath);
})
