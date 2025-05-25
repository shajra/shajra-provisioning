final: prev:

prev.global.overrideAttrs (oldAttrs: {
    postInstall = ''
      mkdir -p "$out/share/emacs/site-lisp"
      cp -v *.el "$out/share/emacs/site-lisp"

      wrapProgram $out/bin/gtags \
        --set GTAGSCONF "$out/share/gtags/gtags.conf" \
        --prefix PYTHONPATH : "$(toPythonPath ${prev.pythonPackages.pygments})"
      wrapProgram $out/bin/global \
        --set GTAGSCONF "$out/share/gtags/gtags.conf" \
        --prefix PYTHONPATH : "$(toPythonPath ${prev.pythonPackages.pygments})"
    '';
})
