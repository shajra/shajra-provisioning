;;; -*- lexical-binding: t; -*-
;;;###if (and (featurep! +dante) (featurep! :checkers syntax))


;;;###autoload
(defun +haskell/dante-hlint-on ()
  "Add HLint checker after Dante checker."
  (interactive)
  (flycheck-add-next-checker
   'haskell-dante
   `(,+haskell-dante-hlint-when-only . haskell-hlint))
  (message "HLint after Dante checking on"))

;;;###autoload
(defun +haskell/dante-hlint-off ()
  "Remove HLint checker from after Dante checker."
  (interactive)
  (flycheck-remove-next-checker 'haskell-dante 'haskell-hlint)
  (message "HLint after Dante checking off"))

;;;###autoload
(defun +haskell/dante-hlint-toggle ()
  "Toogle on/off HLint checker after Dante checker."
  (interactive)
  (if (-contains? (flycheck-get-next-checkers 'haskell-dante) 'haskell-hlint)
      (+haskell/dante-hlint-off)
    (+haskell/dante-hlint-on)))

;;;###autoload
(defun +haskell-dante-hlint-init-off ()
  "Disable HLint checking after Dante checker upon Doom startup.

The Doom module by default automatically enables HLint checking after Dante
checking.  This function can be called in a user's config.el file to override
this enablement.

Note, due to lazy loading, you can't just call ‘+haskell/dante-hlint-off’ until
the packages (flycheck primarily) have loaded.  Packages are loaded typically
from a haskell-mode hook when you open your first Haskell file.  This function
registers the HLint disablement after packages have loaded."
  (after! (dante flycheck)
    (+haskell/dante-hlint-off)))
