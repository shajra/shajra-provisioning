;;; -*- lexical-binding: t; -*-
;;;###if (modulep! +dante)


;;; variables

(defcustom +haskell-dante-hlint-when-only 'info
  "Check with hlint only when Dante reporting is not greater than specified.

Note that Flycheck's checkers are chained globally (as symbol properties), so
this variable doesn't work well when set buffer-local.  Set this variable
globally, and it will affect how ‘+haskell/dante-hlint-on’ chains the HLint
checker.

Also see ‘+haskell/dante-hlint-off’, and ‘+haskell/dante-hlint-toggle’."
  :type '(radio
          (const :tag "show HLint when no warnings or errors" info)
          (const :tag "show HLint when no Dante errors" warning)
          (const :tag "show Hlint before Dante any checking" error)
          (const :tag "show Hlint when no Dante reporting (including info)" t))
  :group 'haskell)

(defcustom +haskell-dante-xref-enable t
  "Whether to enable ‘xref’ support for ‘dante-mode’.

Dante's ‘xref’ backend only finds references local to the project. So using a
tool like haskdogs, codex, or haskell-tags-nix, you might be able to set up a
normal etags backend find references in the source of non-local dependencies."
  :type '(radio
          (const :tag "enable Dante Xref backend" t)
          (const :tag "disable Dante Xref backend" nil))
  :group 'haskell)

;;; package configuration

(use-package! dante

  :hook ((haskell-mode-local-vars . dante-mode)
         (haskell-literate-mode   . dante-mode))

  :init

  (setq dante-load-flags '(;; defaults:
                           "+c"
                           "-fdiagnostics-color=never"
                           "-ferror-spans"
                           "-fno-diagnostics-show-caret"
                           "-Wwarn=missing-home-modules"
                           ;; necessary to make attrap-attrap useful:
                           "-Wall"
                           ;; necessary to make company completion useful:
                           "-fdefer-typed-holes"
                           "-fdefer-type-errors"))

  :config

  (when (modulep! :checkers syntax)
    (+haskell/dante-hlint-on))

  (set-company-backend! 'dante-mode #'dante-company)

  ;; DESIGN: Prevent Dante's buffer saving from leaving buffer in an unmodified
  ;; state. Whether a buffer's state is set as modified or not determines
  ;; whether some save hooks are run (for example autoformatting hooks).  Dante
  ;; works by saving the buffer to a temporary file, but without triggering save
  ;; hooks.  However Dante's saving as implemented upstream resets the buffer's
  ;; state as unmodified, which can prevent save hooks from triggering on a
  ;; subsequent save.
  (defadvice! +haskell--restore-modified-state-a (fn &rest args)
    :around #'dante-async-load-current-buffer
    (let ((modified-p (buffer-modified-p)))
      (apply fn args)
      (if modified-p (set-buffer-modified-p t))))

  (when (modulep 'evil)
    (add-hook 'dante-mode-hook #'evil-normalize-keymaps))

  (map! :map dante-mode-map
        :localleader
        "t" #'dante-type-at
        "i" #'dante-info
        "l" #'haskell-process-load-file
        "e" #'dante-eval-block
        "a" #'attrap-attrap)

  (+haskell--dante-methods-alist-extend)

  (remove-hook 'xref-backend-functions 'dante--xref-backend)

  (defadvice! +haskell--dante-mode-a (orig-fn &rest args)
    :around #'dante-mode
    (+haskell--cond-a 'dante orig-fn #'+haskell--dante-load-current-buffer args)
    (when +haskell-dante-xref-enable
      (add-hook 'xref-backend-functions 'dante--xref-backend nil t))))

(after! (dante envrc)
  (defadvice! +haskell--dante-envrc-a (&rest _)
    :before #'dante-start
    (envrc-reload)))

(after! (dante direnv)
  (defadvice! +haskell--dante-direnv-a (&rest _)
    :before #'dante-start
    (direnv-update-directory-environment)))
