;;; -*- no-byte-compile: t; -*-


;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
;; (package! some-package :recipe (:local-repo "path/to/repo"))

(package! f)

;; DESIGN: https://www.reddit.com/r/DoomEmacs/comments/q9njsv/folks_on_a_recent_commit_of_emacs_28_are_seeing/
;; Feel this was fixed with a GCC Emacs upgrade.
;;
;;(package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9")

;; DESIGN: waiting for https://github.com/hlissner/doom-emacs/pull/5665
(package! gitconfig-mode :disable t)
(package! gitignore-mode :disable t)
(package! git-modes :pin "433e1c57a63c88855fc41a942e29d7bc8c9c16c7")
