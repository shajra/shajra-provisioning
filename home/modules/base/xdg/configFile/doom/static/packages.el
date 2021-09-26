;;; -*- no-byte-compile: t; -*-


;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
;; (package! some-package :recipe (:local-repo "path/to/repo"))

(package! f)

;; DESIGN: https://www.reddit.com/r/DoomEmacs/comments/q9njsv/folks_on_a_recent_commit_of_emacs_28_are_seeing/
(package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9")
