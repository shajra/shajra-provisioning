;;; -*- lexical-binding: t; -*-


(require 'f)

;;;###autoload
(defun +f-dirs-inclusive (dir)
  (cons dir (f-directories dir)))

;;;###autoload
(defun +org-agenda-files-includes? (file)
  (-any
   (lambda (f)
     (or
      (equal (expand-file-name f) (expand-file-name file))
      (file-in-directory-p file f)))
   org-agenda-files))
