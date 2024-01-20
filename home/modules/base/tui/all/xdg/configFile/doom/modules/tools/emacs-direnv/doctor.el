;;; -*- lexical-binding: t; -*-


(when (modulep! :tools direnv)
  (error! "you can't use ':tools direnv' with ':tools emacs-direnv'"))
