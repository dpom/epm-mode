;;; epm-config.el --- epm configurations

;; Copyright (C) 2008 Dan Pomohaci (epm)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

(provide 'epm-config)


(add-to-list 'load-path "/local/share/emacs/site-lisp/epm")
(require 'epm)
(require 'epm-rally)

(setq epm-repository "/home/users/dan/work/plan/")
(setq epm-main-file "index.muse")
(setq epm-burndown-template-file "/home/users/dan/work/plan/burndown.gp")
(setq epm-resource-list '("adrian" "claudia" "dan" "justin" "silviu" "sorin"))

;;; epm-config.el ends here
