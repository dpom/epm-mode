;;; epm-config.el --- epm configurations

;; Copyright (C) 2008 Dan Pomohaci (epm)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

(provide 'epm-config)


(add-to-list 'load-path "/local/share/emacs/site-lisp/epm")
(require 'epm)
(require 'epm-rally)

(setq epm-repository "~/work/plan/")
(setq epm-main-file "index.muse")
(setq epm-burndown-template-file "~/pers/project/dpom/epm/src/main/config/burndown.gp")
(setq epm-resource-list '("bill" "dan" "john" "justin" "mary" "steve"))
(define-key global-map (kbd "\C-c\C-p") 'epm-prefix-map)

;;; epm-config.el ends here
