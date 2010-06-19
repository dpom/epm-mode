(defvar dotfiles-dir "" "emacs local configuration dir")
(defvar contrib-dir   ""  "local elisp directory")

(setq dotfiles-dir "~/.emacs.d/")
(setq contrib-dir   (concat dotfiles-dir "contrib/"))

(add-to-list 'load-path (concat contrib-dir "ebld/"))
(require 'ebld)

(add-to-list 'load-path dotfiles-dir)
(require 'init-muse)
(require 'calendar)

(add-to-list 'load-path (concat contrib-dir "gnuplot-mode/"))
(require 'gnuplot)

;;; define local variables

;;; set local variables

(setq ebld-project-name "epm")
(setq ebld-clean-default-regexp "~$\\|semantic.cache\\|\.elc$")
(setq ebld-elisp-deploy-root contrib-dir)
(setq ebld-default-task 'install)

;; (load-file (expand-file-name "cedet/common/cedet.el" local-elisp-dir))
;; (setq debug-on-error t)
(add-to-list 'load-path ebld-elisp-src-dir)

;;; auxilary functions

;;; tasks

(load ebld-init-file)


(task 'update () "update to last version" '(lambda (&optional x) "git svn rebase"))

(task 'commit () "commit" '(lambda (&optional x) "git svn dcommit"))

(task 'install '(clean genautoload elispbuild) "install package")




