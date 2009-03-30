(defvar local-elisp-dir "~/.emacs.d/contrib/" "local elisp directory")
(add-to-list 'load-path (concat local-elisp-dir "ebld/"))
(require 'ebld)

;;; define local variables

;;; set local variables

(add-to-list 'load-path local-elisp-dir)
(add-to-list 'load-path "~/.emacs.d/elpa/muse-3.12")
(load-file (expand-file-name "cedet/common/cedet.el" local-elisp-dir))
(setq debug-on-error t)

(setq ebld-project-name "epm")
(setq ebld-clean-regexp "~$\\|semantic.cache\\|\.elc$")
(setq ebld-elisp-deploy-root local-elisp-dir)
(setq ebld-default-task 'install)
(add-to-list 'load-path ebld-elisp-src-dir)

;;; auxilary functions

;;; tasks

(load ebld-init-file)

(task 'update () "update to last version" '(lambda (&optional x) "git svn rebase"))

(task 'commit () "commit" '(lambda (&optional x) "git svn dcommit"))

(task 'install '(clean genautoload elispbuild) "install package")




