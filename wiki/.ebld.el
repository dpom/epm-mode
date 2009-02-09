(add-to-list 'load-path "/local/share/emacs/site-lisp/ebld/")
(require 'ebld)

(add-to-list 'load-path "/local/share/emacs/site-lisp/")
(add-to-list 'load-path "/local/share/emacs/site-lisp/muse")
(load-file "/local/share/emacs/site-lisp/cedet/common/cedet.el")
(setq debug-on-error t)

(setq ebld-project-name "epm-wiki")

(ebld-init)

(setq ebld-clean-regexp "~$\\|\.elc$\\|\.orig$\\|semantic\.cache")
(setq ebld-elisp-src-dir "src/main/lisp/")
(add-to-list 'load-path (expand-file-name ebld-elisp-src-dir))
(setq ebld-default-task 'update)

(defvar usb-dir "")
(defvar pc-dir "")
(setq usb-dir "/media/USB/dan/archive/pers/project/dpom/epm")
(setq pc-dir "/misc/partner/users/dan/pers/project/dpom/epm")

(task 'putusb () "push changes to usb" '(lambda (&optional x)
                      (concat "git push -v " usb-dir)))

(task 'getusb () "push changes from usb" '(lambda (&optional x)
                      (concat "git pull -v " usb-dir)))

(task 'putpc () "push changes to pc" '(lambda (&optional x)
                      (concat "git push -v " pc-dir)))

(task 'getpc () "push changes from pc" '(lambda (&optional x)
                      (concat "git pull -v " pc-dir)))

(task 'update () "update to last version" '(lambda (&optional x) "git svn rebase"))

(task 'commit () "commit" '(lambda (&optional x) "git svn dcommit"))

