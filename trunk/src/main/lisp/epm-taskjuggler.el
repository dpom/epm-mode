;;; epm-taskjuggler.el --- Convert epm to taskjuggler

;; Copyright (C) 2008-2009 Dan Pomohaci (dpom)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;;; Code

(provide 'epm-taskjuggler)

(require 'epm)

;; Global variables

(defvar epm-tjp-header-file ""
  "header file name")

(defvar epm-tjp-footer-file ""
  "footer file name")

(defvar epm-current-date-placeholder "<current-date>"
  "placeholder for current date")

;; Functions


;; TaskJuggler conversion

;;;###autoload
(defun epm-to-tjp (tjp)
  "Convert the current buffer epm project in the TJP file."
  (interactive "FTJP file: ")
  (let (featuresList tasksList)
    (setq featuresList (epm-get-all-features))
    (setq tasksList (append (epm-get-planned-tasks)
                            (epm-get-working-tasks)))
    (with-temp-file tjp
      (set-buffer-file-coding-system 'latin-1)
      (insert-file-contents epm-tjp-header-file)
      (goto-char (point-max))
      (mapc (lambda (x)
              (epm-write-feature x (epm-select-feature-tasks
                                    (epm-get-feature-id x) tasksList)))
            featuresList)
      (insert-file-contents epm-tjp-footer-file)
      (epm-set-curent-date)

      )
    (message "Write file %s" tjp)))


(defun epm-set-curent-date ()
  "Replace in current buffer epm-current-date-placeholder with current date."
  (goto-char (point-min))
  (while (re-search-forward epm-current-date-placeholder nil t)
       (replace-match (format-time-string "%Y-%m-%d"))))



(defun epm-write-feature (feature tasks)
  "Write a FEATURE and his TASKS as tjp tasks in current buffer."
  (insert (format "    task f%d \"%s\" {\n" (epm-get-feature-id feature)
                  (epm-get-feature-desc feature)))
  (mapc #'epm-write-task tasks)
  (insert "    }\n"))


(defun epm-write-task (task)
  "Write a plan TASK as a tjp task in the current buffer."
  (let ((status (epm-get-task-state task))
        (indent "      "))
    (insert (format "%stask  t%d_%02d \"%s\" {\n"
                    indent
                    (epm-get-task-featureid task)
                    (epm-get-task-id task)
                    (epm-get-task-desc task)))
    (if (eq status :working) (insert (format "%s  start %s\n" indent
                                             (format-time-string "%Y-%m-%d"))))
    (insert (format "%s  effort %.3fd\n" indent
                    (if (eq status :planned) (epm-get-task-estimate task)
                      (epm-get-task-work task))))
    (let ((res (epm-get-task-resource task)))
      (insert (format "%s  allocate %s\n" indent
                      (if (stringp res) res "${?allocDev}"))))
    (insert (concat indent "}\n"))))
  

(define-key epm-prefix-map "j" 'epm-to-tjp)


;;; epm-taskjuggler.el ends here