;;; epm-scrum.el --- scrum specific 

;; Copyright (C) 2008-2009 Dan Pomohaci (dpom)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;;; Code

(require 'epm)
(require 'gnuplot)

;; Global variables

(defvar epm-burndown-template-file ""
  "burndown gnuplot template file")

(defvar epm-ework-regexp "^ework:[ ]*\\([0-9.]*\\)"
  "ework regexp")

(defvar epm-etime-regexp "^etime:[ ]*\\([0-9.]*\\)"
  "etime regexp")

(defvar epm-team-regexp "^team:[ ]*\\([0-9.]*\\)"
  "etime regexp")

(defvar epm-burndown-regexp "^burndown:[ ]+\\([0-9., ]*\\)"
  "burndown regexp")

  
(defvar epm-ework-placeholder "<ework>"
  "placeholder for ework")

(defvar epm-etime-placeholder "<etime>"
  "placeholder for etime")

;; Functions

(defun epm-compute-item (getitem taskslist)
  "Compute item obtained with GETITEM function from TASKSLIST."
  (apply '+ (mapcar (lambda (x) (funcall getitem x)) taskslist)))
  

(defsubst epm-compute-estimate () (epm-compute-item 'epm-get-task-estimate (epm-get-all-tasks)))
(defsubst epm-compute-remaining-estimate () (epm-compute-item 'epm-get-task-estimate
                                                           (append (epm-get-planned-tasks)
                                                                   (epm-get-working-tasks))))
(defsubst epm-compute-done () (epm-compute-item 'epm-get-task-done (epm-get-all-tasks)))
(defsubst epm-compute-todo () (epm-compute-item 'epm-get-task-todo (epm-get-all-tasks)))
(defsubst epm-compute-user-done (user) (epm-compute-item 'epm-get-task-done (epm-get-user-tasks user (epm-get-all-tasks))))
  
(defun epm-get-item (regexp)
  "Get the item with REGEXP from current buffer."
  (goto-char (point-min))
  (if (re-search-forward regexp)
      (match-string-no-properties 1)))

(defsubst epm-get-ework () (string-to-number (epm-get-item epm-ework-regexp)))
(defsubst epm-get-etime () (string-to-number (epm-get-item epm-etime-regexp)))
(defsubst epm-get-team () (string-to-number (epm-get-item epm-team-regexp)))
(defsubst epm-get-burndown () (epm-get-item epm-burndown-regexp))


(defun epm-replace-placeholder (ph av)
  "Replace PH placeholder with AV actual value."
  (goto-char (point-min))
  (while (re-search-forward ph nil t)
       (replace-match (number-to-string av))))

(defun epm-plot-burndown (ework etime burndown)
  "Plot the burndown chart."
  (with-temp-buffer
    (insert-file-contents epm-burndown-template-file)
    (epm-replace-placeholder epm-ework-placeholder ework)
    (epm-replace-placeholder epm-etime-placeholder etime)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "," "\n" burndown))
    (insert "\ne")
    (gnuplot-mode)
    (gnuplot-send-buffer-to-gnuplot)))


;; Commands

(defun epm-display-item (mes getitem)
  "Display item for current buffer"
  (save-excursion
    (let ((val (format "%.2f" (funcall getitem))))
      (kill-new val)
      (message (format mes val)))))

(defun epm-display-estimate ()
  "Display remaining estimate for current buffer"
  (interactive)
  (epm-display-item "Estimate: %s" 'epm-compute-estimate))


(defun epm-display-remaining-estimate ()
  "Display remaining estimate for current buffer"
  (interactive)
  (epm-display-item "Remaining Estimate: %s" 'epm-compute-remaining-estimate))

(defun epm-display-done ()
  "Display the total work for current buffer"
  (interactive)
  (epm-display-item  "Total done: %s" 'epm-compute-done))

(defun epm-display-user-done ()
  "Display the specific user total work for current buffer"
  (interactive)
  (save-excursion
    (let* ((user (epm-read-resource))
           (val (format "%.2f" (epm-compute-user-done user))))
      (kill-new val)
      (message (format "%s done: %s" user val)))))


(defun epm-display-todo ()
  "Display todo for current buffer"
  (interactive)
  (epm-display-item  "Total todo: %s" 'epm-compute-todo))

(defun epm-display-focus ()
  "Display focus for current buffer"
  (interactive)
  (save-excursion
    (message (format "Focus: %f"
                   (/ (epm-compute-estimate) (* (epm-get-etime) (epm-get-team)))))))


(defun epm-display-burndown-chart ()
  "Display the scrum burndown chart for the current buffer project."
  (interactive)
  (save-excursion
    (epm-plot-burndown (epm-get-ework) (epm-get-etime) (epm-get-burndown))))

(defun epm-update-task ()
  "Redisplay the actual task updating estimate and done fields."
  (interactive)
  (save-excursion
    (let ((task (epm-get-current-task)))
      (let ((todo (read-number "Estimate todo: " (epm-get-task-todo task))))
        (epm-set-task-todo task todo)
        (epm-set-task-done task (epm-get-task-total-done task))
        (beginning-of-line)
        (delete-region (point) (+ (epm-goto-endtask) epm-endtask-len))
        (insert (epm-task-print task))))))


(define-key epm-prefix-map "f" 'epm-display-focus)
(define-key epm-prefix-map "g" 'epm-display-burndown-chart)
(define-key epm-prefix-map "i" 'epm-display-user-done)
(define-key epm-prefix-map "s" 'epm-display-remaining-estimate)
(define-key epm-prefix-map "u" 'epm-update-task)
(define-key epm-prefix-map "v" 'epm-display-todo)
(define-key epm-prefix-map "w" 'epm-display-done)
(define-key epm-prefix-map "z" 'epm-display-estimate)


(provide 'epm-scrum)

;;; epm-scrum.el ends here