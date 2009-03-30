;;; epm-rally.el --- imports features and tasks from rally
;; Copyright (C) 2008-2009 Dan Pomohaci (dpom)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;;; Code

(provide 'epm-rally)

(require 'epm-scrum)

;; Global variables

(defvar epm-rally-userstory-regexp "[S\\|DE]\\([0-9]+\\): \\(.+\\)"
  "User story regular expression")

;; Functions

(defun epm-buffer-to-list ()
  "Return a list wich elements are current buffer lines starting from point"
  (let (result start)
    (while (not (eobp))
      (setq start (line-beginning-position))
      (end-of-line)
      (setq result (append result (list
				   (buffer-substring-no-properties
				    start (point)))))
      (forward-line 1))
    result))

(defsubst epm-string-trim (string seq)
  "Lose leading and trailing whitespace.  Also remove all properties
from string."
  (if (string-match (concat "\\`[" seq "]+") string)
      (setq string (substring string (match-end 0))))
  (if (string-match (concat "[" seq "]+\\'") string)
      (setq string (substring string 0 (match-beginning 0))))
  (set-text-properties 0 (length string) nil string)
  string)


(defsubst epm-rally-task-get-userstory (task) (epm-string-trim (nth 3 task) "\""))

(defsubst epm-rally-task-get-estimate (task)
  (string-to-number (epm-string-trim (nth 7 task) "\"")))

(defsubst epm-rally-task-get-todo (task)
  (string-to-number (epm-string-trim (nth 8 task) "\"")))

(defsubst epm-rally-task-get-done (task)
  (string-to-number (epm-string-trim (nth 9 task) "\"")))

(defsubst epm-rally-task-get-taskid (task)
  (substring (epm-string-trim (nth 1 task) "\"") 2))

(defsubst epm-rally-task-get-desc (task)
  (epm-string-trim (nth 2 task) "\""))

(defun epm-rally-task-get-userstory-id (task)
   (let ((userstory (epm-rally-task-get-userstory task)))
    (string-match epm-rally-userstory-regexp userstory)
    (match-string-no-properties 1 userstory)))

(defun epm-rally-task-get-userstory-name (task)
  (let ((userstory (epm-rally-task-get-userstory task)))
    (string-match epm-rally-userstory-regexp userstory)
     (match-string-no-properties 2 userstory)))

(defsubst epm-rally-userstory-get-id (userstory)
  (substring (epm-string-trim (nth 1 userstory) "\"") 2))

(defsubst epm-rally-userstory-get-name (userstory)
  (epm-string-trim (nth 2 userstory) "\""))

(defun epm-alist-add-value (alist key value)
  (if (not (assoc key alist))
      (cons (cons key value) alist)
    (let ((acc ()))
      (mapc #'(lambda (x)
                (if (equal (car x) key)
                    (setq acc (cons (cons key (+ (cdr x) value)) acc))
                  (setq acc (cons x acc))))
            alist)
      acc)))



(defun epm-rally-get-us-len ()
  "Import a rally task list and return an alist with user stories and their planned estimations"
    (let ((uslist ()))
    (mapc #'(lambda (x)
              (setq uslist
                    (epm-alist-add-value uslist (epm-rally-task-get-userstory x)
                                         (epm-rally-task-get-estimate x))))
          (epm-rally-import))
    uslist))

(defun epm-csv-to-list ()
  "Convert a csv buffer in a list. Each element of the list is a
  list containing csv fields."
  (mapcar #'(lambda (x)
              (split-string x "\"\\{1\\},\"\\{1\\}"))
          (epm-buffer-to-list)))

(defun epm-rally-import ()
  (goto-char (point-min))
  (forward-line 1)
  (epm-csv-to-list)
)


(defun epm-rally-import-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (epm-rally-import)))


(defun epm-rally-to-epm-task (task)
  "Convert rally TASK into an epm task"
  (list
   (string-to-number (epm-rally-task-get-userstory-id task))
   (string-to-number (epm-rally-task-get-taskid task))
   (epm-rally-task-get-desc task)
   (epm-rally-task-get-estimate task)
   (epm-rally-task-get-todo task)
   (epm-rally-task-get-done task)
   ""))
  

(defun epm-rally-get-feature-list (file)
  "Extract feature list from rally tasks csv FILE"
   (sort 
    (remove-duplicates 
    (mapcar #'(lambda (x)
                (list (string-to-number (epm-rally-task-get-userstory-id x))
                      (epm-rally-task-get-userstory-name x)))
            (epm-rally-import-file file))
     :test 'epm-feature=)
    'epm-feature<))

(defun epm-rally-get-feature-list2 (file)
  "Extract feature list from rally user stories csv FILE"
  (mapcar #'(lambda (x)
              (list (string-to-number (epm-rally-userstory-get-id x))
                    (epm-rally-userstory-get-name x)))
          (epm-rally-import-file file)))

(defun epm-rally-get-task-list (file)
  "Extract task list from rally tasks csv FILE"
  (mapcar #'epm-rally-to-epm-task (epm-rally-import-file file)))

;; Commands

;;;###autoload
(defun epm-rally-print-project (file)
  "Insert in the current buffer a printable form of the project using a rally csv FILE"
  (interactive "fRally csv file: ")
  (epm-print-project (epm-rally-get-feature-list file) (epm-rally-get-task-list file)))
  
;;;###autoload
(defun epm-rally-print-features (file)
  "Insert in the current buffer a printable form of the features list using a rally csv FILE"
  (interactive "fRally tasks csv file: ")
  (mapc #'(lambda (x)
            (insert (epm-feature-print x)))
        (epm-rally-get-feature-list file)))

  
;;;###autoload
(defun epm-rally-print-features2 (file)
  "Insert in the current buffer a printable form of the features list using a rally user stories csv FILE"
  (interactive "fRally features csv file: ")
  (mapc #'(lambda (x)
            (insert (epm-feature-print x)))
        (epm-rally-get-feature-list2 file)))


;; keymap
(define-key epm-prefix-map "b" 'epm-rally-print-project)
(define-key epm-prefix-map "e" 'epm-rally-print-features)
(define-key epm-prefix-map "k" 'epm-rally-print-features2)


;;; epm-rally.el ends here