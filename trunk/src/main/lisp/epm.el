;;; epm.el --- emacs project mamagement tool

;; Copyright (C) 2008-2009 Dan Pomohaci (dpom)

;; Author: Dan Pomohaci <dan.pomohaci@gmail.com>
;; Maintainer: Dan Pomohaci <dan.pomohaci@gmail.com>
;; Created: 27 Jan 2007
;; Version: 1.0
;; Keywords: project management muse scrum

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;;; Code

(require 'muse)   
(require 'muse-mode)
(require 'org)
(require 'cl)

;; Global variables

(defvar epm-repository ""
  "Project repository. It has a rigid structure (./day...).")

(defvar epm-main-file ""
  "Project main file")

(defvar epm-resource-list '()
  "resource names list")

(defvar epm-task-regexp "^!\\([0-9]*\\)\.\\([0-9]*\\) \"\\([^\"]*\\)\"[ ]*{\\([0-9.]*\\)}[ ]*(\\([0-9.]*\\))[ ]*\|\\([0-9.]*\\)\|[ ]*\\(.*\\)"
  "Regular expresion for a task.")

;(setq epm-task-regexp "^!\\([0-9]*\\)\.\\([0-9]*\\) \"\\([^\"]*\\)\"[ ]*{\\([0-9.]*\\)}[ ]*(\\([0-9.]*\\))[ ]*\|\\([0-9.]*\\)\|[ ]*\\(.*\\)")

(defvar epm-feature-regexp "^\*\* \\([0-9]+\\)\. \\(.*\\)"
  "Regular expresion for a feature.")

(defvar epm-resource-regexp ".*#res-\\([^] \n]*\\)"
  "Regular expresion for a resource.")

(defvar epm-day-regexp "day/\\([0-9-]+\\)"
  "Regular expresion for a day.")

(defvar epm-endtask-regexp "\n\n"
  "Regular expresion for finding end task")

(defvar epm-endtask-len 2
  "Endtask marker length")

(defvar epm-resources-regexp "^\* Resources")

(defvar epm-planned-regexp "^\* Planned")

(defvar epm-working-regexp "^\* Working")

(defvar epm-done-regexp "^\* Done")

(defvar epm-endsection-regexp "^\* \\|^--")

(defvar epm-endsection-len 2
  "Endsection marker length.")

(defvar epm-endfeature-regexp "^\*\\|^-")

(defvar epm-endfeature-len 1
  "Endfeature marker length")

(defvar epm-startfeature-format "** %d\. ")



;; Tasks functions

(defun epm-get-task ()
  "Create a task list based on epm-task-regexp.
\(id featureid desc estimation todo done res+date \)"
  (list (string-to-number (match-string-no-properties 1))
        (string-to-number (match-string-no-properties 2))
        (match-string-no-properties 3)
        (string-to-number (match-string-no-properties 4))
        (string-to-number (match-string-no-properties 5))
        (string-to-number (match-string-no-properties 6))
        (match-string-no-properties 7)))

  
(defsubst epm-get-task-featureid (task) (car task))
(defsubst epm-get-task-id (task) (nth 1 task))
(defsubst epm-get-task-desc (task) (nth 2 task))
(defsubst epm-get-task-estimate (task) (nth 3 task))
(defsubst epm-get-task-todo (task) (nth 4 task))
(defsubst epm-get-task-done (task) (nth 5 task))
(defsubst epm-get-task-resdate (task) (nth 6 task))

(defun epm-set-task-todo (task todo)
  (setf (nth 4 task) todo)
  task)

(defun epm-set-task-done (task done)
  (setf (nth 5 task) done)
  task)

(defun epm-task-print (task)
  "Return a printable form of a TASK."
  (format "!%d.%d \"%s\" {%.2f} (%.2f) |%.2f| %s\n\n"
          (epm-get-task-featureid task)
          (epm-get-task-id task)
          (epm-get-task-desc task)
          (epm-get-task-estimate task)
          (epm-get-task-todo task)
          (epm-get-task-done task)
          (epm-get-task-resdate task)))


(defun epm-get-task-resource (task)
  (let ((str (epm-get-task-resdate task)))
    (if (not (= (length str) 0))
        (progn
          (string-match epm-resource-regexp str)
          (match-string-no-properties 1 str)))))

(defun epm-get-task-startday (task)
  (let ((str (epm-get-task-resdate task)))
    (string-match epm-day-regexp str)
    (match-string-no-properties 1 str)))



(defun epm-get-next-day (str pos)
  (if (string-match epm-day-regexp str pos)
      (cons (match-string-no-properties 1 str)
            (epm-get-next-day str (match-end 0)))))

(defun epm-get-task-days (task)
  (epm-get-next-day (epm-get-task-resdate task) 0))
   

(defun epm-task< (task1 task2)
  (or (< (epm-get-task-featureid task1) (epm-get-task-featureid task2))
      (and (= (epm-get-task-featureid task1) (epm-get-task-featureid task2))
           (< (epm-get-task-id task1) (epm-get-task-id task2)))))


      

;; Feature functions

(defsubst epm-get-feature-id (feature) (car feature))
(defsubst epm-get-feature-desc (feature) (nth 1 feature))

(defun epm-feature< (feature1 feature2)
  (< (epm-get-feature-id feature1) (epm-get-feature-id feature2)))

(defun epm-feature= (feature1 feature2)
  (= (epm-get-feature-id feature1) (epm-get-feature-id feature2)))

  

(defun epm-get-features (min max)
  "Get the features found in the region starting in MIN point and ending in MAX."
  (let ((acc ()))
    (goto-char min)
    (while (re-search-forward epm-feature-regexp max t)
      (setq acc (cons  (epm-get-feature) acc)))
    acc))


(defun epm-get-feature ()
  (list (string-to-number (match-string-no-properties 1))
        (match-string-no-properties 2)))


    
(defun epm-select-feature-tasks (featureId tasksList)
  "Select from TASKLIST only the tasks with FEATUREID."
  (let ((acc ()))
    (mapc (lambda (x)
            (if (= featureId  (epm-get-task-featureid x))
                (setq acc (cons x acc))))
          tasksList)
    (sort acc 'epm-task<)))

(defun epm-feature-print (feature)
  "Return a printable form of a FEATURE."
  (format "** %d. %s\n\n"
          (epm-get-feature-id feature)
          (epm-get-feature-desc feature)))



;; Project file functions

(defun epm-get-section (section)
  "Return the beginning (after title) and the ending points of a
section. The section is identified based on epm-*-regexp."
  (goto-char (point-min))
  (values (re-search-forward section)
          (- (re-search-forward epm-endsection-regexp) epm-endsection-len)))

(defsubst epm-get-planned-section ()
  (epm-get-section epm-planned-regexp))

(defsubst epm-get-working-section ()
  (epm-get-section epm-working-regexp))
    
(defsubst epm-get-done-section ()
  (epm-get-section epm-done-regexp))


(defun epm-get-all-features ()
  (save-excursion
    (sort (multiple-value-bind (x y) (epm-get-planned-section) (epm-get-features  x y)) 'epm-feature< )))


(defun epm-get-tasks (min max)
  "Get all the tasks found in the region starting in MIN point
and ending in MAX."
  (let ((acc ()))
    (goto-char min)
    (while (re-search-forward epm-task-regexp max t)
      (setq acc (cons  (epm-get-task) acc)))
    acc))

(defun epm-get-planned-tasks ()
  "Get planned task from current buffer."
  (multiple-value-bind (x y) (epm-get-planned-section) (epm-get-tasks x y)))

(defun epm-get-working-tasks ()
  "Get working task from current buffer."
  (multiple-value-bind (x y) (epm-get-working-section) (epm-get-tasks x y)))

(defun epm-get-done-tasks ()
  "Get done task from current buffer."
  (multiple-value-bind (x y) (epm-get-done-section) (epm-get-tasks x y)))

(defun epm-get-all-tasks ()
  "Get all task from current buffer."
  (save-excursion
    (append
     (epm-get-planned-tasks)
     (epm-get-working-tasks)
     (epm-get-done-tasks))))


(defun epm-get-tasks-from-file (file)
  "Get all task from a day/resource FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (epm-get-tasks (point-min) (point-max))))


(defun epm-get-task-from-list (feature id tasks)
    (car (remove nil (mapcar (lambda (x)
                          (and (= (epm-get-task-featureid x) feature)
                               (= (epm-get-task-id x) id)
                               x))
                        tasks))))


(defun epm-get-task-total-done (task)
  "Get the total done time for the TASK."
  (save-excursion
    (apply '+ (mapcar (lambda (x)
                        (epm-get-task-done (epm-get-task-from-list
                                            (epm-get-task-featureid task)
                                            (epm-get-task-id task)
                                            (epm-get-tasks-from-file
                                             (concat epm-repository "day/" x ".muse")))))
                      (epm-get-task-days task)))))


(defun epm-goto-endtask ()
  "Go to task end"
  (if (re-search-forward epm-endtask-regexp nil t)
      (goto-char (forward-point (- 0 epm-endtask-len)))
    (error "Not in a task")))

(defun epm-read-day (&optional prompt)
  "Read a day."
  (org-read-date nil nil nil prompt))

(defun epm-read-resource ()
  "Read a resource name"
  (completing-read "Resource: " epm-resource-list nil t))

(defun epm-get-current-task ()
  "Get the task where is the cursor."
  (beginning-of-line)
  (if (re-search-forward epm-task-regexp)
      (epm-get-task)))

(defun epm-get-current-feature ()
  "Get the feature before the cursor."
  (save-excursion
    (if (re-search-backward epm-feature-regexp)
        (epm-get-feature))))

(defun epm-print-project (features tasks)
  "Insert in the current buffer a printable form of the project using FEATURES list and TASKS list"
  (mapc #'(lambda (x)
            (insert (epm-feature-print x))
            (mapc #'(lambda (y)
                      (insert (epm-task-print y)))
                  (epm-select-feature-tasks (epm-get-feature-id x) tasks)))
        features))

(defun epm-goto-feature-end (featureid min max)
  "Move the current point to the end of a feature section found between MIN and MAX points"
  (goto-char min)
  (if (re-search-forward (format epm-startfeature-format featureid) max  t)
    (if (re-search-forward epm-endfeature-regexp max t)
        (goto-char (forward-point(- 0 epm-endfeature-len)))
      (goto-char max))
    (goto-char max)))

(defun epm-move-task (section)
  "Move current task in a section. The section is recognized with epm-*-regexp."
  (save-excursion
    (let ((task (epm-get-current-task)))
      (beginning-of-line)
      (delete-region (point) (+ (epm-goto-endtask) epm-endtask-len))
      (multiple-value-bind (x y) (epm-get-section section)
        (epm-goto-feature-end (epm-get-task-featureid task) x y))
    (insert (epm-task-print task)))))
  

(defun epm-get-user-tasks (user tasks)
  "Select from TASKS only USER tasks"
  (remove nil (mapcar (lambda (x) (if (equal user (epm-get-task-resource x)) x))
                      tasks)))


(defun epm-get-iso-next-day (day)
  "Return the next day. The DAY and the next day are in ISO format."
  (let* ((lday (split-string day "-"))
         (year (string-to-number (car lday)))
         (month (string-to-number (nth 1 lday)))
         (day (string-to-number (nth 2 lday)))
         (last-day (calendar-last-day-of-month month year))
         (next-year
          (if (and (= 12 month) (= 31 day))
              (1+ year)
            year))
         (next-month
          (if (>= day last-day)
              (1+ (mod month 12))
            month))
         (next-day (if (< day last-day) (1+ day) 1)))
    (format "%d-%02d-%02d" next-year next-month next-day)))

(defun epm-generate-iso-day-list (begin end)
  "Generate a list with iso dates between BEGIN and END"
  (if (string= begin end)
      (list end)
    (cons begin (epm-generate-iso-day-list (epm-get-iso-next-day begin) end))))

;; Commands

(defun epm ()
  "Main entry point."
  (interactive)
  (find-file (concat epm-repository epm-main-file))) 

(defun epm-add-resource ()
  "Add a resource (from resource list) to a task."
  (interactive)
  (epm-goto-endtask)
  (let ((resource (epm-read-resource)))
    (insert (format " [[#res-%s][%s]]" resource resource))))

(defun epm-add-day ()
  "Add a date to a task. Default current date."
  (interactive)
  (epm-goto-endtask)
  (let ((day (epm-read-day)))
    (insert (format " [[day/%s][%s]]" day day))))



(defun epm-display-task-total-done ()
  "Diplay the total todo time for the current task."
  (interactive)
    (save-excursion
      (let ((val (format "%.2f" (epm-get-task-total-done (epm-get-current-task)))))
      (kill-new val)
      (message "Total done on this task: %s" val))))

(defun epm-copy-task-to-day ()
  "Copy the current task to a specific day."
  (interactive)
  (save-window-excursion
    (let ((task (epm-get-current-task))
          (day (epm-read-day)))
      (let ((done (read-number "Done: " (epm-get-task-done task))))
        (setq task (epm-set-task-done task done))
        (find-file (concat epm-repository "/day/" day ".muse"))
        (goto-char (point-max))
        (insert (epm-task-print task))
        (save-buffer)))))

(defun epm-move-task-to-working ()
  (interactive)
  (epm-move-task epm-working-regexp))

(defun epm-move-task-to-done ()
  (interactive)
  (epm-move-task epm-done-regexp))

(defun epm-move-task-to-planned ()
  (interactive)
  (epm-move-task epm-planned-regexp))

(defun epm-add-nonworking-period ()
  (interactive)
  "Insert an non-working period with the following format:
?resource startdate enddate"
  (insert (format "?%s %s %s\n" (epm-read-resource) (epm-read-day "Start day") (epm-read-day "End day"))))

;; Key map

(defvar epm-prefix-map nil)

(setq epm-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'epm-move-task-to-working)
    (define-key map "c" 'epm-copy-task-to-day)
    (define-key map "d" 'epm-add-day)
    (define-key map "h" 'epm-help)
    (define-key map "m" 'epm)
    (define-key map "n" 'epm-add-nonworking-period)
    (define-key map "p" 'epm-move-task-to-planned)
    (define-key map "r" 'epm-add-resource)
    (define-key map "t" 'epm-display-task-total-done)
    (define-key map "x" 'epm-move-task-to-done)
    map))
(fset 'epm-prefix-map epm-prefix-map)

(defun epm-help ()
  "Display a help buffer"
  (interactive)
  (with-output-to-temp-buffer "*Epm Help*"
    (princ (format "Epm prefix key C-cm\n\n"))
    (mapc (lambda (x)
            (if (integerp (car x))
                (princ (format "%s %S\n" (string (car x)) (cdr x)))))
            (sort (copy-tree (cdr epm-prefix-map)) 'map<))))

(defun map< (x y)
  (< (car x) (car y)))
  


(define-derived-mode epm-mode muse-mode "EPM"
  "A project manager mode."
  (set (make-local-variable 'normal-auto-fill-function) nil)
  (turn-off-auto-fill))


(provide 'epm)

;;; epm.el ends here