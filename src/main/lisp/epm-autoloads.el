;;; epm-autoloads.el --- autoloads for epm
;;
;;; Code:

;;;### (autoloads (epm-mode epm-help epm-print-all-tasks-between
;;;;;;  epm-add-nonworking-period epm-move-task-to-planned epm-move-task-to-done
;;;;;;  epm-move-task-to-working epm-copy-task-to-day epm-display-task-total-done
;;;;;;  epm-add-day epm-add-resource epm) "epm" "epm.el" (18896 56225))
;;; Generated autoloads from epm.el

(autoload (quote epm) "epm" "\
Main entry point.

\(fn)" t nil)

(autoload (quote epm-add-resource) "epm" "\
Add a resource (from resource list) to a task.

\(fn)" t nil)

(autoload (quote epm-add-day) "epm" "\
Add a date to a task. Default current date.

\(fn)" t nil)

(autoload (quote epm-display-task-total-done) "epm" "\
Diplay the total todo time for the current task.

\(fn)" t nil)

(autoload (quote epm-copy-task-to-day) "epm" "\
Copy the current task to a specific day.

\(fn)" t nil)

(autoload (quote epm-move-task-to-working) "epm" "\
Not documented

\(fn)" t nil)

(autoload (quote epm-move-task-to-done) "epm" "\
Not documented

\(fn)" t nil)

(autoload (quote epm-move-task-to-planned) "epm" "\
Not documented

\(fn)" t nil)

(autoload (quote epm-add-nonworking-period) "epm" "\
Insert an non-working period with the following format:
?resource startdate enddate

\(fn)" t nil)

(autoload (quote epm-print-all-tasks-between) "epm" "\
Insert in current buffer all tasks contained in day files.

\(fn)" t nil)

(defvar epm-prefix-map (let ((map (make-sparse-keymap))) (define-key map "a" (quote epm-move-task-to-working)) (define-key map "c" (quote epm-copy-task-to-day)) (define-key map "d" (quote epm-add-day)) (define-key map "h" (quote epm-help)) (define-key map "l" (quote epm-print-all-tasks-between)) (define-key map "m" (quote epm)) (define-key map "n" (quote epm-add-nonworking-period)) (define-key map "p" (quote epm-move-task-to-planned)) (define-key map "r" (quote epm-add-resource)) (define-key map "t" (quote epm-display-task-total-done)) (define-key map "x" (quote epm-move-task-to-done)) map))

(fset (quote epm-prefix-map) epm-prefix-map)

(autoload (quote epm-help) "epm" "\
Display a help buffer

\(fn)" t nil)

(autoload (quote epm-mode) "epm" "\
A project manager mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (epm-update-task epm-display-burndown-chart epm-display-focus
;;;;;;  epm-display-todo epm-display-user-done epm-display-done epm-display-remaining-estimate
;;;;;;  epm-display-estimate epm-display-item) "epm-scrum" "epm-scrum.el"
;;;;;;  (18896 54084))
;;; Generated autoloads from epm-scrum.el

(autoload (quote epm-display-item) "epm-scrum" "\
Display item for current buffer

\(fn MES GETITEM)" nil nil)

(autoload (quote epm-display-estimate) "epm-scrum" "\
Display remaining estimate for current buffer

\(fn)" t nil)

(autoload (quote epm-display-remaining-estimate) "epm-scrum" "\
Display remaining estimate for current buffer

\(fn)" t nil)

(autoload (quote epm-display-done) "epm-scrum" "\
Display the total work for current buffer

\(fn)" t nil)

(autoload (quote epm-display-user-done) "epm-scrum" "\
Display the specific user total work for current buffer

\(fn)" t nil)

(autoload (quote epm-display-todo) "epm-scrum" "\
Display todo for current buffer

\(fn)" t nil)

(autoload (quote epm-display-focus) "epm-scrum" "\
Display focus for current buffer

\(fn)" t nil)

(autoload (quote epm-display-burndown-chart) "epm-scrum" "\
Display the scrum burndown chart for the current buffer project.

\(fn)" t nil)

(autoload (quote epm-update-task) "epm-scrum" "\
Redisplay the actual task updating estimate and done fields.

\(fn)" t nil)

;;;***

;;;### (autoloads (epm-to-tjp) "epm-taskjuggler" "epm-taskjuggler.el"
;;;;;;  (18896 54126))
;;; Generated autoloads from epm-taskjuggler.el

(autoload (quote epm-to-tjp) "epm-taskjuggler" "\
Convert the current buffer epm project in the TJP file.

\(fn TJP)" t nil)

;;;***

;;;### (autoloads (epm-rally-print-features2 epm-rally-print-features
;;;;;;  epm-rally-print-project) "epm-rally" "epm-rally.el" (18896
;;;;;;  54021))
;;; Generated autoloads from epm-rally.el

(autoload (quote epm-rally-print-project) "epm-rally" "\
Insert in the current buffer a printable form of the project using a rally csv FILE

\(fn FILE)" t nil)

(autoload (quote epm-rally-print-features) "epm-rally" "\
Insert in the current buffer a printable form of the features list using a rally csv FILE

\(fn FILE)" t nil)

(autoload (quote epm-rally-print-features2) "epm-rally" "\
Insert in the current buffer a printable form of the features list using a rally user stories csv FILE

\(fn FILE)" t nil)

;;;***

(provide 'epm-autoloads)
;;; epm-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

