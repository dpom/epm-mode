;;; epm-test.el --- tests for epm
;; Copyright (C) 2008 Dan Pomohaci (dpom)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

(require 'cl)

(defvar epm-test-project-home (expand-file-name "~/pers/project/epm/")
  "project directory")

(defvar epm-test-project-src (expand-file-name  "src/main/lisp/" epm-test-project-home)
  "project sources directory")

(add-to-list 'load-path epm-test-project-src)

(load-file (expand-file-name "src/main/lisp/epm.el" epm-test-project-home))

(require 'ert)

;; global variables

(defvar epm-test-work-dir (expand-file-name "target/test/" epm-test-project-home)
  "temporary testing directory")

(defvar epm-test-resources-dir (expand-file-name "src/test/resources/" epm-test-project-home)
  "testing resource directory")

(defvar epm-test-buffer nil
  "temporary test buffer")

;; auxiliary functions

(defun epm-test-fixture (body)
  (unwind-protect
      (progn
        (epm-test-setup)
        (cd epm-test-work-dir)
        (funcall body))
    (epm-test-tear-down)))


(defun epm-test-setup ()
  (make-directory epm-test-work-dir t)
  (epm-rcopy epm-test-resources-dir epm-test-work-dir ".*"))

(defun epm-test-tear-down ()
  (kill-buffer epm-test-buffer)
  (dired-delete-file epm-test-work-dir 'always))

(defsubst epm-test-existence-with-refdir (files refdir)
  "Check file from FILES list existence. The returning list will have t if file exists and nil else. "
  (mapcar #'(lambda (x)
              (file-exists-p (expand-file-name x refdir)))
          files))

(defsubst epm-test-existence (files)
  "Same as epm-test-existence-with-refdir but the files names are relative to epm-test-work-dir"
  (epm-test-existence-with-refdir files epm-test-work-dir))


(defun epm-rcopy (src dest regexp)
  "Recursive copy REGEXP files from SRC to DEST"
  (epm-walk src regexp '(lambda (x)
                             (let ((out (replace-regexp-in-string src dest x)))
                               (message "copy %s to %s" x out)
                               (make-directory (file-name-directory out) t)
                               (copy-file x out t)))))

(defun epm-walk (dir regexp function)
  "Walk DIR recursively and execute FUNCTION for REGEXP match files"
  (cond
   ((file-regular-p dir) (and (string-match regexp dir) (funcall function dir)))
   ((file-directory-p dir) (mapc #'(lambda (x) (epm-walk x regexp function))
                                 (directory-files (expand-file-name dir) t "[^.]$")))))

;; tests

;; test template
;; (deftest epm-test-template ()
;;   "documentation"
;;   (epm-test-fixture
;;    (lambda ()
;;      (let ((acc 0))
;;        body
;;        (should (equal acc check))))))

(deftest epm-test-get-user-tasks ()
  "Select from TASKS only USER tasks"
  (epm-test-fixture
   (lambda ()
     (let ((user "dan")
           (file "TestGetUserTasks.muse")
           (check 9))
       (setq epm-test-buffer (find-file (expand-file-name file epm-test-work-dir)))
       (should (= (length (epm-get-user-tasks user (epm-get-all-tasks))) check))))))


(deftest epm-test-get-iso-next-day ()
  "Return the next day. The DAY and the next day are in ISO format."
  (should (string= (epm-get-iso-next-day "2009-02-16") "2009-02-17")) ; normal day
  (should (string= (epm-get-iso-next-day "2009-02-28") "2009-03-01")) ; last day of the 28 days month
  (should (string= (epm-get-iso-next-day "2009-04-30") "2009-05-01")) ; last day of the 30 days month 
  (should (string= (epm-get-iso-next-day "2009-03-31") "2009-04-01")) ; last day of the 31 days month
  (should (string= (epm-get-iso-next-day "2009-12-31") "2010-01-01")) ; last day of the year
  )


(deftest epm-test-generate-iso-day-list ()
  "Generate a list with iso dates between BEGIN and END"
  (should (equal (epm-generate-iso-day-list "2009-01-01" "2009-01-01") '("2009-01-01"))) ; single day
  (should (equal (epm-generate-iso-day-list "2009-01-01" "2009-01-05") '("2009-01-01" "2009-01-02" "2009-01-03" "2009-01-04" "2009-01-05"))))

(deftest epm-test-get-all-tasks-between ()
    "Get all tasks from days between BEGIN and END"
  (epm-test-fixture
   (lambda ()
     (setq epm-repository epm-test-work-dir)
     (should (equal (epm-get-all-tasks-between "2009-01-01" "2009-01-03")
                    '((3 "1" "task 3" 3 0 3 "[[#res-dan][dan]] [[day/2009-01-01][2009-01-01]] [[day/2009-01-02][2009-01-02]] [[day/2009-01-03][2009-01-03]]")
                      (2 "1" "task 4" 2.0 0 2 "[[#res-doe][doe]] [[day/2009-01-02][2009-01-02]] [[day/2009-01-03][2009-01-03]]")
                      (1 "4" "task 2" 1.0 0 1 "[[#res-doe][doe]] [[day/2009-01-01][2009-01-01]] [[day/2009-01-02][2009-01-02]]")
                      (1 "3" "task 1" 1.0 0.5 0.5 "[[#res-dan][dan]] [[day/2009-01-01][2009-01-01]]"))
                    )))))

;; scrum tests

(load-file (expand-file-name "src/main/lisp/epm-scrum.el" epm-test-project-home))


(deftest epm-test-compute-user-done ()
  "Compute total done for a specific USER"
  (epm-test-fixture
   (lambda ()
     (let ((user "dan")
           (file "TestGetUserTasks.muse")
           (check 4.5))
       (setq epm-test-buffer (find-file (expand-file-name file epm-test-work-dir)))
       (should (= (epm-compute-user-done user) check))))))


;; rally tests

(load-file (expand-file-name "src/main/lisp/epm-rally.el" epm-test-project-home))
