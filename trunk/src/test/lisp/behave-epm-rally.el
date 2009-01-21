;;; behave-epm-rally.el --- behave for epm-rally
;; Copyright (C) 2007 Dan Pomohaci (dpom)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.


(add-to-list 'load-path "../../src/lisp")
(load-file "../../src/lisp/epm-rally.el")

(require 'behave)

(defvar epm-test-dir "~/pers/project/dpom/epm/test/lisp/"
  "epm test directory")


(context "epm-rally-get-feature-list"
         (tag epm-rally)
         (specify "should return a feature alist generated from rally tasks csv file"
                  (expect (length (epm-rally-get-feature-list (concat epm-test-dir "rally.csv"))) equal  17))
         (specify "should return a feature alist with user stories ids as keys"
                  (expect (car (car (epm-rally-get-feature-list (concat epm-test-dir "rally.csv")))) equal 1))
         (specify "should return a feature alist with user stories names as values"
                  (expect (nth 1 (car (epm-rally-get-feature-list (concat epm-test-dir "rally.csv")))) equal
                          "user story 1")))

(context "epm-rally-get-feature-list2"
         (tag epm-rally)
         (specify "should return a feature alist generated from rally user stories csv file"
                  (expect (length (epm-rally-get-feature-list2 (concat epm-test-dir "rally-userstories.csv"))) equal  8))
         (specify "should return a feature alist with user stories ids as keys"
                  (expect (car (car (epm-rally-get-feature-list2 (concat epm-test-dir "rally-userstories.csv")))) equal 32))
         (specify "should return a feature alist with user stories names as values"
                  (expect (nth 1 (car (epm-rally-get-feature-list2 (concat epm-test-dir "rally-userstories.csv")))) equal
                          "user story 32")))

(context "epm-rally-to-epm"
         (tag epm-rally)
         (specify "should return an epm task"
                  (expect (epm-rally-to-epm-task
                           '("\"2.0\"" "\"TA84\"" "\"task name\"" "\"S47: user story 47\"" "\"V2.5\"" "\"2.5.Beta1\"" "\"In-Progress\"" "\"0.5\"" "\"0.6\"" "\"0.7\"" "\"\""))
                            equal '(47 84 "task name" 0.5 0.6 0.7 ""))))


(context "epm-rally-get-task-list"
         (tag epm-rally)
         (specify "should return a task list generated from rally csv file"
                  (let ((tlist (epm-rally-get-task-list (concat epm-test-dir "rally.csv"))))
                    (expect (length tlist) equal  51)
                    (expect (car tlist) equal '(1 2 "task name" 0.5 1.0 2.0 "")))))


;;; behave-epm-rally.el ends here