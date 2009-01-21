;;; behave-epm.el --- behave for epm
;; Copyright (C) 2007 Dan Pomohaci (dpom)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.


(add-to-list 'load-path "../../src/lisp")
(load-file "../../src/lisp/epm.el")

(require 'behave)

(defvar epm-test-dir "/home/users/dan/pers/project/dpom/epm/test/lisp"
  "epm test directory")

(defvar epm-test-project (expand-file-name "V2.5.muse" epm-test-dir)
  "epm test project")

;; Tasks functions

;; Features functions

(context "epm-feature-print"
         (tag epm)
         (specify "should return a feature printable representation as it is in project file"
                  (expect (epm-feature-print '(20 "Feature name")) equal  "** 20. Feature name\n\n")))

;; Project functions

(context "epm-goto-endtask"
         (tag epm)
         (specify "should return the task end position"
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (goto-char 641)
                    (expect (epm-goto-endtask) equals 686))))

(context "epm-goto-feature-end"
         (tag epm)
         (specify "should move the current point to the end of a feature section found between min and max points"
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (expect (epm-goto-feature-end 102 803 1057) 1034)))
         (specify "should move the current point to the max point if it dosen't found feature")
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (expect (epm-goto-feature-end 504 803 1057) equals 1057))
         (specify "should move the current point to the max point if the feature is the last feature from Done section")
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (expect (epm-goto-feature-end 103 803 1057) equals 1057)))

(context "epm-get-planned-section"
         (tag epm)
         (specify "should return the beginning (after title) and the ending points of the Planned section"
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (expect (epm-get-planned-section) equals '(385 590)))))

(context "epm-get-working-section"
         (tag epm)
         (specify "should return the beginning (after title) and the ending points of the Working section"
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (expect (epm-get-working-section) equals '(599 796)))))

(context "epm-get-done-section"
         (tag epm)
         (specify "should return the beginning (after title) and the ending points of the Working section"
                  (with-temp-buffer
                    (insert-file-contents epm-test-project)
                    (expect (epm-get-done-section) equals '(802 1057)))))



;;; behave-epm.el ends here