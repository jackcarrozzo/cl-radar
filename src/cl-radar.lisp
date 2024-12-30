(in-package :cl-user)
(defpackage cl-radar
  (:use :cl
        :simple-date-time))
(in-package :cl-radar)
(cl-syntax:use-syntax :annot)

;; (ql-dist:install-dist "http://dists.cl21.org/cl21.txt")
;; (ql:quickload 'cl21)
