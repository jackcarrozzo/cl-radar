(in-package :cl-user)
(defpackage cl-radar.test-main
  (:use :cl :clunit))
(in-package :cl-radar.test-main)

(cl-syntax:use-syntax :annot)

(defsuite MainSuite ())

@export
(defun run ()
  (ql:quickload 'cl-radar)
  (print (run-suite 'MainSuite))
  (format t "~%~%-- k.~%"))

;;; sbcl --non-interactive --eval "(ql:quickload 'cl-radar)" --eval "(cl-radar.test-main:run)"

;; tests needed: (TODO:)
;; - fmcw coverage
;; - generate stereo wavs of known shapes and run wav reading stuff on them
;; - wavs into fmcw parts
;; - chopper, edge mgmt code, remove wack edges etc
;; - zero padding and whatnots
;; - edges of chunk stuff
