(in-package :cl-user)
(defpackage cl-radar.test-fmcw
  (:use :cl :clunit))
(in-package :cl-radar.test-fmcw)

(cl-syntax:use-syntax :annot)


(defsuite FmcwSuite (cl-radar.test-main::mainsuite))

(defsuite TriggerSuite (FmcwSuite))

(defsuite EdgeSuite (TriggerSuite))

(deftest test-apply-edge-offsets (EdgeSuite)
  (assert-equal
      (cl-radar.fmcw:apply-edge-offsets
       '((100 . 200) (300 . 400))
       :start-offset 50 :end-offset 30 :debug-p nil)
      '((150 . 170) (350 . 370)))
  (assert-equal
      (cl-radar.fmcw:apply-edge-offsets '((100 . 200)) :debug-p nil)
      (list (cons
             (+ 100 cl-radar.fmcw::+trigger-offset-start-samples+)
             (- 200 cl-radar.fmcw::+trigger-offset-end-samples+)))))

(deftest test-edge-list-avg-length (EdgeSuite)
  (assert-equal
      (cl-radar.fmcw::edge-list-avg-length '((5 . 10) (10 . 16) (9 . 13)))
      5.0))

(deftest test-filter-wack-edges (EdgeSuite)
  (multiple-value-bind (res before after)
      (cl-radar.fmcw::filter-wack-edges '((5 . 10) (10 . 16) (9 . 13) (2 . 3) (11 . 21)))
    (assert-equal res '((5 . 10) (10 . 16) (9 . 13)))
    (assert-equal before 5.2)
    (assert-equal after 5.0)))

(deftest test-find-edges (EdgeSuite)
  (multiple-value-bind (r period)
      (cl-radar.fmcw::find-edges
       #(-0.5 -0.4 -0.3 0.2 0.3 0.4 -0.4 -0.5 -0.6 -0.2 0.1 0.2 0.3 0.4 0.5 0.6 0.2 -0.1 -0.2 -0.3 -0.4 -0.5))
    (assert-equal r '((3 . 6) (10 . 17)))
    (assert-equal period 7.0))

  (multiple-value-bind (r period)
      (cl-radar.fmcw::find-edges
       #(-0.5 -0.4 -0.3 0.2 0.3 0.4 -0.4 -0.5 -0.6 -0.2 0.1 0.2 0.3 0.4 0.5 0.6 0.2 -0.1 -0.2 -0.3 -0.4 -0.5 0.5 0.9))
    (assert-equal r '((3 . 6) (10 . 17)))
    (assert-equal period 9.5))

  (multiple-value-bind (r period)
      (cl-radar.fmcw::find-edges
       #(-0.5 -0.4 -0.3 0.2 0.3 0.4 -0.4 -0.5 -0.6 -0.2 0.1 0.2 0.3 0.4 0.5 0.6 0.2 -0.1 -0.2 -0.3 -0.4 -0.5 0.5 0.9 0.8 0.9 -0.2 -0.1 0.5))
    (assert-equal r '((3 . 6) (10 . 17) (22 . 26)))
    (assert-equal period 8.333333)))

(deftest test-find-ac-edges (EdgeSuite)
  (cl-radar.audio:read-wav "../data/captrigfast_stereo_vshort.wav" t)
  (assert-equal
      (cl-radar.fmcw::find-ac-edges
       cl-radar.audio::*last-left-samps*
       :min-slope 0.1)
      '((439 . 912) (1146 . 1618))))
