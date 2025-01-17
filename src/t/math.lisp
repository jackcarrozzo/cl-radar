(in-package :cl-user)
(defpackage cl-radar.test-math
  (:use :cl :clunit))
(in-package :cl-radar.test-math)

(cl-syntax:use-syntax :annot)

;; TODO: use cl-radar-test instead of cl-radar.test-x

(defsuite MathSuite (CL-RADAR.TEST-MAIN::MAINSUITE))
(defsuite ArraySuite (MathSuite))

;; ----------

(defsuite BasicArraySuite (ArraySuite))

(deftest test-float-array-mostly-equal-p (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       #(1.1 2.2 3.3) #(1.1 2.2 3.300000000000001)))

  (assert-false
      (cl-radar.math:float-array-mostly-equal-p
       #(1.1 2.2) #(1.1 2.2 3.3)))

  (assert-false
      (cl-radar.math:float-array-mostly-equal-p
       #(1.1 2.2 3.3) #(1.1 2.2)))

  (assert-false
      (cl-radar.math:float-array-mostly-equal-p
       #(1.1 2.2) nil))

  (assert-false
      (cl-radar.math:float-array-mostly-equal-p
       nil #(1.1 2.2))))

(deftest test-float-multidim-array-mostly-equal-p (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       #2A((1.1 2.2 3.3)
           (1.1 2.2 3.3))
       #2A((1.1 2.2 3.3)
           (1.1 2.2 3.300000000000001))))

  (assert-false
      (cl-radar.math:float-multidim-array-mostly-equal-p
       #2A((1.1 2.2 3.3)
           (1.1 2.2 3.3))
       #2A((1.1 2.2 3.3)
           (1.1 2.2 9.0))))

  (assert-false
      (cl-radar.math:float-multidim-array-mostly-equal-p
       #2A((1.1 2.2 3.3)
           (1.1 2.2 3.3))
       #2A((1.1 2.2 9.0)
           (1.1 2.2 3.3)))))

;; something very odd going on with this one, where it
;;  passes the first time the file is loaded but fails after
#|
(deftest test-db-ulize-array (BasicArraySuite)
  (let ((r #(1.1 9.9 3.3 2.0)))
    (setf r #(1.1 9.9 3.3 2.0))
    (cl-radar.math::db-ulize-array r)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         r
         #(-9.542425 0.0 -4.7712126 -6.9460516))
      r))

  (let ((r #(2.0)))
    (setf r #(2.0))
    (cl-radar.math::db-ulize-array r)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         r
         #(0.0))
      r))

  (let ((r #(2.0 1.0 1.0)))
    (cl-radar.math::db-ulize-array r)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         r
         #(0.0 -3.0102997 -3.0102997))
      r))

  (let ((r #(2.0 1.0 1.0 3.0)))
    (cl-radar.math::db-ulize-array r)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         r
         #(-1.7609124 -4.7712126 -4.7712126 0.0))
      r)))
|#

(deftest test-complex-ar-mags (BasicArraySuite)
  (let ((in #(#C(2.4 0.1) #C(1.2 0.2) #C(0.8 0.9) #C(0.5 0.4)))
        (out #(9.9 8.8)))
    (cl-radar.math::complex-ar-mags in out)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         out
         #(2.4020824 1.2165525))))
  )

(deftest test-complex-ar-mags-incf (BasicArraySuite)
  (let ((in #(#C(2.4 0.1) #C(1.2 0.2) #C(0.8 0.9) #C(0.5 0.4)))
        (out #(2.0 3.0)))
    (cl-radar.math::complex-ar-mags in out :incf)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         out
         #(4.4020824 4.2165525)))))

(deftest test-fft-and-sum-slices (BasicArraySuite)
  (setf cl-radar.math::*loop-fft-sums* nil) ;; TODO:

  (with-output-to-string (*standard-output*)
    (cl-radar.math::fft-and-sum-slices
     (list
      #(1.1 2.2 3.3 4.4 4.4 4.4 4.4 4.4)
      #(5.5 5.5 5.5 5.5 8.8 8.8 8.8 8.8))))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       cl-radar.math::*loop-fft-sums*
       #(28.565852922619673d0 21.710768274298538d0 9.545894876356686d0
         3.2741580915929624d0))))

(deftest test-sum-arrays (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::sum-arrays (list #(3.3 4.4)))
       #(3.299999952316284d0 4.400000095367432d0)))

  (cl-radar.math::sum-arrays (list #(3.3 4.4) #(9.9 1.1)))
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::sum-arrays (list #(3.3 4.4) #(9.9 1.1)))
       #(13.199999570846558d0 5.5000001192092896d0))))


(deftest test-subseq-array (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 1)
       #(2 3 4 1.1)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 2)
       #(3 4 1.1)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 2 1)
       #(3)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 0 1)
       #(1)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 0)
       #(1 2 3 4 1.1)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 4)
       #(1.1)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 4 1)
       #(1.1)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::subseq-array #(1 2 3 4 1.1) 4 0)
       #())))

(deftest test-array-average (BasicArraySuite)
  (assert-equal
      (cl-radar.math::array-average #(3.3 4.4 1.1 9.9))
      4.675)

  (assert-equal
      (cl-radar.math::array-average #(3.0 4.0))
      3.5))

(deftest test-array-add-offset (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::array-add-offset #(1.0 2.0 -1.0 0.0 -30.0 20.0) 0.2)
       #(1.2 2.2 -0.8 0.2 -29.8 20.2)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::array-add-offset #(1.0 2.0 -1.0 0.0 -30.0 20.0) 0.0)
       #(1.0 2.0 -1.0 0.0 -30.0 20.0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::array-add-offset #(1.0 2.0 -1.0 0.0 -30.0 20.0) -2.3)
       #(-1.3 -0.29999995 -3.3 -2.3 -32.3 17.7))))

(deftest test-array-multiply-scalar (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::array-multiply-scalar #(1.0 2.0 -1.0 0.0 -30.0 20.0) 0.2)
       #(0.2 0.4 -0.2 0.0 -6.0 4.0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::array-multiply-scalar #(1.0 2.0 -1.0 0.0 -30.0 20.0) 0.0)
       #(0.0 0.0 0.0 0.0 0.0 0.0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::array-multiply-scalar #(1.0 2.0 -1.0 0.0 -30.0 20.0) -2.3)
       #(-2.3 -4.6 2.3 -0.0 69.0 -46.0))))

(deftest test-array-copy-into (BasicArraySuite)
  (let ((in-ar #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8)))
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (cl-radar.math:array-copy-into in-ar 1 3 #(0.0 0.0 0.0 0.0))
         #(2.2 3.3 0.0 0.0)))

    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (cl-radar.math:array-copy-into in-ar 4 3 #(0.0 0.0 0.0 0.0)) ;; TODO:
         #(0.0 0.0 0.0 0.0)))

    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (cl-radar.math:array-copy-into in-ar 4 6 #(0.0 0.0 0.0 0.0))
         #(5.5 6.6 0.0 0.0)))

    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (cl-radar.math:array-copy-into in-ar 0 3 #(0.0 0.0 0.0 0.0))
         #(1.1 2.2 3.3 0.0)))

    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (cl-radar.math:array-copy-into in-ar 0 3 #(0.0 0.0 0.0 0.0) 1)
         #(0.0 1.1 2.2 3.3)))))

(deftest test-array-slice-into-chunks (BasicArraySuite)
  (let ((src-ar #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9)))
    (assert-true
        (cl-radar.math:list-of-arrays-equal
         (cl-radar.math:array-slice-into-chunks src-ar (list (cons 2 6) (cons 7 11)))
         '(#(3.3 4.4 5.5 6.6) #(8.8 9.9 1.1 2.2))))

    (assert-true
        (cl-radar.math:list-of-arrays-equal
         (cl-radar.math:array-slice-into-chunks src-ar (list (cons 2 6) (cons 7 11) (cons 8 9)))
         '(#(3.3 4.4 5.5 6.6) #(8.8 9.9 1.1 2.2) #(9.9))))

    (assert-true
        (let ((ol '(#(1.1 2.2 3.3 4.4) #(1.1 2.2 3.3 4.4) #(1.1 2.2 3.3 4.4) #(1.1 2.2 3.3 4.4))))
          (cl-radar.math:array-slice-into-chunks
           src-ar
           (list (cons 2 6) (cons 7 11)) ol)
          ;;(format t "ol is ~a~%" ol)
          (cl-radar.math:list-of-arrays-equal
           ol
           '(#(3.3 4.4 5.5 6.6) #(8.8 9.9 1.1 2.2) #(1.1 2.2 3.3 4.4) #(1.1 2.2 3.3 4.4)))))

    (assert-true
        (let ((ol '(#(1.5 2.5 3.3 4.4) #(1.1 2.2 3.3 4.4) #(1.1 2.2 3.3 4.4) #(1.1 2.2 3.3 4.4))))
          (cl-radar.math:array-slice-into-chunks
           src-ar
           (list (cons 0 2) (cons 3 5)  (cons 6 8) (cons 8 10)) ol)
          ;;(format t "ol is ~a~%" ol)
          (cl-radar.math:list-of-arrays-equal
           ol
           '(#(1.1 2.2 3.3 4.4) #(4.4 5.5 3.3 4.4) #(7.7 8.8 3.3 4.4) #(9.9 1.1 3.3 4.4)))))))

(deftest test-dc-center-slice (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:dc-center-slice #(3.3 1.1 2.2 5.5 1.1 4.4 0.1 9.1))
       #(-0.05000019 -2.25 -1.1500001 2.1499999 -2.25 1.05 -3.2500002 5.75))))

(deftest test-dc-center-slices (BasicArraySuite)
  (let ((slices
          (list #(1.0 2.0 1.0 2.0)
                #(1.0 3.0 1.0 3.0))))
    (cl-radar.math:dc-center-slices slices)
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (first slices)
         #(-0.5 0.5 -0.5 0.5)))
    (assert-true
        (cl-radar.math:float-array-mostly-equal-p
         (second slices)
         #(-1.0 1.0 -1.0 1.0)))))

(deftest test-array-max-min (BasicArraySuite)
  ;; TODO: needs to cover min too
  (assert-equal
      (cl-radar.math:array-max-min #(-3.3 0.0 2.2 9.9 0.1 -3.3))
      9.9))

(deftest test-array-list-max (BasicArraySuite)
  (assert-equal
      (cl-radar.math:array-list-max
       '(#(1.1 2.2 3.3 4.4)  #(-3.3 0.0 2.2 9.9 0.1 -3.3)))
      9.9))

;; -----------

(defsuite DownsamplerSuite (ArraySuite))

(deftest test-downsampler-1 (DownsamplerSuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        3
        #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 3.1 4.1 5.1))
       #(2.2 5.5 8.8 2.3999999)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        3
        #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 3.1 4.1 5.1 3.2))
       #(2.2 5.5 8.8 2.3999999)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        3
        #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2))
       #(2.2 5.5 8.8 1.7333332 4.133333)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        3
        #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2)
        #(0 0 0 0 0 0))
       #(2.2 5.5 8.8 1.7333332 4.133333 0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        3
        #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2)
        #(0 0 0 0 0))
       #(2.2 5.5 8.8 1.7333332 4.133333)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        3
        #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2)
        #(0 0 0 0))
       #(2.2 5.5 8.8 1.7333332))))

(deftest test-downsampler-2 (DownsamplerSuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        2
        #(1.1 2.2 3.3 4.4 5.5))
       #(1.6500001 3.85)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        2
        #(1.1 2.2 3.3 4.4))
       #(1.6500001 3.85)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math:mean-downsampler
        2
        #(1.1 2.2 3.3 4.4 6.6 7.7))
       #(1.6500001 3.85 7.1499996))))

(deftest test-next-power-of-two (BasicArraySuite)
  (assert-equal
      (cl-radar.math::next-power-of-two 11)
      16)

  (assert-equal
      (cl-radar.math::next-power-of-two 1)
      1)

  (assert-equal
      (cl-radar.math::next-power-of-two 2)
      2)

  (assert-equal
      (cl-radar.math::next-power-of-two 3)
      4)

  (assert-equal
      (cl-radar.math::next-power-of-two 4)
      4)

  (assert-equal
      (cl-radar.math::next-power-of-two 5)
      8)

  (assert-equal
      (cl-radar.math::next-power-of-two 3254657)
      4194304))


(deftest test-zero-pad-and-copy (BasicArraySuite)
  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::zero-pad-and-copy #(1.0 2.0 3.0 4.0 5.0 6.0) 2 5)
       #(3.0 4.0 5.0 0.0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::zero-pad-and-copy #(1.0 2.0 3.0 4.0 5.0 6.0) 2 6)
       #(3.0 4.0 5.0 6.0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::zero-pad-and-copy #(1.0 2.0 3.0 4.0 5.0 6.0 4.4 2.2) 2 7)
       #(3.0 4.0 5.0 6.0 4.4 0.0 0.0 0.0)))

  (assert-true
      (cl-radar.math:float-array-mostly-equal-p
       (cl-radar.math::zero-pad-and-copy #(1.0 2.0 3.0 4.0 5.0 6.0 4.4 2.2) 2 8)
       #(3.0 4.0 5.0 6.0 4.4 2.2 0.0 0.0))))



;;(print (run-suite 'NumberSuite))


#|
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) #(0 0 0 0 ))

#(2.2 5.5 8.8 1.7333332)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) #(0 0 0 0 0))
#(2.2 5.5 8.8 1.7333332 4.133333)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) #(0 0 0 0 0 0))
#(2.2 5.5 8.8 1.7333332 4.133333 0)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) )
#(2.2 5.5 8.8 1.7333332 4.133333)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 3.1 4.1 5.1 3.2) )
#(2.2 5.5 8.8 2.3999999)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 3.1 4.1 5.1 ) )
#(2.2 5.5 8.8 2.3999999)
|#
