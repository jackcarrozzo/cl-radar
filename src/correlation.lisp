(in-package :cl-user)
(defpackage cl-radar.corr
  (:use :cl))
(in-package :cl-radar.corr)
(cl-syntax:use-syntax :annot)


;; barker codes- the first few remain here for completeness but prolly
;;   dont make a lot of sense to use for radar..

@export
(defparameter +barker-codes+
  '((1)
    (1 -1)
    (1 1 -1)
    (1 1 -1 1)
    (1 1 1 -1 1)
    (1 1 1 -1 -1 1 -1)
    (1 1 1 -1 -1 -1 1 -1 -1 1 -1)
    (1 1 1 1 1 -1 -1 1 1 -1 1 -1 1)))


;; first-relative-prime and all-relative-primes now in .math

#|
(vgplot:plot (cl-radar.math:complex-imags (cl-radar.corr:zadoff-chu-sequence 120 :root 2)))

root gives how many times thru the sequeuence it goes during length
|#


;; exp(-j * pi * root * (n + offset) * (n + offset + 1) / length)
@export
(defun zadoff-chu-sequence (length &key (root 1) (offset 0.0))
  (declare (type fixnum length)
           (type (or integer float) root)
           (type float offset))
  (let ((seq (make-array length :initial-element #C(0.0d0 0.0d0))))
    (dotimes (n length)
      (let* ((n+q (+ n (float offset)))
             (arg (/ (* Pi
                        root
                        n+q
                        (+ n+q 1.0d0))
                     length))
             (theta (- arg)))
        (setf (aref seq n)
              (complex (cos theta)
                       (sin theta)))))   ; e^(j * theta)
    seq))




;;;;;;;;;;;;;;; correlators

;; TODO: the complex array stuff can prolly go into math or util

;; complex correlation - real and inverse imag - complex conj convolution
@export
(defun complex-mag (v)
  (sqrt
   (+ (expt (realpart v) 2)
      (expt (imagpart v) 2))))

;; reverse with each element the complex conjugate of its input
(defun reverse-and-conjugate (vec)
  (let ((n (length vec))
        (res (make-array (length vec) :initial-element #c(0.0d0 0.0d0))))
    (loop for i from 0 below n do
         (setf (aref res (- n 1 i))
               (conjugate (aref vec i))))
    res))

;; multiply b into a
(defun pointwise-multiply! (a b)
  (loop for i from 0 below (length a) do
        (setf (aref a i) (* (aref a i) (aref b i)))))

(defun fft-inplace! (vec)
  (bordeaux-fft:fft! vec vec))

(defun ifft-inplace! (vec)
  (bordeaux-fft:ifft! vec vec))

(defun scale-array! (vec scalar)
  (loop for i from 0 below (length vec) do
        (setf (aref vec i) (* (aref vec i) scalar))))

;; bigger means less difference
;; inner product method
@export
(defun complex-pt-correlation (p1 p2)
  (complex-mag (* p1 (conjugate p2))))

;; bigger means more difference
;; euclidean distance method
@export
(defun complex-vector-euclidean-dist (v1 v2) ;; mean squared err
  (sqrt
   (+ (expt (- (realpart v1) (realpart v2)) 2)
      (expt (- (imagpart v1) (imagpart v2)) 2))))

@export
(defun complex-correlation (ref-ar sig-ar &optional (compare-fn #'complex-pt-correlation))
  (assert (= (length ref-ar) (length sig-ar)))
  (let ((sum 0.0d0))
    (dotimes (i (length ref-ar))
      (incf sum
            (funcall compare-fn (aref ref-ar i) (aref sig-ar i))))
    (/ sum (float (length ref-ar)))))

@export
(defun sliding-complex-correlation (ref-ar sig-ar &optional (search-n 100))
  (let ((r (make-array search-n :initial-element 0.0d0)))
    (dotimes (i search-n)
      (setf (aref r i)
            (complex-correlation
             (subseq sig-ar i)
             (subseq ref-ar 0 (- (length ref-ar) i)))))
    r))
