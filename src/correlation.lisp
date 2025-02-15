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


;; TODO: put this one back in
#|
(defun make-gold-code-generator-simple
    (&key (taps1 '(6 5 0))
          (initial-state1 '(1 0 0 0 0 0 1))
          (taps2 '(6 1 0))
          (initial-state2 '(1 1 0 0 0 1 0))
          (offset 10))
  ;; local function to step an lfsr
  (labels ((step-lfsr (reg taps)
             (let* ((feedback (reduce #'cl-radar.math::xor
                                      (mapcar (lambda (tap)
                                                (nth (- (length reg) 1 tap) reg))
                                              taps)
                                      :initial-value 0))
                    (out (car (last reg)))
                    (shifted (butlast reg)))
               (values out (cons feedback shifted)))))
    (let ((reg1 (copy-list initial-state1))
          (reg2 (copy-list initial-state2)))
      ;; advance second lfsr 'offset' times
      (dotimes (i offset)
        (setf reg2
              (nth-value 1
                         (step-lfsr reg2 taps2))))
      (lambda (length)
        (let ((bits nil))
          (dotimes (i length)
            (multiple-value-bind (o1 n1) (step-lfsr reg1 taps1)
              (multiple-value-bind (o2 n2) (step-lfsr reg2 taps2)
                (push (cl-radar.math::xor o1 o2) bits)
                (setf reg1 n1
                      reg2 n2))))
(nreverse bits))))))
|#

;; TODO: still needed?
(defun zgcd (a b)
  (if (zerop b)
      a
      (gcd b (mod a b))))

(defun zlcm (a b)
  (if (or (zerop a) (zerop b))
      0
      (/ (abs (* a b)) (gcd a b))))

;; returns a generator closure and repeat length
@export
(defun make-gold-code-generator
    (&key (taps1 '(6 5 0))
          (initial-state1 '(1 0 0 0 0 0 1))
          (taps2 '(6 1 0))
          (initial-state2 '(1 1 0 0 0 1 0))
          (offset 10))
  ;; local lfsr step
  (labels ((step-lfsr (reg taps)
             (let* ((feedback (reduce #'cl-radar.math::xor
                                      (mapcar (lambda (tap)
                                                (nth (- (length reg) 1 tap) reg))
                                              taps)
                                      :initial-value 0))
                    (out (car (last reg)))
                    (shifted (butlast reg)))
               (values out (cons feedback shifted)))))
    (let* ((reg1 (copy-list initial-state1))
           (reg2 (copy-list initial-state2))
           (n1 (length reg1))
           (n2 (length reg2))
           (period1 (1- (ash 1 n1)))  ;; 2^n1 - 1
           (period2 (1- (ash 1 n2)))  ;; 2^n2 - 1
           (repeat-length (zlcm period1 period2)))
      ;; offset second lfsr
      (dotimes (i offset)
        (multiple-value-bind (o2 new-reg2)
            (step-lfsr reg2 taps2)
          (declare (ignore o2))
          (setf reg2 new-reg2)))
      (values (lambda (length)
                (let ((bits nil))
                  (dotimes (i length)
                    (multiple-value-bind (o1 new-reg1) (step-lfsr reg1 taps1)
                      (multiple-value-bind (o2 new-reg2) (step-lfsr reg2 taps2)
                        (push (cl-radar.math::xor o1 o2) bits)
                        (setf reg1 new-reg1
                              reg2 new-reg2))))
                  (nreverse bits)))
              repeat-length)))) ;; repeat-length is wrong seemingly? see graph

@export
(defun gold-gen-tester ()
  (multiple-value-bind (g repeat-n)
      (make-gold-code-generator :offset 3)
    (format t "first 8 bits: ~a~%" (funcall g 8))
    (format t "next 8 bits: ~a~%" (funcall g 8))
    (format t "64 bits: ~a~%" (funcall g 64))
    (format t "512 bits: ~a~%" (funcall g 512))
    (format t "repeats after ~a bits.~%" repeat-n)))

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

;; also works on arrays of reals
@export
(defun sliding-complex-correlation (ref-ar sig-ar &optional (search-n 100))
  (let ((r (make-array search-n :initial-element 0.0d0)))
    (dotimes (i search-n)
      (setf (aref r i)
            (complex-correlation
             (subseq sig-ar i) ;; sig-ar starts farther fwd each time
             (subseq ref-ar 0 (- (length ref-ar) i))))) ;; ref-ar one shorter each time so it can slide
    r))

;; this one is remarkable!
@export
(defun graph-gold-correlation (&key (sig-len 10000) (search-n 1000))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (format t "-- gold code repeats every ~a symbols/chips.~%" gold-repeat-n)
    (let* ((full-ref (make-array sig-len
                                 :initial-contents (funcall gold-gen sig-len)))
           (ref-slice (subseq full-ref 20))
           (sig-slice (subseq full-ref 0 (- (length full-ref) 20)))
           (corr-r (sliding-complex-correlation ref-slice sig-slice search-n)))
      (format t "-- corr-r came back len ~a.~%" (length corr-r))
      (vgplot:plot corr-r))))

;; this one is really good too
@export
(defun graph-gold-mixed (&key (sig-len 10000) (search-n 1000))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (format t "-- gold code repeats every ~a symbols/chips.~%" gold-repeat-n)
    (let* ((full-ref (make-array sig-len
                                 :initial-contents (funcall gold-gen sig-len)))
           (ref-slice (subseq full-ref 100))
           (sig1-slice (subseq full-ref 80 (- (length full-ref) 20)))
           (sig2-slice (subseq full-ref 50 (- (length full-ref) 50)))
           (sigs (cl-radar.math:sum-arrays (list sig1-slice sig2-slice)))
           (corr-r (sliding-complex-correlation ref-slice sigs search-n)))
      (format t "-- corr-r came back len ~a.~%" (length corr-r))
      (vgplot:plot corr-r))))

;; TODO: this prolly belongs elsewhere
@export
(defun symbols-to-n-samples (symbols-ar n-samples-per-sym)
  (let ((r (make-array (* n-samples-per-sym (length symbols-ar)) :initial-element 0.0)))
    (loop for i from 0 below (length symbols-ar)
          do
             (loop for j from 0 below n-samples-per-sym
                   do
                      (let ((r-index (+ (* i n-samples-per-sym) j)))
                        #|(format t "-- i ~a, j ~a, setting r ~a to symbol ~a: ~a~%"
                                i j (+ i j) r-index (aref symbols-ar i))|#
                        (setf (aref r r-index)
                              (aref symbols-ar i)))))
    r))

@export
(defun graph-gold-subsampled (&key (sig-len 10000) (search-n 1000) (expand-by-n 4))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (format t "-- gold code repeats every ~a symbols/chips.~%" gold-repeat-n)
    (let* ((full-ref
             (symbols-to-n-samples
              (make-array sig-len
                          :initial-contents (funcall gold-gen sig-len))
              expand-by-n))
           (ref-slice (subseq full-ref 100))
           (sig1-slice (subseq full-ref 80 (- (length full-ref) 20)))
           (sig2-slice (subseq full-ref 50 (- (length full-ref) 50)))
           (sigs (cl-radar.math:sum-arrays (list sig1-slice sig2-slice)))
           (corr-r (sliding-complex-correlation ref-slice sigs search-n)))
      (format t "-- corr-r came back len ~a.~%" (length corr-r))
      (vgplot:plot corr-r))))

;; TODO: this also should live elsewhere
@export
(defun resample-signal (ratio input-ar)
  (assert (and (>= ratio 0.5) (<= ratio 2.0)))

  (let* ((in-len (length input-ar))
         ;; at 'half speed' (ratio 0.5), out-len = 2*in-len;
         ;; at double speed (ratio 2), outlen = in-len/2
         ;; maybe it should be the other way around tbh
         (out-len (round (/ in-len ratio)))
         (output (make-array out-len :initial-element 0.0)))
    (dotimes (i out-len)
      (let* ((x (* i ratio))
             (floor-x (floor x))
             (alpha (- x floor-x))
             ;; clamp next-x to stay within array
             (next-x (min (1+ floor-x) (1- in-len)))
             (s0 (aref input-ar floor-x))
             (s1 (aref input-ar next-x)))
        ;; linear interpolation: y = s0*(1-alpha) + s1*alpha
        (setf (aref output i)
              (+ (* (- 1 alpha) s0)
                 (* alpha s1)))))
    output))

;; another good one- at freq ratio 1.01, sig-len 800 is still a good peak but much lower
;;   at 2000, small lump at 5000, gone at 10000 gone above with expand-by-n 4
;;
;; (cl-radar.corr::graph-gold-freq-diff :freq-ratio 1.001 :sig-len 5000)
;;
;; without expanding, peak and smear much more visible with
;;
;; (cl-radar.corr::graph-gold-freq-diff :freq-ratio 1.001 :sig-len 5000 :expand-by-n 1)
;; (cl-radar.corr::graph-gold-freq-diff :freq-ratio 1.002 :sig-len 5000 :expand-by-n 1)
;;

@export
(defun graph-gold-freq-diff (&key (sig-len 800) (search-n 200) (expand-by-n 4) (freq-ratio 1.01))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (format t "-- gold code repeats every ~a symbols/chips.~%" gold-repeat-n)
    (let* ((full-ref
             (symbols-to-n-samples
              (make-array sig-len
                          :initial-contents (funcall gold-gen sig-len))
              expand-by-n))
           (ref-slice (subseq full-ref 100))
           (sig1-slice (subseq full-ref 80 (- (length full-ref) 20)))
           (resampled (resample-signal freq-ratio
                                       (subseq full-ref 50
                                               (- (length full-ref) 50))))
           (sig2-slice (subseq
                        resampled
                        0 (min (length resampled) (- (length full-ref) 50))))
           (sigs (cl-radar.math:sum-arrays (list sig1-slice sig2-slice)))
           (corr-r (sliding-complex-correlation ref-slice sigs search-n)))
      (format t "-- corr-r came back len ~a.~%" (length corr-r))
      (vgplot:plot corr-r))))
