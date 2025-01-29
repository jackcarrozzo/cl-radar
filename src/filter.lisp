(in-package :cl-user)
(defpackage cl-radar.filter
  (:use :cl))
(in-package :cl-radar.filter)
(cl-syntax:use-syntax :annot)


;;;;;;;; testing utils

@export
(defun filter-testing-comb-ar ()
  (fill-signal-ar-sines
   (loop for f from 250 upto 10000 by 250 collecting f)))

#|
(progn (setf gg (cl-radar.filter::filter-testing-comb-ar)) 7)
(progn (setf g2 (bordeaux-fft:windowed-fft gg 256 512)) 7)
(progn (setf g3 (cl-radar.math:complex-ar-mags g2 (make-array 256 :initial-element 0.0d0))) 7)
(vgplot:plot g3)
|#

(defvar *last-comb-ar* nil)
(defvar *last-comb-fft* nil)

@export
(defun comb-test-wrapper ()
  (vgplot:plot
   (setf *last-comb-fft*
         (cl-radar.math:complex-ar-mags
          (bordeaux-fft:windowed-fft
           (setf *last-comb-ar*
                 (filter-testing-comb-ar))
           256 512)
          (make-array 256 :initial-element 0.0d0)))))

(defvar *last-filtered-ar* nil)

(defun filter1 (&optional (in-ar *last-comb-ar*))
  (let ((hpf (make-fir-highpass-filter 48000.0 4000.0 :order 51))
        (r (make-array (array-dimension in-ar 0) :initial-element 0.0d0)))
    (dotimes (i (array-dimension in-ar 0))
      (setf (aref r i)
            (funcall hpf (aref in-ar i))))
    (setf *last-filtered-ar*
          r)))

#|
(progn (setf f1 (cl-radar.filter::filter1)) 7)
(vgplot:plot cl-radar.filter::*last-comb-ar*)
(vgplot:plot f1)

(setf g2 (bordeaux-fft:windowed-fft f1 256 512))
(progn (setf g3 (cl-radar.math:complex-ar-mags g2 (make-array 256 :initial-element 0.0d0))) 7)
(vgplot:plot g3)
|#

(defvar *last-filter-fft* nil)

(defun run-filter-fft ()
  (vgplot:plot
   (setf *last-filter-fft*
         (cl-radar.math:complex-ar-mags
          (bordeaux-fft:windowed-fft
           (filter1
            (filter-testing-comb-ar))
           256 512)
          (make-array 256 :initial-element 0.0d0)))))

;; broken for list-arg freq-mag-list members TODO:
@export
(defun fill-signal-ar-sines (freq-mag-list
                             &key (n-samples 512)
                               (sample-rate 48000)
                               (debug-p t))
  (let ((r (make-array n-samples :initial-element 0.0d0)))
    (loop for f-mag in freq-mag-list
           do
              (let ((arg-type (type-of f-mag))
                    (this-f 1000.0)
                    (this-mag 1.0))
                (cond
                  ((or (equal arg-type 'single-float) ;; if its just a number
                       (equal arg-type 'double-float)
                       (equal (first arg-type) 'integer))
                   (setf this-f f-mag))
                  ((eql 'consff arg-type)             ;; if arg is a list
                   (format t "-- here.~%")
                   (setf this-f (or (first f-mag) t))
                   (setf this-mag (second f-mag)))
                  (t
                   (format t "-- unknown freq-mag-list item type, skipping: ~a, item: ~a~%"
                           arg-type f-mag)
                   (setf this-f nil)))
                (when this-f
                  (when debug-p
                    (format t "-- adding sine of f ~a, amplitude ~a.~%"
                            this-f this-mag))

                  (setf cl-radar.wavegen::*phase* 0.0) ;; TODO:

                  (dotimes (i n-samples)
                    (incf (aref r i)
                          (cl-radar.wavegen:sines-get-next
                           sample-rate this-f this-mag))))))
    r))



;;; FIR highpass real

;;; windowed-sinc (spectral inversion) approach with hamming window
@export
(defun fir-highpass-coefficients (sample-rate cutoff &key (order 101))
  (let* ((coeffs (make-array order :initial-element 0.0))
         ;; midpoint index (for linear-phase symmetry)
         (alpha (/ (float (1- order)) 2.0))
         ;; normalized angular cutoff frequency (in radians/sample)
         (wc (* 2.0 pi (/ cutoff sample-rate))))

    (dotimes (n order)
      (let* ((m (- n alpha))
             ;; ideal high-pass = sinc(pi*m) - sinc(wc*m),
             ;; with the special case at m=0
             (ideal
               (cond
                 ((= m 0.0)
                  ;; limit as m->0 for [sin(pi*m) - sin(wc*m)]/(pi*m)
                  (- 1.0 (/ wc pi)))
                 (t
                  (/
                    (- (sin (* pi m))
                       (sin (* wc m)))
                    (* pi m)))))
             ;; hamming window:
             ;; w[n] = 0.54 - 0.46 cos(2*pi*n/(order-1))
             (w (let ((ratio (/ n (float (1- order)))))
                  (- 0.54
                     (* 0.46 (cos (* 2.0 pi ratio))))))
             (val (* ideal w)))
        (setf (aref coeffs n) val)))
    coeffs))

;; returns a closure containing the filter
@export
(defun make-fir-highpass-filter (sample-rate cutoff
                                &key (order 101))
  (let* ((coeffs (fir-highpass-coefficients sample-rate cutoff :order order))
         ;; FIR state delay line (crb)
         (state  (make-array order :initial-element 0.0))
         (pos    0))
    (lambda (sample)
      ;; newest sample into the crb
      (setf (aref state pos) sample)
      (incf pos)
      (when (>= pos order)
        (setf pos 0))
      ;; FIR convolution
      (let ((acc 0.0)
            (idx pos))
        (dotimes (i order)
          (decf idx)
          (when (< idx 0)
            (setf idx (1- order)))
          (incf acc (* (aref coeffs i)
                       (aref state idx))))
        acc))))

(defun fir-ex ()
  (let ((my-hpf (make-fir-highpass-filter 48000.0 1000.0 :order 101)))
    (dotimes (i 10)
      (format t "~&input=1.0 => output=~F" (funcall my-hpf 1.0)))))


;;;;; FIR highpas complex

;; TODO: same as above? real coefs for both filter types
@export
(defun fir-highpass-coefficients-complex (sample-rate cutoff &key (order 101))
  (let* ((coeffs (make-array order :initial-element 0.0))
         (alpha (/ (float (1- order)) 2.0))
         (wc (* 2.0 pi (/ cutoff sample-rate))))  ;; normalized rad freq

    (dotimes (n order)
      (let* ((m (- n alpha))
             ;; ideal high-pass = sinc(pi*m) - sinc(wc*m)
             (ideal (cond
                      ((= m 0.0)
                       (- 1.0 (/ wc pi)))
                      (t
                       (/
                        (- (sin (* pi m))
                           (sin (* wc m)))
                        (* pi m)))))
             ;; hamming window
             (w (let ((ratio (/ n (float (1- order)))))
                  (- 0.54
                     (* 0.46 (cos (* 2.0 pi ratio))))))
             (val (* ideal w)))
        (setf (aref coeffs n) val)))
    coeffs))

@export
(defun make-fir-highpass-filter-complex (sample-rate cutoff
                                     &key (order 101))
  (let* ((coeffs (fir-highpass-coefficients-complex sample-rate cutoff :order order))
         (state  (make-array order :initial-element #C(0d0 0d0)))
         (pos    0))
    (lambda (sample)
      ;; complex sample into circular buffer
      (setf (aref state pos) sample)
      (incf pos)
      (when (>= pos order)
        (setf pos 0))
      ;; convolution
      (let ((acc #C(0d0 0d0))
            (idx pos))
        (dotimes (i order)
          (decf idx)
          (when (< idx 0)
            (setf idx (1- order)))
          ;; multiply the real coefficient by the complex sample,
          ;; then add to the complex accumulator
          (setf acc (complex-plus acc
                                  (complex-times (aref coeffs i)
                                                 (aref state idx)))))
        acc))))

;; TODO: move to math

(defun complex-plus (z1 z2)
  (complex (+ (realpart z1) (realpart z2))
           (+ (imagpart z1) (imagpart z2))))

(defun complex-times (real-coeff z)
  (complex (* real-coeff (realpart z))
           (* real-coeff (imagpart z))))

(defun complex-minus (z1 z2)
  (complex (- (realpart z1) (realpart z2))
           (- (imagpart z1) (imagpart z2))))

;;;;;; IIR highpass real

;; standard 2nd‐order “RBJ cookbook” highpass design (sometimes referred to as a “Butterworth‐style” or “Q=1/√2” approach)


;;; 1) coefficient generator for a 2nd-order RBJ cookbook hpf

@export
(defun iir-biquad-highpass-coeffs (sample-rate cutoff &key (q (/ 1.0 (sqrt 2.0))))
  "compute 2nd-order IIR high-pass filter coefficients (RBJ cookbook).
   returns (b0 b1 b2 a1 a2), already normalized by a0.
   Q defaults to 1/sqrt(2) for Butterworth-like response."
  (let* ((omega (* 2.0 pi (/ cutoff sample-rate)))  ; 2 * pi * (fc/fs)
         (cosw (cos omega))
         (sinw (sin omega))
         (alpha (/ sinw (* 2.0 q)))

         ;; biquad denominators
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cosw))
         (a2 (- 1.0 alpha))

         ;; biquad numerators for high-pass
         (b0 (/ (+ 1.0 cosw) 2.0))
         (b1 (- (+ 1.0 cosw)))
         (b2 (/ (+ 1.0 cosw) 2.0)))

    ;; normalize by a0
    (labels ((norm (x) (/ x a0)))
      ;;(list (nth-value 0 (norm b0)) (norm b1) (norm b2)
      ;;      (norm a1) (norm a2))
      (list (norm b0) (norm b1) (norm b2) (norm a1) (norm a2)))))

@export
(defun make-iir-highpass-filter (sample-rate cutoff
                                 &key (q (/ 1.0 (sqrt 2.0))))
  ;; q = resonance / damping factor (default = 1/sqrt(2) => Butterworth)
  (let* ((rl (iir-biquad-highpass-coeffs sample-rate cutoff :q q))
         (b0 (first rl))
         (b1 (second rl))
         (b2 (third rl))
         (a1 (fourth rl))
         (a2 (fifth rl)))

    ;; destrucuring-bind silently failed above for unknown reasons ^

    (format t "-- b0 is ~a~%" b0)
    (format t "-- b1 is ~a~%" b1)

    ;; https://stackoverflow.com/questions/60104101/common-lisp-difference-between-declare-check-type
#|
caught WARNING:
;   Derived type of COMMON-LISP-USER::B1 is
;     (VALUES NULL &OPTIONAL),
;   conflicting with its asserted type
;     NUMBER.
;   See also:
;     The SBCL Manual, Node "Handling of Types"
;
|#
    (check-type b0 number)
    (check-type b1 number)
    (check-type b2 number)
    (check-type a1 number)
    (check-type a2 number)


    ;; state vars: x[n-1], x[n-2], y[n-1], y[n-2]
    (let ((x1 0.0)
          (x2 0.0)
          (y1 0.0)
          (y2 0.0))
      (lambda (x0)
        (let* ((y0 (+ (* b0 x0)
                      (* b1 x1)
                      (* b2 x2)
                      (- (* a1 y1))
                      (- (* a2 y2)))))
          ;; shift states
          (setf x2 x1
                x1 x0
                y2 y1
                y1 y0)
          y0)))))


(defun iir-ex ()
  (let ((hpf (make-iir-highpass-filter 48000.0 1000.0)))
    (format t "processing samples:~%")
    (dolist (sample '(0.0 1.0 1.0 1.0 1.0 1.0))
      (format t "~&input=~a  ->  output=~a" sample (funcall hpf sample)))))

;;;;;;;;; IIR highpass complex

;; TODO: also same as reals
@export
(defun iir-biquad-highpass-coeffs-complex (sample-rate cutoff
                                           &key (q (/ 1.0 (sqrt 2.0))))
  (let* ((omega (* 2.0 pi (/ cutoff sample-rate)))
         (cosw (cos omega))
         (sinw (sin omega))
         (alpha (/ sinw (* 2.0 q)))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cosw))
         (a2 (- 1.0 alpha))
         (b0 (/ (+ 1.0 cosw) 2.0))
         (b1 (- (+ 1.0 cosw)))
         (b2 (/ (+ 1.0 cosw) 2.0)))
    ;; normalize by a0
    (labels ((norm (x) (/ x a0)))
      (list (norm b0) (norm b1) (norm b2)
            (norm a1) (norm a2)))))

@export
(defun make-iir-highpass-filter-complex (sample-rate cutoff
                                       &key (q (/ 1.0 (sqrt 2.0))))
  (multiple-value-bind (b0 b1 b2 a1 a2)
      (apply #'iir-biquad-highpass-coeffs-complex sample-rate cutoff
             (append '() (list :q q)))
    (let ((x1 #C(0d0 0d0))
          (x2 #C(0d0 0d0))
          (y1 #C(0d0 0d0))
          (y2 #C(0d0 0d0)))
      (lambda (x0)
        ;; difference equation:
        ;; y0 = b0*x0 + b1*x1 + b2*x2 - a1*y1 - a2*y2
        (let ((y0 (complex-plus
                   (complex-plus
                    (complex-plus
                     (complex-times b0 x0)
                     (complex-times b1 x1))
                    (complex-times b2 x2))
                   (complex-minus
                    (complex-times a1 y1)
                    (complex-times a2 y2)))))
          ;; shift states
          (setf x2 x1
                x1 x0
                y2 y1
                y1 y0)
          y0)))))


(defun complex-both-ex ()

  (let ((fir-hpf-complex
          (make-fir-highpass-filter-complex
           48000.0 1000.0 :order 11)))
    (format t "FIR complex filter:~%")
    (dolist (in '(#C(1 0) #C(1 1) #C(0.5 -0.5) #C(-1 1) #C(0 0)))
      (format t "~&in=~a => out=~a"
              in (funcall fir-hpf-complex in))))

  (let ((iir-hpf-complex
          (make-iir-highpass-filter-complex
           48000.0 1000.0 :q 0.7071)))
    (format t "~%~%IIR complex filter:~%")
    (dolist (in '(#C(1 0) #C(1 1) #C(0.5 -0.5) #C(-1 1) #C(0 0)))
      (format t "~&in=~a => out=~a"
              in (funcall iir-hpf-complex in)))))
