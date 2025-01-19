(in-package :cl-user)
(defpackage cl-radar.filter
  (:use :cl))
(in-package :cl-radar.filter)
(cl-syntax:use-syntax :annot)



;;; FIR highpass real

;;; Helper function to generate high-pass coefficients using
;;; the windowed-sinc (spectral inversion) approach with a Hamming window.
@export
(defun fir-highpass-coefficients (sample-rate cutoff &key (order 101))
  "Compute FIR high-pass filter coefficients of length ORDER,
   given SAMPLE-RATE and CUTOFF frequency in Hz.
   Uses a Hamming window by default."
  (let* ((coeffs (make-array order :initial-element 0.0))
         ;; Midpoint index (for linear-phase symmetry)
         (alpha (/ (float (1- order)) 2.0))
         ;; Normalized angular cutoff frequency (in radians/sample)
         (wc (* 2.0 pi (/ cutoff sample-rate))))

    (dotimes (n order)
      (let* ((m (- n alpha))
             ;; Ideal high-pass = sinc(pi*m) - sinc(wc*m),
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
             ;; Hamming window:
             ;; w[n] = 0.54 - 0.46 cos(2*pi*n/(order-1))
             (w (let ((ratio (/ n (float (1- order)))))
                  (- 0.54
                     (* 0.46 (cos (* 2.0 pi ratio))))))
             (val (* ideal w)))
        (setf (aref coeffs n) val)))

    ;; (Optional) Could normalize or adjust gain if desired.

    coeffs))

;;; Main function that returns a closure implementing the FIR filter.
@export
(defun make-fir-highpass-filter (sample-rate cutoff
                                &key (order 101))
  "Create a high-pass FIR filter closure, given SAMPLE-RATE and CUTOFF (Hz).
   ORDER is the number of coefficients/taps in the FIR filter.
   Returns a function that takes one sample at a time and returns the filtered sample."
  (let* ((coeffs (fir-highpass-coefficients sample-rate cutoff :order order))
         ;; Internal delay line (circular buffer) for FIR state
         (state  (make-array order :initial-element 0.0))
         (pos    0))
    (lambda (sample)
      "Call this closure with successive samples to get filtered outputs."
      ;; Insert the newest sample into the circular buffer
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
  ;; Create a filter with default order = 101 taps,
  ;; sample-rate = 48000 Hz, cutoff = 1000 Hz
  (let ((my-hpf (make-fir-highpass-filter 48000.0 1000.0 :order 101)))
    ;; Filter some samples:
    (dotimes (i 10)
      (format t "~&Input=1.0 => Output=~F" (funcall my-hpf 1.0)))
    ;; ...
    ))


;;;;; FIR highpas complex

@export
(defun fir-highpass-coefficients-complex (sample-rate cutoff &key (order 101))
  "Compute real FIR high-pass filter coefficients of length ORDER,
   for use with complex input. The design is identical to the real version,
   but we return real-valued coefficients that you can apply to complex samples.

   sample-rate : float
   cutoff      : float
   order       : integer (number of taps)"
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
             ;; Hamming window
             (w (let ((ratio (/ n (float (1- order)))))
                  (- 0.54
                     (* 0.46 (cos (* 2.0 pi ratio))))))
             (val (* ideal w)))
        (setf (aref coeffs n) val)))
    coeffs))

@export
(defun make-fir-highpass-filter-complex (sample-rate cutoff
                                     &key (order 101))
  "Return a closure implementing an FIR high-pass filter
   for complex input/output, with real-valued coefficients.

   sample-rate : float
   cutoff      : float
   order       : integer (number of taps)."
  (let* ((coeffs (fir-highpass-coefficients-complex sample-rate cutoff :order order))
         ;; State buffer now holds COMPLEX samples
         (state  (make-array order :initial-element #C(0d0 0d0)))
         (pos    0))
    (lambda (sample)
      "Filter a single complex sample, returning a complex result."
      ;; Insert the newest complex sample in circular buffer
      (setf (aref state pos) sample)
      (incf pos)
      (when (>= pos order)
        (setf pos 0))
      ;; Convolution
      (let ((acc #C(0d0 0d0))
            (idx pos))
        (dotimes (i order)
          (decf idx)
          (when (< idx 0)
            (setf idx (1- order)))
          ;; Multiply the real coefficient by the complex sample,
          ;; then add to the complex accumulator
          (setf acc (complex-plus acc
                                  (complex-times (aref coeffs i)
                                                 (aref state idx)))))
        acc))))

;; TODO: move to math
;;; Helper complex arithmetic macros/functions for clarity (optional):
(defun complex-plus (z1 z2)
  (complex (+ (realpart z1) (realpart z2))
           (+ (imagpart z1) (imagpart z2))))

(defun complex-times (real-coeff z)
  "Multiply a REAL coefficient by a complex number Z."
  (complex (* real-coeff (realpart z))
           (* real-coeff (imagpart z))))

;; Helper functions again (or inline them):
(defun complex-minus (z1 z2)
  "z1 - z2."
  (complex (- (realpart z1) (realpart z2))
           (- (imagpart z1) (imagpart z2))))

;;;;;; IIR highpass real

;; standard 2nd‐order “RBJ cookbook” highpass design (sometimes referred to as a “Butterworth‐style” or “Q=1/√2” approach)


;;; 1) Coefficient generator for a 2nd-order RBJ "cookbook" highpass filter

@export
(defun iir-biquad-highpass-coeffs (sample-rate cutoff &key (q (/ 1.0 (sqrt 2.0))))
  "Compute 2nd-order IIR high-pass filter coefficients (RBJ cookbook).
   Returns (b0 b1 b2 a1 a2), already normalized by a0.
   Q defaults to 1/sqrt(2) for a Butterworth-like response."
  (let* ((omega (* 2.0 pi (/ cutoff sample-rate)))  ; 2 * pi * (fc/fs)
         (cosw (cos omega))
         (sinw (sin omega))
         (alpha (/ sinw (* 2.0 q)))

         ;; Biquad denominators
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cosw))
         (a2 (- 1.0 alpha))

         ;; Biquad numerators for high-pass
         (b0 (/ (+ 1.0 cosw) 2.0))
         (b1 (- (+ 1.0 cosw)))
         (b2 (/ (+ 1.0 cosw) 2.0)))

    ;; Normalize by a0
    (labels ((norm (x) (/ x a0)))
      (list (nth-value 0 (norm b0)) (norm b1) (norm b2)
            (norm a1) (norm a2)))))

;;; 2) Returns a closure implementing the 2nd-order highpass filter

@export
(defun make-iir-highpass-filter (sample-rate cutoff
                               &key (q (/ 1.0 (sqrt 2.0))))
  "Create a 2nd-order IIR highpass filter closure using the RBJ cookbook method.
   SAMPLE-RATE = sampling frequency in Hz
   CUTOFF = desired highpass cutoff in Hz
   ORDER = 2 (this argument is just for API similarity with the FIR version)
   Q = resonance / damping factor (default = 1/sqrt(2) => Butterworth)

   Returns a function that takes a single sample and returns the filtered sample."

  (multiple-value-bind (b0 b1 b2 a1 a2)
      (apply #'iir-biquad-highpass-coeffs sample-rate cutoff
             (list :q q))

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


    ;; State variables: x[n-1], x[n-2], y[n-1], y[n-2]
    (let ((x1 0.0)
          (x2 0.0)
          (y1 0.0)
          (y2 0.0))
      (lambda (x0)
        "Call with one input sample at a time; returns the highpassed output."
        (let* ((y0 (+ (* b0 x0)
                      (* b1 x1)
                      (* b2 x2)
                      (- (* a1 y1))
                      (- (* a2 y2)))))
          ;; Shift the states
          (setf x2 x1
                x1 x0
                y2 y1
                y1 y0)
          y0)))))


(defun iir-ex ()
  ;; Create a 2nd-order highpass filter with fc=1000 Hz, fs=48000 Hz, default Q.
  ;; Then process a small number of samples:

  (let ((hp (make-iir-highpass-filter 48000.0 1000.0)))
    (format t "Processing some samples:~%")
    (dolist (sample '(0.0 1.0 1.0 1.0 1.0 1.0))
      (format t "~&Input=~F  ->  Output=~F" sample (funcall hp sample)))))

;;;;;;;;; IIR highpass complex

@export
(defun iir-biquad-highpass-coeffs-complex (sample-rate cutoff
                                           &key (q (/ 1.0 (sqrt 2.0))))
  "Compute 2nd-order RBJ cookbook HIGH-PASS coefficients (real),
   suitable for filtering complex input.

   Returns (b0 b1 b2 a1 a2), normalized by a0."
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

    ;; Normalize by a0
    (labels ((norm (x) (/ x a0)))
      (list (norm b0) (norm b1) (norm b2)
            (norm a1) (norm a2)))))

@export
(defun make-iir-highpass-filter-complex (sample-rate cutoff
                                       &key (q (/ 1.0 (sqrt 2.0))))
  "Create a 2nd-order IIR high-pass filter for COMPLEX input.
   The underlying design is real-valued RBJ highpass with
   Q = 1/sqrt(2) by default for a Butterworth-like slope.

   Returns a closure that takes a single complex sample
   and returns a complex output."

  (multiple-value-bind (b0 b1 b2 a1 a2)
      (apply #'iir-biquad-highpass-coeffs-complex sample-rate cutoff
             (append '() (list :q q)))
    (let ((x1 #C(0d0 0d0))
          (x2 #C(0d0 0d0))
          (y1 #C(0d0 0d0))
          (y2 #C(0d0 0d0)))
      (lambda (x0)
        "Call this closure with a complex sample; returns a complex output."
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
          ;; Shift states
          (setf x2 x1
                x1 x0
                y2 y1
                y1 y0)
          y0)))))


(defun complex-both-ex ()

;;; Example usage for the FIR complex filter
  (let ((fir-hp-complex
          (make-fir-highpass-filter-complex
           48000.0 1000.0 :order 11)))
    (format t "FIR Complex filter test:~%")
    (dolist (in '(#C(1 0) #C(1 1) #C(0.5 -0.5) #C(-1 1) #C(0 0)))
      (format t "~&In=~A => Out=~A"
              in (funcall fir-hp-complex in))))

;;; Example usage for the IIR complex filter
  (let ((iir-hp-complex
          (make-iir-highpass-filter-complex
           48000.0 1000.0 :q 0.7071)))
    (format t "~%~%IIR Complex filter test:~%")
    (dolist (in '(#C(1 0) #C(1 1) #C(0.5 -0.5) #C(-1 1) #C(0 0)))
      (format t "~&In=~A => Out=~A"
              in (funcall iir-hp-complex in)))))
