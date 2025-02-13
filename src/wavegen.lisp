(in-package :cl-user)
(defpackage cl-radar.wavegen
  (:use :cl
   ;;:portaudio
   :cl-radar.util))
(in-package :cl-radar.wavegen)
(cl-syntax:use-syntax :annot)

;; TODO: qam / ofdma correlation wavegen and decode


(defconstant twopi (* 2 3.14159))

;; sine: if phi > 2*pi, -=
;; ramp: should we use 2*pi?

;; each gen func: carries current-phi and d-phi-per-sample

;; phase 0 to 2pi
(defun make-sinegen (freq-hz samples-sec &key (ampl 1.0)) ;; TODO: is this used?
  (let* ((samples-per-period (/ samples-sec (float freq-hz)))
         (phi-per-sample (* samples-per-period twopi))
         (phase (* -1 phi-per-sample))) ;; start one sample before 0

    (format t "--- ~a hz at ~a hz sample rate; ~a samples per cycle; ~a phi per sample~%"
            freq-hz samples-sec samples-per-period phi-per-sample)

    (lambda ()
      (incf phase phi-per-sample)
      (format t "-] phase now ~a~%" phase)
      (when (>= phase twopi)
        (decf phase twopi))
      (* ampl (sin phase)))))

;; ----

;; cover 0 - 2*pi in 0 - 44,100
;;   sin( t * (2 * pi * f)) for t :: 0-1?

;; TODO:
(defparameter +s-r+ 48000) ;; hz
(defparameter +f+ 100) ;; hz

;; these hold state for incremental gens (faster than thunks etc)
(defvar *amplitude* 0.5)
(defvar *phase* 0.0) ;; current posn from 0 to 2*pi

;; TODO: these both, and all instances of each, use and increment *phase*;
;;  should have them return a closure for the wrapper to call

@export
(defun sines-get-next (&optional (sample-rate +s-r+) (freq +f+) (amplitude *amplitude*))
  (let ((r (* amplitude (sin (* freq *phase*)))))
    (incf *phase* (* 2 pi (/ 1 sample-rate)))
    r))

;; ramp: a ramp 0.1s long is 0.1 * +s-r+ samples long
;;         and at ampl 0.1 goes from -0.05 to 0.05
@export
(defun ramp-get-next (&optional (sample-rate +s-r+) (freq +f+) (amplitude *amplitude*))
  ;; TODO: this sucks
  (let* ((ramp-period (* 8.2 (/ 1.0 freq))) ;; the 10.0 is fixup to match scope
        ;;(ramp-len-samples (* ramp-period sample-rate))
        (r (- (* *phase* (/ amplitude ramp-period)) (/ amplitude 2.0))))

    (incf *phase* (* 2 pi (/ 1 sample-rate)))
    (when (> r amplitude) ;; TODO: wtf
      (setf *phase* 0.0))
    r))

@export
(defun signals-fill-2ch-array (ar &key (left-sig-next-fn #'sines-get-next)
                                   (right-sig-next-fn #'sines-get-next)
                                   (left-ampl 1.0) (right-ampl 1.0))
  "call next-fn for each member of both chans of array
(2d portaudio split array, (2 1024) :float)"
  (assert (= 2 (length (array-dimensions ar))))

  (let ((last-trig-sample 0.1)
        (trig-start-i 0)
        (edges '()))
    (dotimes (i (array-dimension ar 1))
      (let ((trig-ch (coerce
                      (* left-ampl (funcall left-sig-next-fn))
                      'single-float)))
        (setf (aref ar 0 i)
              trig-ch)
        (when (and (> last-trig-sample 0.0) ;; falling edge of trig channel
                   (< trig-ch 0.0))

          (push (cons trig-start-i (- i 1)) edges) ;; add (start . end) to edgelist
          (setf trig-start-i i))
        (setf last-trig-sample trig-ch))

      (setf (aref ar 1 i)
            (coerce
             (* right-ampl (funcall right-sig-next-fn))
             'single-float)))
    (values
     ar
     (nreverse edges))))

@export
(defun sines-fill-2ch-array (ar)
  (assert (= 2 (length (array-dimensions ar))))

 ;; (format t "--- sines fill array dims: ~a~%" (array-dimensions ar))
  (loop for i from 0 below (array-dimension ar 1)
        do
           (let ((nextv (coerce (sines-get-next) 'single-float)))
             ;;(format t "next val is ~a, of type ~a.~%" nextv (type-of nextv))
             (setf (aref ar 0 i) ;; left
                   nextv)
             (setf (aref ar 1 i) ;; right
                   (* 0.1 nextv))))
  ar)

;; TODO: chans
@export
(defun sines-fill-portaudio-2ch-array (ar) ;; 1d, 2x len
  (assert (= 1 (length (array-dimensions ar))))

  (let ((n-samples (floor (/ (array-dimension ar 0) 2))))

    ;;(format t "--- sines fill pa array dims: ~a~%" n-samples)

    (loop for i from 0 below n-samples
          do
             (let ((nextv (coerce (sines-get-next) 'single-float)))
               ;;(format t "next val is ~a, of type ~a.~%" nextv (type-of nextv))
               ;;(setf (aref ar 0 i) ;; left
               ;;      nextv)
               ;;(setf (aref ar 1 i) ;; right
               ;;      (* 0.1 nextv))
               (setf (aref ar (* 2 i))
                     (* 1.5 nextv))
               (setf (aref ar (+ (* 2 i) 1))
                     (* 0.3 nextv)))))
  ar)



;;;;;;;
;; generate a single random deviate from a normal distribution
;;   with the given mean and std dev using Box-Muller."
@export
(defun random-gaussian (&optional (mean 0d0) (std 1d0))

  (multiple-value-bind (u1 u2)
      (values (random 1.0d0) (random 1.0d0))
    (let* ((r (sqrt (* -2d0 (log (max u1 1.0d-30)))))  ; avoid log(0)
           (theta (* 2d0 pi u2)))
      (+ mean (* std r (cos theta))))))

(defconstant +c+ 2.998d8)

@export
(defun generate-fmcw-if-samples
    (targets
     &key
       (center-frequency 1.05d10)  ; 10.5 ghz default
       (bandwidth       1.0d9)     ; 1 ghz default
       (sample-frequency 4.8d4)    ; 48 hkz default
       (sweep-time      0.016d0)   ; 16 ms default
       (element-spacing 0.01d0)    ; spacing between array elements in meters
       (num-elements    8)         ; how many elements in the linear array
       (noise-std       0.0d0))    ; noise standard deviation (default none)

  (let* (;; sweep slope (hz/s)
         (sweep-frequency (/ bandwidth sweep-time))
         ;; duration of one chirp (sweep)
         (chirp-time sweep-time)
         ;; number of time samples for this single chirp
         (num-samples (round (* chirp-time sample-frequency)))
         ;; 2d result array: row=element, col=sample
         (if-array (make-array (list num-elements num-samples)
                               :initial-element #c(0d0 0d0))))

    ;; for each sample
    (dotimes (n num-samples)
      (let ((tm (/ n sample-frequency)))  ; time in seconds
        ;; for each antenna element
        (dotimes (elem num-elements)
          (let ((sum-of-targets #c(0d0 0d0)))
            ;; sum the contribution of each target
            (dolist (target targets)
              (destructuring-bind (range-m angle-deg amplitude) target
                (let* ((theta-rad (* angle-deg (/ pi 180.0d0)))
                       ;; mixed down freq for this target
                       (f-b (* sweep-frequency (/ (* 2d0 range-m) +c+)))
                       ;; phase offset for angle at this element
                       (element-phase-offset
                         (* 2d0 pi (/ center-frequency +c+)
                            elem element-spacing (sin theta-rad)))
                       ;; instantaneous phase for time tm
                       (inst-phase (+ (* 2d0 pi f-b tm)
                                      element-phase-offset)))
                  (incf sum-of-targets
                        (complex
                         (* amplitude (cos inst-phase))
                         (* amplitude (sin inst-phase)))))))
            ;; add noise to the final sum for this sample
            ;; each channel gets its own random noise in-phase & quadrature
            (incf sum-of-targets
                  (complex
                   (random-gaussian 0d0 noise-std)
                   (random-gaussian 0d0 noise-std)))

            (setf (aref if-array elem n) sum-of-targets)))))
    if-array))

@export
(defun generate-fmcw-if-samples-nonoise
    (targets
     &key
       (center-frequency 1.05d10)
       (bandwidth       1.0d9)
       (sample-frequency 4.8d4)
       (sweep-time      0.016d0)
       (element-spacing 0.01d0)
       (num-elements    8))
  (let* ((sweep-frequency (/ bandwidth sweep-time))
         (chirp-time sweep-time)
         (num-samples (round (* chirp-time sample-frequency)))
         (if-array (make-array (list num-elements num-samples)
                               :initial-element #c(0d0 0d0))))
    (dotimes (n num-samples)
      (let ((tm (/ n sample-frequency)))
        (dotimes (elem num-elements)
          (let ((sum-of-targets #c(0d0 0d0)))
            (dolist (target targets)
              (destructuring-bind (range-m angle-deg amplitude) target
                (let* ((theta-rad (* angle-deg (/ PI 180.0d0)))
                       (f-b (* sweep-frequency
                               (/ (* 2d0 range-m) +c+)))
                       (element-phase-offset
                         (* 2d0 pi (/ center-frequency +c+)
                            elem element-spacing (sin theta-rad)))
                       (inst-phase (+ (* 2d0 pi f-b tm)
                                      element-phase-offset)))
                  (incf sum-of-targets
                        (complex
                         (* amplitude (cos inst-phase))
                         (* amplitude (sin inst-phase)))))))
            (setf (aref if-array elem n) sum-of-targets)))))
    if-array))


;;;;;;;

@export
(defun sine-into! (ar freq-hz &key (sample-rate 24000) (magnitude 1.0) (offset 0.0))
  (let ((tx 0.0))
    (dotimes (i (length ar))
      (setf (aref ar i)
            (+ offset
               (* magnitude
                  (sin
                   (*
                    freq-hz
                    (incf tx (/ 1 sample-rate))
                    2 Pi))))))
    ar))

@export
(defun triangle-into! (ar freq-hz &key (sample-rate 24000) (magnitude 1.0)
                                    (offset 0.0) debug-p)
  (let* ((period-in-samples (/ (float sample-rate) freq-hz))
         (half-period (/ period-in-samples 2.0))
         (pis '()))

    (when debug-p
      (format t "period in samples is ~a.~%" period-in-samples))

    (dotimes (i (length ar))
      (let* ((i+offset (+ i offset))
             (phase-in-samples (mod i+offset period-in-samples)))
        (when debug-p (push phase-in-samples pis))
        (setf (aref ar i)
              (* magnitude ;; peak to peak
                 (+ 0.5 ;; center on on [-0.5, 0.5]
                    (if (< phase-in-samples half-period)
                        (/ (- phase-in-samples half-period) half-period)
                        (/ (- half-period phase-in-samples) half-period)))))))
    (when debug-p (format t "phase in samples: ~a~%" (nreverse pis)))
    ar))
