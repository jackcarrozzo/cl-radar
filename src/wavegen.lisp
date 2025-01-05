(in-package :cl-user)
(defpackage cl-radar.wavegen
  (:use :cl
   ;;:portaudio
   :cl-radar.util))
(in-package :cl-radar.wavegen)
(cl-syntax:use-syntax :annot)


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

;; TODO: these both, and all instances of each, use and increment *phase*

@export
(defun sines-get-next (&optional (sample-rate +s-r+) (freq +f+))
  (let ((r (* *amplitude* (sin (* freq *phase*)))))
    (incf *phase* (* 2 pi (/ 1 sample-rate)))
    r))

;; ramp: a ramp 0.1s long is 0.1 * +s-r+ samples long
;;         and at ampl 0.1 goes from -0.05 to 0.05
@export
(defun ramp-get-next (&optional (sample-rate +s-r+) (freq +f+))
  ;; TODO: this sucks
  (let* ((ramp-period (* 8.2 (/ 1.0 freq))) ;; the 10.0 is fixup to match scope
        ;;(ramp-len-samples (* ramp-period sample-rate))
        (r (- (* *phase* (/ *amplitude* ramp-period)) (/ *amplitude* 2.0))))

    (incf *phase* (* 2 pi (/ 1 sample-rate)))
    (when (> r *amplitude*) ;; TODO: wtf
      (setf *phase* 0.0))
    r))

@export
(defun signals-fill-2ch-array (ar &key (left-sig-next-fn #'sines-get-next)
                                   (right-sig-next-fn #'sines-get-next)
                                   (left-ampl 1.0) (right-ampl 1.0))
  "call next-fn for each member of both chans of array
(2d portaudio split array, (2 1024) :float)"
  (assert (= 2 (length (array-dimensions ar))))

  (dotimes (i (array-dimension ar 1))
    (setf (aref ar 0 i)
          (coerce
           (* left-ampl (apply left-sig-next-fn nil))
           'single-float))
    (setf (aref ar 1 i)
          (coerce
           (* right-ampl (apply right-sig-next-fn nil))
           'single-float)))
  ar)

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
