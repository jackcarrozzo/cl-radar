(in-package :cl-user)
(defpackage cl-radar.mod
  (:use :cl))
(in-package :cl-radar.mod)
(cl-syntax:use-syntax :annot)

;; modulation and demodulation stuff


;; create closure for n-ASK mod with continuous phase
@export
(defun make-ask-modulator (sample-rate n tone-distance
                           offset symbol-length
                           &optional (amplitude 1.0))
  (let* ((two-pi (* 2d0 (acos -1.0)))
         ;; tone frequencies around offset
         (freqs (loop for i from 0 below n
                      collect (+ offset
                                 (* tone-distance
                                    (- i (/ (1- n) 2.0))))))
         ;; precompute phase increments for each tone
         (phase-incr (mapcar (lambda (f) (/ (* two-pi f) sample-rate))
                             freqs))
         ;; running phases for each tone
         (phases (make-array n :initial-element 0.0d0)))
    (lambda (bits)
      ;;(format t "-- phases: ~a~%" phases)
      ;;(format t "-- bits: ~a~%" bits)

      ;; bits should be multiple of n
      (unless (zerop (mod (length bits) n))
        (error "bit vector length must be multiple of n (~d)" n))

      (let* ((num-symbols (/ (length bits) n))
             (total-samples (* num-symbols symbol-length))
             (result (make-array total-samples :initial-element #c(0.0d0 0.0d0))))
        (dotimes (sym num-symbols)
          (let ((start (* sym n))
                (out-offset (* sym symbol-length)))
            (dotimes (s symbol-length)
              (let ((sum 0d0))
                (dotimes (tone n)
                  (let ((bit (elt bits (+ start tone))))
                    ;;(format t "---- bit ~a: ~a~%" (+ start tone) bit)
                    (when (not (zerop bit))
                      (incf sum (complex (* amplitude bit (cos (aref phases tone)))
                                         (* amplitude bit (sin (aref phases tone))))))))
                (setf (aref result (+ out-offset s)) sum))
              (dotimes (tone n)
                (setf (aref phases tone)
                      (mod (+ (aref phases tone) (nth tone phase-incr))
                           two-pi))))))
        result))))

(defun ask-t2 ()
  (let ((mod (make-ask-modulator 10000     ; sample rate
                                 4         ; 4 tones
                                 1000.0    ; spacing in hz
                                 0.0       ; offset
                                 4)))    ; samples per symbol
    (let ((result1 (funcall mod #(0 1 0 0))))
      (format t "~&first call samples: ~a~%" result1))
    (let ((result2 (funcall mod #(0 1 0 0 0 1 0 0))))
      (format t "~&second call samples: ~a~%" result2))))

;; (cl-radar.mod::graph-n-ask :gold-code-p t :samples 128)
@export
(defun graph-n-ask (&key (n 8) (samples 4096) (sample-rate 10000) (gold-code-p nil))
  (let* ((ask-mod (make-ask-modulator sample-rate
                                      n
                                      1000.0
                                      0.0
                                      samples))
         (syms-input (if (not gold-code-p)
                         (make-array n :initial-element 1)
                         (let ((gcf (cl-radar.corr:make-gold-code-generator :offset 7)))
                           (funcall gcf 64))))
         (iq-data (funcall ask-mod syms-input))
         (fft-data (bordeaux-fft:windowed-fft iq-data (/ samples 2) samples))
         (fft-mags (cl-radar.math:complex-mags fft-data))
         (x-axis (make-array (length fft-mags) :initial-element 0.0))
         (half-sr (/ sample-rate 2)))
    (format t "-- syms: ~a~%" syms-input)
    (format t "-- iq data is ~a long.~%" (length iq-data))

    (dotimes (i (length x-axis))
      (setf (aref x-axis i)
            (+ (* -1 half-sr) (* sample-rate (/ i (length x-axis))))))

    (format t "-- 0 hz center at index ~a.~%" (/ (length fft-mags) 2))
    (vgplot:plot x-axis (cl-radar.math:fft-swap fft-mags))))
