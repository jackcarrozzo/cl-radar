(in-package :cl-user)
(defpackage cl-radar.mod
  (:use :cl))
(in-package :cl-radar.mod)
(cl-syntax:use-syntax :annot)

;; modulation and demodulation stuff

(defconstant +two-pi+ (* 2.0 Pi))

@export
(defun make-ask-modulator (sample-rate n tone-distance
                           offset symbol-length
                           &optional (amplitude 1.0))
  (let* ((freqs (loop for i from 0 below n
                      collect (+ offset
                                 (* tone-distance
                                    (- i (/ (1- n) 2.0))))))
         ;; precompute phase increments for each tone
         (phase-incr (mapcar (lambda (f) (/ (* +two-pi+ f) sample-rate))
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
                           +two-pi+))))))
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


;; create closure for n-ASK demod
@export
(defun make-ask-demodulator (n &key (center-frequency 0.0d0)
                                (tone-spacing 100.0d0)
                                (sample-rate 10000.0d0)
                                (symbol-rate 100.0d0)
                                (threshold 0.1d0)
                                (positive-frequency t))
  (let ((samples-per-symbol (round (/ sample-rate symbol-rate))))
    (lambda (iq-samples)
      (let* ((num-symbols (floor (/ (length iq-samples)
                                    samples-per-symbol)))
             (bits (make-array (* n (floor (/ (length iq-samples)
                                              samples-per-symbol)))
                               :initial-element 0))
             (rawbits (make-array (length bits) :initial-element 0.0d0)))
        (dotimes (sym num-symbols)
          (let ((offset (* sym samples-per-symbol)))
            (dotimes (bit n)
              (let ((base-index (- bit (/ (1- n) 2d0)))
                    (acc 0d0))
                ;; TODO: fix this wacky shit
                (let ((freq (if positive-frequency
                                (+ center-frequency (* base-index tone-spacing))
                                (- center-frequency (* base-index tone-spacing)))))
                  (dotimes (j samples-per-symbol)
                    (let* ((idx (+ offset j))
                           (tm (/ idx sample-rate))
                           (phase (* +two-pi+ freq tm))
                           (sample (aref iq-samples idx))
                           (mix-real (cos phase))
                           (mix-imag (- (sin phase)))
                           (s-real (realpart sample))
                           (s-imag (imagpart sample))
                           (mixed-real (- (* s-real mix-real)
                                          (* s-imag mix-imag)))
                           (mixed-imag (+ (* s-real mix-imag)
                                          (* s-imag mix-real)))
                           (mag (sqrt (+ (* mixed-real mixed-real)
                                         (* mixed-imag mixed-imag)))))
                      (incf acc mag))))
                ;;(setf (aref bits (+ (* sym n) bit))
                ;;      (if (> (/ acc samples-per-symbol) threshold) 1 0))
                (let ((idx (+ (* sym n) bit)))
                  (setf (aref bits idx)
                        (if (> (setf (aref rawbits idx)
                                     (/ acc samples-per-symbol))
                               threshold)
                            1 0)))))))
        (values
         bits
         rawbits)))))

@export
(defun n-ask-demod-tester (&key (n 8) (samples 4096) (sample-rate 10000) (threshold 1.1d0))
  (let* ((ask-mod (make-ask-modulator sample-rate
                                      n
                                      5000.0 ; spacing
                                      0.0    ; offset
                                      samples)) ; samples per symbol
         (syms-input (let ((gcf (cl-radar.corr:make-gold-code-generator :offset 7)))
                       (funcall gcf 64)))
         (iq-data (funcall ask-mod syms-input))
         ;;(fft-data (bordeaux-fft:windowed-fft iq-data (/ samples 2) samples))
         ;;(fft-mags (cl-radar.math:complex-mags fft-data))
         ;;(x-axis (make-array (length fft-mags) :initial-element 0.0))
         ;;(half-sr (/ sample-rate 2))
         )
    (format t "-- syms: ~a~%" syms-input)
    (format t "-- iq data is ~a long.~%" (length iq-data))

    (let ((ask-demod (make-ask-demodulator n :center-frequency 0.0
                                             :tone-spacing 5000
                                             :sample-rate 10000
                                             :symbol-rate (float (/ 10000 samples))
                                             :threshold threshold)))
      (multiple-value-bind (output-bits output-rawbits)
          (funcall ask-demod iq-data)
        (format t "-- demod bits: ~a~%" output-bits)
        (assert (= (length output-bits) (length syms-input)))
        (let* ((bit-errs
                 (loop for i from 0 below (length output-bits) ;; 1 for err, 0 for ok
                       collecting
                       (if (= 1 (+ (nth i syms-input) (aref output-bits i))) ;; xor
                           1 0)))
               (errs-total (reduce #'+ bit-errs)))
          (format t "-- errs at : ~a~%" bit-errs)
          (format t "-- (~a bit errors, ~a bits, ~a % BER)~%"
                  errs-total (length output-bits) (/ (* 100.0 errs-total) (length output-bits)))
          (format t "-- raw bits: ~a~%" output-rawbits))))))


;; compute a 2d array of signal energy across n tones at each sample
(defun filterbank-energies (iq-samples n
                            &key (center-frequency 0.0d0)
                              (tone-spacing 100.0d0)
                              (sample-rate 10000.0d0)
                              (window-size 16)
                              (positive-frequency t))
  ;; output array: (# of samples) x n
  (let* ((len (length iq-samples))
         (result (make-array (list len n) :initial-element 0.0d0)))
    (dotimes (i len)
      (dotimes (tone n)
        (let* ((freq-offset (if positive-frequency ; wtf
                                (+ center-frequency
                                   (* (- tone (/ (1- n) 2.0d0))
                                      tone-spacing))
                                (- center-frequency
                                   (* (- tone (/ (1- n) 2.0d0))
                                      tone-spacing))))
               (start (max 0 (- i (1- window-size))))
               (acc-real 0.0d0)
               (acc-imag 0.0d0))
          (dotimes (j (1+ (- i start)))
            (let* ((idx (+ start j))
                   (tm (/ idx sample-rate))
                   (phase (* two-pi freq-offset tm))
                   (c (cos phase))
                   (s (sin phase))
                   (sample (aref iq-samples idx))
                   (sr (realpart sample))
                   (si (imagpart sample))
                   ;; multiply sample by conj(e^(j*phase)) -> e^(-j*phase)
                   (mix-r (+ (* sr c) (* si s)))
                   (mix-i (- (* si c) (* sr s))))

              (incf acc-real mix-r)
              (incf acc-imag mix-i)))
          (setf (aref result i tone)
                (sqrt (+ (* acc-real acc-real)
                         (* acc-imag acc-imag)))))))
    result))

;; returns a real array of tone energy vs time of same len as input
;; multiplying by
;;   𝑒^−𝑗(2𝜋𝑓𝑡)
;; and summing, the resulting complex value has a different angle in the complex plane
;;   if the input tone has a phase offset; taking the magnitude of that sum erases the
;;   phase difference such that this method gives the same amplitude regardless of phase
(defun single-tone-energy (iq-samples &key (frequency 0.0d0)
                                     (sample-rate 10000.0d0)
                                     (window-size 16))
  (let* ((len (length iq-samples))
         (result (make-array len :initial-element 0.0d0))
         (two-pi (* 2d0 pi)))
    (dotimes (i len)
      (let ((start (max 0 (- i (1- window-size))))
            (acc-real 0.0d0)
            (acc-imag 0.0d0))
        (dotimes (j (1+ (- i start)))
          (let* ((idx (+ start j))
                 (tm (/ idx sample-rate))
                 (phase (* two-pi frequency tm))
                 (c (cos phase))
                 (s (sin phase))
                 (sample (aref iq-samples idx))
                 (sr (realpart sample))
                 (si (imagpart sample))
                 (mix-r (+ (* sr c) (* si s)))
                 (mix-i (- (* si c) (* sr s))))
            (incf acc-real mix-r)
            (incf acc-imag mix-i)))
        (setf (aref result i)
              (sqrt (+ (* acc-real acc-real)
                       (* acc-imag acc-imag))))))
    result))

;; TODO: confirm this is right
@export
(defun iq-shift-freq (iq-samples offset-hz sample-rate)
  ;; multiply each sample by e^(j*2*pi*offset*t)
  (let* ((len (length iq-samples))
         (result (make-array len :initial-element #c(0.0d0 0.0d0)))
         (two-pi (* 2d0 pi)))
    (dotimes (i len)
      (let* ((tm (/ i sample-rate))
             (phase (* two-pi offset-hz tm))
             (c (cos phase))
             (s (sin phase))
             (sample (aref iq-samples i))
             (sr (realpart sample))
             (si (imagpart sample))
             (mix-r (- (* sr c) (* si s)))
             (mix-i (+ (* sr s) (* si c))))
        (setf (aref result i) (complex mix-r mix-i))))
    result))


;; convert an integer n (0 <= n < qam-level) to a (i q) coordinate
;; in a rectangular qam constellation of dimension sqrt(qam-level) x sqrt(qam-level).
(defun qam-constellation-point (n sqrt-level)
  ;; for a standard rectangular qam, we can interpret n in row-column form.
  ;; for example, if sqrt-level=8, n=0 maps to (-7 -7), n=1 maps to (-5 -7), etc.
  (let* ((row (floor n sqrt-level))
         (col (mod n sqrt-level)))
    ;; map row and col to i+q in range -sqrt-level+1, ..., +sqrt-level-1
    ;; shift and scale so i and q roughly center around 0.0 (TODO:)
    ;;(offset (ash sqrt-level -1)) ;; sqrt-level/2, seems rude
    (complex
     (- (* 2 col) (1- sqrt-level))
     (- (* 2 row) (1- sqrt-level)))))

;; create a closure that, given data, outputs a single buffer of complex qam samples
@export
(defun make-qam-modulator (&key (qam-level 64)
                                (symbol-rate 1000.0)
                                (sample-rate 48000.0)
                                (buffer-length 1024))
  ;; bits per symbol = log2(qam-level)
  (let* ((bits-per-symbol (round (log qam-level 2)))
         (sqrt-level (round (sqrt qam-level))))
    (lambda (data)
      ;; data is an array of bytes (or bits) that we want to transmit
      ;; we'll convert data bits -> qam symbol -> repeated samples
      ;; (simple rectangular replication; no pulse shaping)
      (let ((out (make-array buffer-length :initial-element #c(0.0 0.0))))
        (let* ((samples-per-symbol (truncate (/ sample-rate symbol-rate)))
               (data-bit-index 0)
               (out-index 0)
               (total-bits (* (length data) 8)))
          (loop while (< out-index buffer-length)
                do
                  ;; gather bits-per-symbol bits from data
                  (when (>= data-bit-index total-bits)
                    ;; if we run out of data, we'll just break or pad with zero
                    (return))
                  (let ((symbol-bits 0))
                    (dotimes (b bits-per-symbol)
                      (let* ((byte-index (truncate (/ data-bit-index 8)))
                             (bit-in-byte (mod data-bit-index 8))
                             (this-byte (aref data byte-index))
                             (bit-val (ldb (byte 1 bit-in-byte) this-byte)))
                        (setf symbol-bits (logior symbol-bits (ash bit-val b)))
                        (incf data-bit-index)))
                    ;; map symbol-bits to constellation
                    (let ((sym (qam-constellation-point symbol-bits sqrt-level)))
                      (dotimes (s samples-per-symbol)
                        (when (>= out-index buffer-length)
                          (return))
                        (setf (aref out out-index) sym)
                        (incf out-index))))))
        out))))

@export
(defun make-qam-modulator-with-offset (&key (qam-level 64)
                                           (symbol-rate 1000.0)
                                           (sample-rate 48000.0)
                                           (buffer-length 1024)
                                           (freq-offset 0.0))
  (let* ((bits-per-symbol (round (log qam-level 2)))
         (sqrt-level (round (sqrt qam-level)))
         (samples-per-symbol (truncate (/ sample-rate symbol-rate))))
    (lambda (data)
      (let ((out (make-array buffer-length :initial-element #c(0.0 0.0)))
            (phase 0.0d0)
            (phase-inc (/ (* +two-pi+ freq-offset) sample-rate)))
        (let ((data-bit-index 0)
              (total-bits (* (length data) 8))
              (out-index 0))
          (loop while (< out-index buffer-length)
                do
                   ;; gather bits-per-symbol bits from data
                   (when (>= data-bit-index total-bits)
                     ;; if we run out of data, we stop or could pad with zeros
                     (return))
                   (let ((symbol-bits 0))
                     (dotimes (b bits-per-symbol)
                       (let* ((byte-index (truncate (/ data-bit-index 8)))
                              (bit-in-byte (mod data-bit-index 8))
                              (this-byte (aref data byte-index))
                              (bit-val (ldb (byte 1 bit-in-byte) this-byte)))
                         (setf symbol-bits (logior symbol-bits (ash bit-val b)))
                         (incf data-bit-index)))
                     ;; map to qam constellation
                     (let ((sym (qam-constellation-point symbol-bits sqrt-level)))
                       (dotimes (s samples-per-symbol)
                         (when (>= out-index buffer-length)
                           (return))
                         ;; rotate by freq-offset to shift spectrum
                         ;; multiply 'sym' by e^(j * phase)
                         (let ((rot-real (cos phase))
                               (rot-imag (sin phase))
                               (sym-i (realpart sym))
                               (sym-q (imagpart sym)))
                           (setf (aref out out-index)
                                 (complex
                                  (- (* sym-i rot-real) (* sym-q rot-imag))
                                  (+ (* sym-i rot-imag) (* sym-q rot-real)))))
                         (incf out-index)
                         (incf phase phase-inc))
                       ;; optionally wrap phase to keep it in [-pi, +pi], not strictly necessary
                       (when (> phase +two-pi+)
                         (decf phase +two-pi+))))))
        out))))

;; binary fsk
@export
(defun make-fsk-modulator (&key (tone-distance 1000.0d0)
                             (sample-rate 48000.0d0)
                             (samples-per-symbol 100)
                             (center-frequency 0.0d0))
  (let* ((twopi 6.283185307179586d0)
         (freq0 (- center-frequency (/ tone-distance 2d0)))
         (freq1 (+ center-frequency (/ tone-distance 2d0)))
         (phase0 0.0d0)
         (phase1 0.0d0)
         ;; per-sample phase increment
         (delta0 (/ (* twopi freq0) sample-rate))
         (delta1 (/ (* twopi freq1) sample-rate)))
    (lambda (bits)
      (let* ((length (length bits))
             (n (* length samples-per-symbol))
             (result (make-array n :initial-element #c(0.0d0 0.0d0))))
        (loop for i from 0 below length
              for b = (aref bits i) do
                (loop for s from 0 below samples-per-symbol
                      for idx = (+ s (* i samples-per-symbol)) do
                        (incf phase0 delta0)
                        (incf phase1 delta1)
                        (setf (aref result idx)
                              (if (zerop b)
                                  (complex (cos phase0) (sin phase0))
                                  (complex (cos phase1) (sin phase1))))))
        result))))


;; cw on/off keying
@export
(defun make-ask-modulator (sample-rate symbol-rate freq &optional (low-level-db -20.0))
  (let* ((samples-per-symbol (floor (/ sample-rate symbol-rate)))
         ;; (pi (acos -1.0d0)) ;; erm.......
         (low-lin (if low-level-db
                      (exp (* (/ low-level-db 20d0) (log 10d0)))
                      0d0)))
    (lambda (symbols)
      (let* ((num-samples (* samples-per-symbol (length symbols)))
             (out (make-array num-samples :initial-element #c(0.0d0 0.0d0))))
        (loop for i from 0 below (length symbols) do
          (let ((amplitude (if (eql (aref symbols i) 1)
                               1d0
                               low-lin)))
            (loop for s from 0 below samples-per-symbol do
              (let* ((tm (/ (+ s (* i samples-per-symbol)) sample-rate))
                     (phase (* 2d0 Pi freq tm))
                     (re (* amplitude (cos phase)))
                     (im (* amplitude (sin phase))))
                (setf (aref out (+ s (* i samples-per-symbol))) (complex re im))))))
        out))))

@export
(defun graph-ask-waves (&key (sample-rate 48000) (num-symbols 8))
  (declare (ignore num-symbols))
  (let* ((ask-mod (make-ask-modulator sample-rate 6000 3000 -20.0))
         (input-syms #(1 0 1 1 0 0 0 1))
         (bb-samples (funcall ask-mod input-syms))
         (bb-reals (cl-radar.math:array-mapcar #'realpart bb-samples))
         (bb-imags (cl-radar.math:array-mapcar #'imagpart bb-samples))
         (sec-per-sample (/ 1.0 sample-rate))
         (x-axis (loop for x from 0.0 below
                                      (* sec-per-sample (length bb-samples))
                       by sec-per-sample
                       collecting x)))
    (vgplot:plot x-axis bb-reals "real" x-axis bb-imags "imag")))

;; pretty
@export
(defun graph-binary-ask-gold-code (&key (sample-rate 10e6) (cfreq -1e6) (symbol-rate 100e3))
  (let* ((ask-mod (make-ask-modulator sample-rate symbol-rate cfreq -20.0))
         (input-syms #(1 0 1 1 0 0 0 1))
         (bb-samples (funcall ask-mod input-syms))
         (bb-reals (cl-radar.math:array-mapcar #'realpart bb-samples))
         (bb-imags (cl-radar.math:array-mapcar #'imagpart bb-samples))
         (sec-per-sample (/ 1.0 sample-rate))
         (x-axis (loop for x from 0.0 below
                                      (* sec-per-sample (length bb-samples))
                       by sec-per-sample
                       collecting x)))
    (vgplot:plot x-axis bb-reals "real" x-axis bb-imags "imag")))

;; very pretty
@export
(defun graph-ask-fft (&key (sample-rate 48000) (symbol-rate 3000) (num-symbols 64) (cfreq -5000))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((samp-per-sym (/ (float sample-rate) symbol-rate))
           (num-samples (* num-symbols samp-per-sym))
           (ask-mod (make-ask-modulator sample-rate symbol-rate cfreq -20.0))
           (gold-code (make-array num-symbols :initial-contents
                                  (funcall gold-gen num-symbols)))
           (bb-samples (funcall ask-mod gold-code))
           (bb-fft (bordeaux-fft:windowed-fft bb-samples (/ (length bb-samples) 2)
                                              (length bb-samples)))
           (fft-mags (cl-radar.math:fft-swap
                      (cl-radar.math:complex-mags bb-fft))))
      (format t "-- ~a symbols, ~a samples.~%" num-symbols num-samples)
      (format t "----: ~a~%" (subseq bb-samples 0 6))
      (format t "-- fft got back ~a~%" (length bb-fft))
      (format t "---: ~a~%" (subseq bb-fft 0 6))

      (let* ((fft-len (length bb-fft))
             (f-start (* -1 (/ sample-rate 2.0)))
             (f-end (/ sample-rate 2.0))
             (d-f (/ sample-rate fft-len)))
        (vgplot:plot
         (loop for x from f-start to f-end by d-f
               collecting x)
         fft-mags)))))
;; TODO: figure out why this is not just the left side ^
;;  answer: windowed-fft doesnt like our complex float array input and is just using reals
;;    see below for better one

;; pretty graphs
@export
(defun graph-ask-fft-split (&key (sample-rate 48000) (symbol-rate 3000)
                              (num-symbols 64) (cfreq -5000) (all-ones-p nil))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((samp-per-sym (/ (float sample-rate) symbol-rate))
           (num-samples (* num-symbols samp-per-sym))
           (ask-mod (make-ask-modulator sample-rate symbol-rate cfreq -20.0))
           (gold-code (make-array num-symbols :initial-contents
                                  (funcall gold-gen num-symbols)))
           (bb-samples (funcall ask-mod (if all-ones-p
                                            (make-array num-symbols :initial-element 1)
                                            gold-code)))
           (bb-csb (cl-radar.math::convert-to-padded-csarray bb-samples))
           ;;(prev-pow-2 (expt 2 (1- (ceiling (/ (float (log (length bb-samples))) (float (log 2)))))))
           ;;(bb-fft (bordeaux-fft:windowed-fft bb-samples (/ prev-pow-2 2)
           ;;                                   prev-pow-2))
           (bb-fft (bordeaux-fft:fft bb-csb))
           (fft-swapped (cl-radar.math:fft-swap bb-fft))
           (fft-mags (cl-radar.math:complex-mags fft-swapped))
           (fft-reals (cl-radar.math:array-mapcar #'realpart fft-swapped))
           (fft-imags (cl-radar.math:array-mapcar #'imagpart fft-swapped)))
      (format t "-- ~a symbols, ~a samples.~%" num-symbols num-samples)
      (format t "----: ~a~%" (subseq bb-samples 0 6))
      (format t "-- fft got back ~a~%" (length bb-fft))
      (format t "---: ~a~%" (subseq bb-fft 0 6))

      (let* ((fft-len (length bb-fft))
             (f-start (* -1 (/ sample-rate 2.0)))
             (f-end (/ sample-rate 2.0))
             (d-f (/ sample-rate fft-len))
             (x-axis (loop for x from f-start to f-end by d-f
                           collecting x)))
        (vgplot:plot
         x-axis
         fft-reals
         "real"
         x-axis
         fft-imags
         "imag"
         x-axis
         fft-mags
         "mags")))))

;; (cl-radar.mod::graph-ask-fft-split :symbol-rate 8000)
;; (cl-radar.mod::graph-ask-fft-split :symbol-rate 8000 :all-ones-p t)
;; (cl-radar.mod::graph-ask-fft-split :symbol-rate 3200)


@export
(defun make-bpsk-modulator (sample-rate symbol-rate freq-offset)
  (let ((sps (round (/ sample-rate symbol-rate)))
        (omega (/ (* 2 pi freq-offset) sample-rate)))
    (lambda (symbols)
      (let ((output (make-array (* (length symbols) sps)
                                :initial-element #c(0.0d0 0.0d0))))
        (loop for i from 0 below (length symbols) do
          (let ((phase-offset (if (zerop (aref symbols i))
                                  0d0
                                  pi)))
            (loop for j from 0 below sps do
              (let* ((index (+ (* i sps) j))
                     (phase (+ (* index omega) phase-offset)))
                (setf (aref output index)
                      (complex (cos phase) (sin phase)))))))
        output))))

@export
(defun graph-bpsk-signal ()
  (let* ((bpsk-mod (make-bpsk-modulator 48000 350 600))
         (in-syms #(1 0 0 1))
         (bb-samples (funcall bpsk-mod in-syms)))
    (cl-radar.math::graph-complex-ar bb-samples)))

(defvar *last-complex-bb-samples* nil)

(defun graph-bpsk-fft (&key (sample-rate 48000) (symbol-rate 3000)
                         (num-symbols 64) (cfreq -5000) (all-ones-p nil)
                         (plot-all-p nil))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((samp-per-sym (/ (float sample-rate) symbol-rate))
           (num-samples (* num-symbols samp-per-sym))
           (bpsk-mod (make-bpsk-modulator sample-rate symbol-rate cfreq))
           (gold-code (make-array num-symbols :initial-contents
                                  (funcall gold-gen num-symbols)))
           (bb-samples (funcall bpsk-mod (if all-ones-p
                                             (make-array num-symbols :initial-element 1)
                                             gold-code)))
           (bb-csb (cl-radar.math::convert-to-padded-csarray bb-samples))
           (bb-fft (bordeaux-fft:fft bb-csb))
           (fft-swapped (cl-radar.math:fft-swap bb-fft))
           (fft-mags (cl-radar.math:complex-mags fft-swapped))
           (fft-reals (cl-radar.math:array-mapcar #'realpart fft-swapped))
           (fft-imags (cl-radar.math:array-mapcar #'imagpart fft-swapped)))
      (format t "-- ~a symbols, ~a samples.~%" num-symbols num-samples)
      (setf *last-complex-bb-samples* bb-samples)
      (format t "---- bb-samples ~a~%" (subseq bb-samples 0 16))

      (let* ((fft-len (length bb-fft))
             (f-start (* -1 (/ sample-rate 2.0)))
             (f-end (/ sample-rate 2.0))
             (d-f (/ sample-rate fft-len))
             (x-axis (loop for x from f-start to f-end by d-f
                           collecting x)))
        (if plot-all-p
            (vgplot:plot
             x-axis
             fft-reals
             "real"
             x-axis
             fft-imags
             "imag"
             x-axis
             fft-mags
             "mags")
            (vgplot:plot x-axis fft-mags "mags"))))))

;; TODO: put these somewhere

;; scale -1.0 to 1.0 over 0 to 65535 inclusive
(defun scale-to-ushort (x)
  (let* ((val (round (* 65535 (/ (+ x 1.0d0) 2.0d0)))))
    (min (max val 0) 65535)))

(defun scale-to-sshort (x)
  (let* ((val (round (* 65535 (/ (+ x 1.0d0) 2.0d0)))))
    (- (min (max val 0) 65535)
       32768)))

;; real then imag, MSB first (big endian)
(defun write-complex-array-to-binary-file (fpath complex-data)
  (with-open-file (stream fpath
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (loop
      for c across complex-data do
        (let ((r (scale-to-ushort (realpart c)))
              (i (scale-to-ushort (imagpart c))))
          (write-byte (ldb (byte 8 8) r) stream)  ; high byte of real
          (write-byte (ldb (byte 8 0) r) stream)  ; low byte of real
          (write-byte (ldb (byte 8 8) i) stream)  ; high byte of imag
          (write-byte (ldb (byte 8 0) i) stream))))
  (format t "-- wrote ~a.~%" fpath))

;; (cl-radar.mod::graph-bpsk-fft :all-ones-p t :sample-rate 25e6 :cfreq -1e6 :num-symbols 100)
;; (cl-radar.mod::write-complex-array-to-binary-file "/Users/jackc/out.iq.short.bin" cl-radar.mod::*last-complex-bb-samples*)

(defvar *last-pre-i* nil)
(defvar *last-pre-q* nil)

(defun write-sc16-sine-to-file (fpath num-samples sample-rate freq-hz &optional (amplitude 1.0))
  (let ((omega (/ (* 2 Pi freq-hz) sample-rate))
        (pre-i (make-array 1000 :initial-element 0.0d0))
        (pre-q (make-array 1000 :initial-element 0.0d0)))
    (with-open-file (stream fpath
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (loop for i from 0 below num-samples
            do
               (let* ((phase (* i omega))
                      (real-val (* amplitude (cos phase)))
                      (imag-val (* amplitude (sin phase)))
                      (real-short (scale-to-sshort real-val))
                      (imag-short (scale-to-sshort imag-val)))
                 (when (< i 1000)
                   (setf (aref pre-i i) real-val)
                   (setf (aref pre-q i) imag-val))
                 (let* ((q-shifted (+ imag-short 32768))
                        (q-msb-val (logand 127 (round (/ (abs q-shifted) 256))))
                        (q-msb-neg (logior 128 q-msb-val)))
                   ;;(if (< imag-val 0.0)                            ;; MSB
                   ;;    (write-byte (ldb (byte 8 0) q-msb-neg) stream)
                   ;;    (write-byte (ldb (byte 8 0) q-msb-val) stream))
                   (write-byte (ldb (byte 8 8) q-shifted) stream) ;; was as above
                   (write-byte (ldb (byte 8 0) imag-short) stream) ;; LSB
                   )

                 (let* ((i-shifted (+ real-short 32768))
                        (i-msb-val (logand 127 (round (/ (abs i-shifted) 256))))
                        (i-msb-neg (logior 128 i-msb-val)))
                   (if (< real-val 0.0)                            ;; MSB
                       (write-byte (ldb (byte 8 0) i-msb-neg) stream)
                       (write-byte (ldb (byte 8 0) i-msb-val) stream))
                   (write-byte (ldb (byte 8 0) real-short) stream) ;; LSB
                   ))))
    (setf *last-pre-i* pre-i)
    (setf *last-pre-q* pre-q))

  (format t "-- wrote ~a.~%" fpath))

(defun plot-parts ()
  (let ((x-axis (loop for i below (length *last-pre-i*) collecting i)))
    (vgplot:plot x-axis *last-pre-i* x-axis *last-pre-q*)))


;; more pretty graphs
@export
(defun graph-bpsk-fft-chunked (&key (sample-rate 48000) (symbol-rate 12000)
                                 (num-symbols 1024) (cfreq -10000) (all-ones-p nil))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((samp-per-sym (/ (float sample-rate) symbol-rate))
           (num-samples (* num-symbols samp-per-sym))
           (bpsk-mod (make-bpsk-modulator sample-rate symbol-rate cfreq))
           (gold-code (make-array num-symbols :initial-contents
                                  (funcall gold-gen num-symbols)))
           (bb-samples (funcall bpsk-mod (if all-ones-p
                                            (make-array num-symbols :initial-element 1)
                                            gold-code)))
           (fft-summed (complex-fft-in-chunks 256 bb-samples))
           (fft-swapped (cl-radar.math:fft-swap fft-summed)))
      (format t "-- ~a symbols, ~a samples.~%" num-symbols num-samples)
      (format t "-- fft-swapped ~a.~%" (length fft-swapped))

      (let* ((fft-len (length fft-swapped))
             (f-start (* -1 (/ sample-rate 2.0)))
             (f-end (/ sample-rate 2.0))
             (d-f (/ sample-rate fft-len))
             (x-axis (loop for x from f-start to f-end by d-f
                           collecting x)))
        (vgplot:plot x-axis fft-swapped "mag")))))

;; (cl-radar.mod::graph-bpsk-fft-chunked :cfreq -10000 :symbol-rate 12000)
;; (cl-radar.mod::graph-bpsk-fft-chunked :cfreq -10000 :symbol-rate 6000)

@export
(defun graph-bpsk-fft-chunked-filtered (&key (sample-rate 48000) (symbol-rate 12000)
                                 (num-symbols 2048) (cfreq -10000) (all-ones-p nil))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((samp-per-sym (/ (float sample-rate) symbol-rate))
           (num-samples (* num-symbols samp-per-sym))
           (bpsk-mod (make-bpsk-modulator sample-rate symbol-rate cfreq))
           (gold-code (make-array num-symbols :initial-contents
                                  (funcall gold-gen num-symbols)))
           (bb-samples (funcall bpsk-mod (if all-ones-p
                                            (make-array num-symbols :initial-element 1)
                                            gold-code)))
           (bb-filter (cl-radar.filter::make-complex-fir-bandpass-filter
                       sample-rate -10000
                       44000 ;; 4400 gives one lobe on either side; main lobe about 2500
                       11)) ;; 33 still serious, 101 very very sharp
           (bb-filtered (cl-radar.math:array-mapcar (lambda (v) (funcall bb-filter v)) bb-samples))
           ;;(fft-summed (complex-fft-in-chunks 256 bb-samples))
           (fft-summed (complex-fft-in-chunks 256 bb-filtered))
           (fft-swapped (cl-radar.math:fft-swap fft-summed)))
      (format t "-- ~a symbols, ~a samples.~%" num-symbols num-samples)
      (format t "-- fft-swapped ~a.~%" (length fft-swapped))

      ;;(loop for i from 0 below 10
      ;;      do
      ;;      (format t "---- ~a~t~a~%" (aref bb-samples i) (aref bb-filtered i)))

      (let* ((fft-len (length fft-swapped))
             (f-start (* -1 (/ sample-rate 2.0)))
             (f-end (/ sample-rate 2.0))
             (d-f (/ sample-rate fft-len))
             (x-axis (loop for x from f-start to f-end by d-f
                           collecting x)))
        (vgplot:plot x-axis fft-swapped "mag")))))

#|
(cl-radar.mod::graph-bpsk-fft-chunked-filtered :cfreq -10000 :symbol-rate 5000)
|#

;; TODO: standardized wrapper for exercising and graphing and ffting etc these
;;  modulations; include fft summing instead of one giant fft

;; TODO: put this somewhere
(defun complex-fft-in-chunks (fft-size in-ar)
  (let ((num-chunks (floor (/ (length in-ar) fft-size))))
    (format t "-- fft-size ~a and input len ~a, so doing ~a ffts (using ~a samples).~%"
            fft-size (length in-ar) num-chunks (* num-chunks fft-size))

    (let ((fft-sum (make-array fft-size :initial-element 0.0d0)))
      (loop for chunk-start from 0 below (length in-ar) by fft-size
            do
               (let* ((this-chunk (subseq in-ar chunk-start (+ chunk-start fft-size)))
                      (bb-csb (cl-radar.math::convert-to-padded-csarray this-chunk))
                      (bb-fft (bordeaux-fft:fft bb-csb))
                      (fft-mags (cl-radar.math:complex-mags bb-fft)))
                 (cl-radar.math::ar-incf fft-sum fft-mags)))
      fft-sum)))



(defun make-bpsk-modulator (sample-rate symbol-rate freq-offset)
  (let ((sps (round (/ sample-rate symbol-rate)))
        (omega (/ (* 2 pi freq-offset) sample-rate)))
    (lambda (symbols)
      (let ((output (make-array (* (length symbols) sps)
                                :initial-element #c(0.0d0 0.0d0))))
        (loop for i from 0 below (length symbols) do
          (let ((phase-offset (if (zerop (aref symbols i))
                                  0d0
                                  pi)))
            (loop for j from 0 below sps do
              (let* ((index (+ (* i sps) j))
                     (phase (+ (* index omega) phase-offset)))
                (setf (aref output index)
                      (complex (cos phase) (sin phase)))))))
        output))))




;;; ------ ofdm ----
;;; TODO: psk and qam of each

(defun fftshift (ar) ;; swap left and right halves
  (let* ((ar-len (length ar))
         (half-len (/ ar-len 2)))
    (concatenate 'simple-vector
                 (subseq ar half-len ar-len)
                 (subseq ar 0 half-len))))

(defun map-ofdm-bits (data-ar start-i n)
  (let ((result (make-array (* 8 n))))
    (loop
      for i from 0 below n
      for b = (aref data-ar (+ start-i i))
      for offset = (* i 8) ;; byte offset
      do (loop
           for bit from 7 downto 0
           for index = (+ offset (- 7 bit))
           do (setf (aref result index)
                    (if (zerop (logand b (ash 1 bit)))
                        -1 1))))
    result))

#|
CL-USER> (cl-radar.mod::map-ofdm-bits #(1 252) 0 2)
#(-1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 1 -1 -1)
CL-USER> (cl-radar.mod::map-ofdm-bits #(1 252 5 4) 0 2)
#(-1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 1 -1 -1)
CL-USER> (cl-radar.mod::map-ofdm-bits #(1 252 5 4) 0 4)
#(-1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1 -1 -1 1 -1 1 -1 -1 -1 -1 -1
1 -1 -1)
|#


(defun ofdm-mod ())
