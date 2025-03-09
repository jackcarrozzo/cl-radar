;;



(defun t1 (&key (sig-len 1000) (slide-len 12))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((gold-code
             (make-array sig-len
                         :initial-contents (funcall gold-gen sig-len)))
           (fsk-modulator (cl-radar.mod::make-fsk-modulator :samples-per-symbol 4 :sample-rate 10000 :center-frequency 350))
           (iq-samples (funcall fsk-modulator gold-code))
           (ref-slice (subseq iq-samples 0 (- (length iq-samples) slide-len)))
           (sig-slice (subseq iq-samples 11 (- (length iq-samples) (- slide-len 11))))
           (corr-out (cl-radar.corr:sliding-complex-correlation
                      sig-slice ref-slice slide-len)))
      (format t "gold: ~a~%" gold-code)
      ;;(format t "samps: ~a~%" iq-samples)
      (format t "-- ~a samples total: ~a~%"
              (length iq-samples) (subseq iq-samples 0 20))
      (format t "---- ref: ~a~%" (subseq ref-slice 0 30))
      (format t "---- sig: ~a~%" (subseq sig-slice 0 30))
      (format t "-- corr: ~a~%" corr-out)
      (vgplot:plot corr-out))))

;;



;; bpsk test: tx side is using this gold code

(defparameter +tx-goldcode+
  '(1 0 1 1 0 0 0 1 0 1 0 0 1 0 1 0 1 1 1 0 0 1 0 0 0 0 1 1 0 0 0 1 1 0 0 1 0 1 1
    1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 1 0 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 1 0 0 1 1
    1 1 1 1 1 0 1 0 1 0 0 0 0 0 1 1 1 0 1 0 1 0))

#|
CL-USER> (cl-radar.corr::gen-some-gold-code 100)
(1 0 1 1 0 0 0 1 0 1 0 0 1 0 1 0 1 1 1 0 0 1 0 0 0 0 1 1 0 0 0 1 1 0 0 1 0 1 1
1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 1 0 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 1 0 0 1 1
1 1 1 1 1 0 1 0 1 0 0 0 0 0 1 1 1 0 1 0 1 0)
CL-USER> (cl-radar.corr::gold-code-as-c-array 100)
uint8_t gold_code[]={1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0};
|#

#|
OTW format: sc16
-- spb is 2904, tx_stream max samps is 363
-- sending 1 buffs total of length 2904.
Setting device timestamp to 0...
Setting TX Freq: 1500.000000 MHz...
Setting TX LO Offset: 0.000000 MHz...
Actual TX Freq: 1500.000000 MHz...

Checking TX: LO: locked ...
Press Ctrl + C to stop streaming...
-- omega: -0.628319, ampl: 0.3
-- samples per wave period: 9.99999
-- samples per symbol: 80
-- symbols per second: 125000
---- inner loop is 2904 iters.
|#

;; phase of baseband carrier resets to 0 each time gold code wraps
(defun make-bpsk-reference-sig (&key (sample-rate 10000000) (syms +tx-goldcode+)
                                  (symbol-rate 125000) (cfreq -1000000))
  (let* ((sym-ar (make-array (length syms) :initial-contents syms))
         (bpsk-mod (cl-radar.mod:make-bpsk-modulator sample-rate symbol-rate cfreq))
         (bb-samples (funcall bpsk-mod sym-ar)))
    (format t "-- ~a syms in, ~a samples out.~%"
            (length syms) (length bb-samples))
    bb-samples))

;; TODO: put this in utils maybe?
(defun read-sc16 (filename &optional (little-endian t))
  (with-open-file (stream filename
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((n-bytes (file-length stream))
           (n-samples (/ n-bytes 2))
           ;;(n-samples 300)
           (r-ints (make-array n-samples :element-type '(signed-byte 16)))
           (r (make-array (/ n-samples 2) :initial-element #c(0.0d0 0.0d0)))
           (q-val 0))
      (dotimes (i n-samples)
        (let ((low  (read-byte stream))
              (high (read-byte stream)))
          (let ((value (if little-endian
                           (logior (ash high 8) low)
                           (logior (ash low 8) high))))
            (when (>= value #x8000)
              (incf value (* -1 #x10000)))

            (setf (aref r-ints i) value)

            (if (= 0 (mod i 2))
                (progn
                  ;;(format t "setting q-val to: ~a~%" value)
                  (setf q-val value))
                (progn
                  ;;(format t "q-val is ~a~%" q-val)
                  (setf (aref r (/ (- i 1) 2))
                        (complex
                         (coerce value 'double-float)
                         (coerce q-val 'double-float))))))))
      ;;(values r r-ints)
      r)))


(defun graph-gen-vs-recorded ()
  (let* ((ref-ar (make-bpsk-reference-sig))
         (sig-ar (read-sc16 "/Users/jackc/txb-1e6_10e6_g40.short.dat"))
         (corr-ar (cl-radar.corr::longish-sliding-complex-correlation ref-ar sig-ar :compare-fn #'cl-radar.corr::complex-vector-euclidean-dist)))
    (format t "-- ref-ar is ~a, sig-ar is ~a.~%" (length ref-ar) (length sig-ar))
    (format t "-- corr-ar is ~a long.~%" (length corr-ar))
    (vgplot:plot corr-ar)))


;; ----

;; TODO: try as 2x real-only inputs as well as complex


;; make a zero (or #c(0.0 0.0)) valued array and copy ar into it centered within
(defun double-ended-pad (len ar)
  (let* ((ar-len (length ar))
         (pad-len (- len ar-len))
         (type-of-first-item (type-of (aref ar 0)))
         (r (make-array
             len
             :initial-element (if (and (eql 'cons (type-of type-of-first-item))
                                       (eql 'complex (first type-of-first-item)))
                                  #c(0.0d0 0.0d0)
                                  0.0d0))))
    (cl-radar.math:array-copy-into ar 0 ar-len r (round (/ pad-len 2)))))

(defun cs-double-ended-pad (len ar)
  (let* ((ar-len (length ar))
         (pad-len (- len ar-len))
         (r (cl-radar.math:make-csarray len)))
    (cl-radar.math:array-copy-into ar 0 ar-len r (round (/ pad-len 2)))))

#|
CL-USER> (double-ended-pad 8 #(3.0 4.0 5.0 6.0))
#(0.0d0 0.0d0 3.0 4.0 5.0 6.0 0.0d0 0.0d0)
CL-USER> (double-ended-pad 8 #(3.0 4.0 5.0 6.0 7.0))
#(0.0d0 0.0d0 3.0 4.0 5.0 6.0 7.0 0.0d0)
CL-USER> (double-ended-pad 8 #(3.0 4.0 5.0 6.0 7.0 8.0))
#(0.0d0 3.0 4.0 5.0 6.0 7.0 8.0 0.0d0)
CL-USER> (double-ended-pad 8 #(#c(3.0 4.0) #c(5.0 6.0)))
#(#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(3.0 4.0) #C(5.0 6.0)
#C(0.0d0 0.0d0) #C(0.0d0 0.0d0) #C(0.0d0 0.0d0))
|#

;; s1 and s2 dont have to be the same length, but they both have to be
;;   less than the same power of 2
(defun fft-correlation (s1 s2)
  (let* ((fft-size (cl-radar.math:next-power-of-two
                    (max (length s1) (length s2))))
         (padded-s1 (cs-double-ended-pad fft-size s1))
         (padded-s2 (cs-double-ended-pad fft-size s2))
         ;;(fft1 (bordeaux-fft:windowed-fft padded-s1 (/ fft-size 2) fft-size))
         ;;(fft2 (bordeaux-fft:windowed-fft padded-s2 (/ fft-size 2) fft-size))
         (fft1 (bordeaux-fft:fft padded-s1))
         (fft2 (bordeaux-fft:fft padded-s2))
         (fft2* (cl-radar.math:array-mapcar #'conjugate fft2))
         (both-fft (cl-radar.math::csarrays-multiply fft1 fft2*))
         (corr (bordeaux-fft:ifft both-fft)))
    (cl-radar.math:graph-complex-ar corr)))

(defun graph-gen-vs-recorded-fft ()
  (let* ((ref-ar (make-bpsk-reference-sig))
         (sig-ar (read-sc16 "/Users/jackc/txb-1e6_10e6_g40.short.dat"))
         (corr-ar (fft-correlation
                   ref-ar
                   (subseq sig-ar
                           (- (length sig-ar) (length ref-ar))
                           (length sig-ar)))))
    (format t "-- ref-ar is ~a, sig-ar is ~a.~%" (length ref-ar) (length sig-ar))
    (format t "-- corr-ar is ~a long.~%" (length corr-ar))))
