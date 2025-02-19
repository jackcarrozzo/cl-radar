;;



(defun t1 (&key (sig-len 100) (slide-len 22))
  (multiple-value-bind (gold-gen gold-repeat-n)
      (cl-radar.corr::make-gold-code-generator :offset 3 :taps1 '(1 3 4 5 6) :taps2 '(6 4 2 1 0))
    (declare (ignore gold-repeat-n))
    (let* ((gold-code
             (make-array sig-len
                         :initial-contents (funcall gold-gen sig-len)))
           (fsk-modulator (cl-radar.mod::make-fsk-modulator :samples-per-symbol 2))
           (iq-samples (funcall fsk-modulator gold-code))
           (ref-slice (subseq iq-samples 0 (- (length iq-samples) slide-len)))
           (sig-slice (subseq iq-samples 20 (- (length iq-samples) (- slide-len 20))))
           (corr-out (cl-radar.corr:sliding-complex-correlation
                      sig-slice ref-slice slide-len)))
      (format t "gold: ~a~%" gold-code)
      ;;(format t "samps: ~a~%" iq-samples)
      (format t "-- ~a samples total: ~a~%"
              (length iq-samples) (subseq iq-samples 0 10))
      (vgplot:plot corr-out))))
