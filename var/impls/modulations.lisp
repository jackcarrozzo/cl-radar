;; scratchpad


(defun float-xor (v1 v2)
  (if (= 1 (count-if
            (lambda (v) (> v 0.0))
            (list v1 v2)))
      1
      0))

(defun xor-float-arrays (ar1 ar2)
  (let* ((len (min (length ar1) (length ar2)))
         (r (make-array len)))
    (loop for i from 0 below len
          do
             (setf (aref r i)
                   (float-xor (aref ar1 i) (aref ar2 i))))
    r))

#|
CL-USER> (xor-float-arrays (sq-wave 8 16) (sq-wave 8 16 :phase-samples 2))
#(0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1)
CL-USER> (cl-radar.math:array-average (xor-float-arrays (sq-wave 8 16) (sq-wave 8 16 :phase-samples 2)))
0.5
|#

(defun sq-wave (period-samples-f len-samples &key (phase-samples 0) (ampl 2.0) (center 0.0) (duty 0.5))
  (let (;;(num-periods (/ len-samples period-samples-f))
        (half-ampl (/ ampl 2))
        (r (make-array len-samples :initial-element 0.0)))
    (loop for i from 0 below len-samples
          do
             (let ((wave-posn (/ (float (mod (+ i phase-samples) period-samples-f))
                                 period-samples-f)))
               (setf (aref r i)
                     (if (< wave-posn duty)
                         (- center half-ampl)
                         (+ center half-ampl)))))
    r))




(defun test-xor-phases (&key (period 64) (n-samples 900))
  (let* ((ref (sq-wave period n-samples))
         (xs '())
         (vals
           (loop for phi from 2.0 to 6.0 by 0.02
                 collecting
                 (progn
                   (push phi xs)
                   (cl-radar.math:array-average
                    (xor-float-arrays
                     ref
                     (sq-wave period n-samples :phase-samples phi)))))))

    (setf xs (reverse xs))

    (format t "-- xs ~a: ~a~%" (length xs) xs)
    (format t "-- vals ~a: ~a~%" (length vals) vals)
    (vgplot:plot (make-array (length xs) :initial-contents xs)
                 (make-array (length vals) :initial-contents vals))))


(defun test-xor-freqs (&key (period 64) (n-samples 1900))
  (let* ((ref (sq-wave period n-samples))
         (xs '())
         (vals
           (loop for df from 1.2 downto 0.8 by 0.002
                 collecting
                 (progn
                   (push df xs)
                   (cl-radar.math:array-average
                    (xor-float-arrays
                     ref
                     (sq-wave (* df period) n-samples
                              :phase-samples (floor (/ period 4)))))))))

    (setf xs (reverse xs))

    (format t "-- xs ~a: ~a~%" (length xs) xs)
    (format t "-- vals ~a: ~a~%" (length vals) vals)
    (vgplot:plot (make-array (length xs) :initial-contents xs)
                 (make-array (length vals) :initial-contents vals))))
