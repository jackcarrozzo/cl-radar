(in-package :cl-user)
(defpackage cl-radar.math
  (:use :cl))
(in-package :cl-radar.math)
(cl-syntax:use-syntax :annot)

@export
(defun first-relative-prime (n &optional (start 2))
  (loop for r from start below n
        when (= (gcd r n) 1)
          do (return r)
        finally (return nil)))

@export
(defun all-relative-primes (n)
  (remove nil
          (loop for r from 1 below n
                collecting
                (if (= 1 (gcd r n))
                    r nil))))

@export
(defun zgcd (a b)
  (if (zerop b)
      a
      (gcd b (mod a b))))

@export
(defun zlcm (a b)
  (if (or (zerop a) (zerop b))
      0
      (/ (abs (* a b)) (gcd a b))))

@export
(defun xor (a b)
  (assert (or
           (and (equal 'bit (type-of a))
                (equal 'bit (type-of b)))
           (and (equal 'integer (first (type-of a)))
                (equal 'integer (first (type-of b))))))
  (if (= 1 (+ a b))
      1
      0))

;; random sample using the Box–Muller transform
@export
(defun random-gaussian (&optional (mean 0.0) (sd 1.0))
  (declare (type float mean sd))
  (let* ((u1 (max 1e-30 (random 1.0)))  ; avoid taking (log 0)
         (u2 (random 1.0))
         (mag (sqrt (* -2.0 (log u1))))
         (z0  (* mag (cos (* 2 pi u2)))))
    (+ mean (* z0 sd))))

;; TODO: this might fit better elsewhere
@export
(defun add-channel-noise (samples &key (mean 0.0) (stddev 0.1))
  (let ((n (length samples))
        (result (make-array (length samples) :initial-element 0.0)))
    (loop for i from 0 below n do
      (setf (aref result i)
            (+ (aref samples i)
               (random-gaussian mean stddev))))
    result))

(defparameter +max-float-diff+ 0.0002)

@export
(defun float-array-mostly-equal-p (ar1 ar2)
  (if (or
       (not ar1)
       (not ar2)
       (not (= 1 (length (array-dimensions ar1))))
       (not (equal (array-dimensions ar1)
                   (array-dimensions ar2))))
      nil
      (let ((continue-p t))
        (loop for i from 0 below (array-dimension ar1 0)
              while continue-p
              do
                 (when (< +max-float-diff+ (abs (- (aref ar2 i)
                                                   (aref ar1 i))))
                   (setf continue-p nil)))
        continue-p)))

@export
(defun float-multidim-array-mostly-equal-p (ar1 ar2)
  (if (or
       (not ar1)
       (not ar2)
       (not (equal (array-dimensions ar1)
                   (array-dimensions ar2))))
      nil
      (let ((dimensions-of-ar (first (array-dimensions ar1)))
            (elements-per-ar (second (array-dimensions ar1)))
            (continue-p t))

        (loop for dim from 0 below dimensions-of-ar
              while continue-p
              do
                    (loop for i from 0 below elements-per-ar
                          while continue-p
                          do
                             (when (< +max-float-diff+ (abs (- (aref ar2 dim i)
                                                               (aref ar1 dim i))))
                               (setf continue-p nil))))
        continue-p)))

@export
(defun complex-array-difference (ar1 ar2)
  (assert (= (length ar1) (length ar2)))

  (let ((sum 0.0d0))
    (dotimes (i (length ar1))
      (let ((ar1-i (realpart (aref ar1 i)))
            (ar1-q (imagpart (aref ar1 i)))
            (ar2-i (realpart (aref ar2 i)))
            (ar2-q (imagpart (aref ar2 i))))
        (incf sum
              (sqrt
               (+
                (expt (- ar2-i ar1-i) 2)
                (expt (- ar2-q ar1-q) 2))))))
    sum))

@export
(defun complex-array-mostly-equal-p (ar1 ar2 &optional (max-diff-per-el 0.001))
  (< (complex-array-difference ar1 ar2) (* max-diff-per-el (length ar1))))

@export
(defun complex-multidim-array-difference (ar1 ar2)
  (assert (equal (array-dimensions ar1)
                 (array-dimensions ar2)))

  (let ((sum 0.0d0))
    (dotimes (i (array-dimension ar1 0))
      (dotimes (j (array-dimension ar1 1))
        (let ((ar1-i (realpart (aref ar1 i j)))
              (ar1-q (imagpart (aref ar1 i j)))
              (ar2-i (realpart (aref ar2 i j)))
              (ar2-q (imagpart (aref ar2 i j))))
          (incf sum
                (sqrt
                 (+
                  (expt (- ar2-i ar1-i) 2)
                  (expt (- ar2-q ar1-q) 2)))))))
    sum))

@export
(defun complex-multidim-array-mostly-equal-p (ar1 ar2 &optional (max-diff-per-el 0.001))
  (< (complex-multidim-array-difference ar1 ar2)
     (* max-diff-per-el
        (array-dimension ar1 0)
        (array-dimension ar1 1))))

@export
(defun list-of-arrays-equal (list1 list2)
  (and
   (= (length list1) (length list2))
   (= (length list1)
      (length (remove-if #'not
                         (loop for a1 in list1
                               for a2 in list2
                               collecting
                               (float-array-mostly-equal-p a1 a2)))))))

;; bordeaux-fft internal format compatible complex-sample arrays
@export
(defmacro make-csarray (n &optional initial-contents)
  (let ((args `(make-array ,n :element-type 'bordeaux-fft:complex-sample)))
    (if initial-contents
        `(,@args :initial-contents ,initial-contents)
        args)))

@export
(defun convert-to-csarray (in-ar)
  (let ((r (make-csarray (length in-ar))))
    (dotimes (i (length r))
      (setf (aref r i)
            (complex
             (float (realpart (aref in-ar i)) 0.0d0) ;; convert to double-float
             (float (imagpart (aref in-ar i)) 0.0d0))))
    r))

@export
(defun convert-to-padded-csarray (in-ar &optional pad-to-n)
  (let ((r (make-csarray (or pad-to-n
                             (next-power-of-two (length in-ar))))))
    (dotimes (i (length in-ar))
      (setf (aref r i)
            (complex
             (float (realpart (aref in-ar i)) 0.0d0) ;; convert to double-float
             (float (imagpart (aref in-ar i)) 0.0d0))))
    r))

;; make a zero (or #c(0.0 0.0)) valued array and copy ar into it centered within
@export
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
    (array-copy-into ar 0 ar-len r (round (/ pad-len 2)))))

@export
(defun cs-double-ended-pad (len ar)
  (let* ((ar-len (length ar))
         (pad-len (- len ar-len))
         (r (make-csarray len)))
    (cl-radar.math:array-copy-into ar 0 ar-len r (round (/ pad-len 2)))))

@export
(defun csarrays-multiply (s1 s2)
  (assert (= (length s1) (length s2)))
  (let ((r (make-csarray (length s1))))
    (dotimes (i (length s1))
      (setf (aref r i)
            (* (aref s1 i) (aref s2 i))))
    r))

@export
(defun complex-named-window-in-place (ar &optional (window-fn #'bordeaux-fft:hann))
  (let ((n (length ar)))
    (loop for i from 0 below n
          do
             (let ((v (aref ar i))
                   (win-val (funcall window-fn i n)))
               (setf (aref ar i)
                     (complex (* win-val (realpart v))
                              (* win-val (imagpart v))))))
    ar))

@export
(defun graph-complex-ar (in-ar &optional include-magnitude-p)
  (let ((mags (if include-magnitude-p (complex-mags in-ar)))
        (reals (array-mapcar #'realpart in-ar))
        (imags (array-mapcar #'imagpart in-ar))
        (x-ax (loop for i from 0 below (length in-ar) collecting i)))
    (if include-magnitude-p
        (vgplot:plot x-ax mags "mags" x-ax reals "reals" x-ax imags "imags")
        (vgplot:plot x-ax reals "reals" x-ax imags "imags")
        )))

@export
(defun graph-complex-fft (in-ar &optional sample-rate)
  (let* ((bb-csb (convert-to-padded-csarray in-ar))
         (bb-fft (bordeaux-fft:fft bb-csb))
         (fft-swapped (cl-radar.math:fft-swap bb-fft))
         (fft-mags (cl-radar.math:complex-mags fft-swapped)))
    (if sample-rate
        (let* ((fft-len (length bb-fft))
               (f-start (* -1 (/ sample-rate 2.0)))
               (f-end (/ sample-rate 2.0))
               (d-f (/ sample-rate fft-len))
               (x-axis (loop for x from f-start to f-end by d-f
                             collecting x)))
          (vgplot:plot x-axis fft-mags "mag"))
        (vgplot:plot fft-mags))))


;; ref its highest value; in place
@export
(defun dB-ulize-array-in-place (in-ar)
  (let ((ar-len (array-dimension in-ar 0))
        (ar-max (aref in-ar 0)))
    (loop for i from 0 below ar-len ;; find max
          do
             (when (> (aref in-ar i) ar-max)
               (setf ar-max (aref in-ar i))))
    (loop for i from 0 below ar-len ;; apply log
          do
             (setf (aref in-ar i)
                   (*
                    (realpart
                     (log
                      (/
                       (aref in-ar i)
                       (+ ar-max 0.000001)) ;; avoid div by 0
                      10))
                    10)))))

@export
(defun dB-ulize-array (in-ar)
  (let* ((ar-len (array-dimension in-ar 0))
         (r (make-array ar-len :initial-element 0.0d0))
         (ar-max (aref in-ar 0)))
    (loop for i from 0 below ar-len ;; find max
          do
             (when (> (aref in-ar i) ar-max)
               (setf ar-max (aref in-ar i))))
    (loop for i from 0 below ar-len ;; apply log
          do
             (setf (aref r i)
                   (*
                    (realpart
                     (log
                      (/
                       (aref in-ar i)
                       (+ ar-max 0.000001)) ;; avoid div by 0
                      10))
                    10)))
    r))


(defparameter +comp-ar-mag-dbg+ t) ;; t

@export
(defun array-mapcar (fn ar)
  (let ((r (make-array (length ar) :initial-element 0.0d0)))
    (dotimes (i (length ar))
      (setf (aref r i)
            (funcall fn (aref ar i))))
    r))

@export
(defun complex-mags (ar) ;; this is the simple one
  (array-mapcar
   (lambda (c)
     (sqrt
      (+ (expt (realpart c) 2)
         (expt (imagpart c) 2))))
   ar))

;; [0 - (n/2)-1][n/2 - n] to
;; [n/2 - n][0 - (n/2)-1]
@export
(defun fft-swap (in-ar)
  (let* ((n (length in-ar))
         (half-n (floor (/ n 2)))
         (r (make-array n :initial-element (aref in-ar 0))))
    (array-copy-into in-ar half-n n r 0)
    (array-copy-into in-ar 0 half-n r half-n)
    r))

@export
(defun complex-reals (ar)
  (array-mapcar #'realpart ar))

@export
(defun complex-imags (ar)
  (array-mapcar #'imagpart ar))

@export
(defun complex-ar-mags (complex-src real-dst &optional incf-p) ;; TODO: fft specific, rename
  (if (not (= (/ (array-dimension complex-src 0) 2)
              (array-dimension real-dst 0)))
      (format t "--- complex-ar-mags: odd sizes, complex-src ~a vs real-dst ~a, skipping. ~%"
              (array-dimension complex-src 0)
              (array-dimension real-dst 0))

      ;; just copy up as far as dst goes, because we only care about the first
      ;;   half of fft data (TODO: confirm this is true)
      (loop for i from 0 below (array-dimension real-dst 0)
            do
               (progn
                 ;;(format t "i ~a, complex size ~a, real size ~a.~%"
                 ;;        i
                 ;;        (array-dimension complex-src 0)
                 ;;        (array-dimension real-dst 0))
                 (if incf-p ;; TODO: there is proilly a cleaner way to do this
                     (incf (aref real-dst i)
                           (sqrt (+ (expt (realpart (aref complex-src i)) 2)
                                    (expt (imagpart (aref complex-src i)) 2))))
                     (setf (aref real-dst i)
                           (sqrt (+ (expt (realpart (aref complex-src i)) 2)
                                    (expt (imagpart (aref complex-src i)) 2))))))))
      real-dst)


;; TODO: use this below to streamline find-edges, filter-wack-edges,
;;   apply-edge-offsets, zero-pad-and-copy

;; TODO: this probably is fucked up because it increases ar size if it sees a single slice
;;   large enough to kick it up, which fft will then use as its size etc etc
;;   need to decide ar and fft size at read-and-slice-all level

@export
(defun array-slice-into-chunks (src-ar edges-list &optional dst-list)
  ;; slice src-ar into a list of ar chunks from [(start . finish)

  ;; (equal (first (type-of (first dst-list))) 'simple-vector)
  ;; allocate the ar into the list of its not the right type

  (let ((out-list
          (if (and dst-list
                   (>= (length dst-list) (length edges-list)))
              dst-list
              (loop for n from 0 below (length edges-list) collecting n)))
        (i 0))

    (loop for edge in edges-list
          do
             (let ((slice-start (first edge))
                   (slice-end (rest edge))
                   (dst-ar (nth i out-list)))
               (if (not (and (equal (type-of (type-of dst-ar)) 'cons) ;; insane
                             (equal (first (type-of dst-ar)) 'simple-vector)
                             (>= (array-dimension dst-ar 0) (- slice-end slice-start))))
                   (progn
                     (setf (nth i out-list)
                           (make-array (next-power-of-two (- slice-end slice-start)) :initial-element 0.0))))
               (array-copy-into src-ar slice-start slice-end (nth i out-list))

               (incf i)))
    out-list))

;; copy src-ar[start:end] into dst-ar starting at dst-offset
@export
(defun array-copy-into (src-ar src-start src-end dst-ar &optional (dst-offset 0))
  ;;(format t "ar dim dist-ar 0: ~a~%" (array-dimension dst-ar 0))
  ;;(format t "math end start: ~a~%" (+ (- end start) dst-offset))

  (if (= 0 src-end)
      (setf src-end (length src-ar)))

  (assert (>= (array-dimension dst-ar 0) (+ (- src-end src-start) dst-offset)))
  ;;(assert (>= (array-dimension src-ar 0) (- src-end 1)))
  (assert (>= (array-dimension src-ar 0) src-end))
  ;;(assert (> src-end src-start))

  (loop for i from src-start below src-end
        do
           (setf (aref dst-ar (+ (- i src-start) dst-offset))
                 (aref src-ar i)))
  dst-ar)

;; TODO: needs tests
;; used to zero end of sample ar
@export
(defun array-set-range (ar start end value)
  (assert (>= start 0))
  (assert (<= end (array-dimension ar 0)))

  (loop for i from start below end
        do
        (setf (aref ar i) value)))

@export
(defun dc-center-slice (ar &optional (debug-p nil))
  ;; dc center a single array (find mean and subtract it from each sample)

  (let ((mean (array-average ar)))
    (when debug-p
      (format t "-- this mean is ~a.~%" mean))

    (array-add-offset ar (* -1 mean))))

@export
(defun dc-center-slices (slices)
  ;; dc center a list of arrays
  (mapcar #'dc-center-slice slices))

;;;;;;;;;;;;;; fft (TODO: ) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defvar *loop-fft-sums* nil)
(defvar *loop-fft-sums-size* nil)

;; takes a list of sample arrays, ffts them, and sums the
;;   mag results into *loop-fft-sum*
@export
(defun fft-and-sum-slices (slices-list &optional dont-reset-p)
  (assert slices-list)

  (format t "------ fft-and-sum-slices: got ~a slices~%" (length slices-list))

  (when (or (not dont-reset-p)
            (not *loop-fft-sums*))
    (setf *loop-fft-sums*
          (make-array (/ (array-dimension (first slices-list) 0) 2)
                      :initial-element 0.0d0)))

  ;; random-ish (TODO:) selection of correct ar size
  (setf *loop-fft-sums-size*
        (array-dimension
         (nth
          (if (> (length slices-list) 10) 5 1)
          slices-list)
         0)) ;; makes tests with short slice lists pass

  ;; TODO: use one array length the whole time

  (let ((skips 0))
    (loop for n from 0 below (length slices-list)
          do
             (let* ((this-ar (nth n slices-list))
                    (slice-len (array-dimension this-ar 0))
                    (fft-out (bordeaux-fft:windowed-fft
                              this-ar (/ slice-len 2) slice-len
                              'bordeaux-fft:blackman-harris)))

               ;; TODO: only add them if sizes are sane
               (if (= *loop-fft-sums-size* slice-len)
                   (if (= 0 n)
                       (complex-ar-mags fft-out *loop-fft-sums*)
                       (complex-ar-mags fft-out *loop-fft-sums* :incf))
                   (progn
                     ;;(format t "------- fft-and-sum-slices: skip ~a fft incf because we expect ~a but this slice is size ~a.~%"
                     ;;        (1+ skips) *loop-fft-sums-size* slice-len)
                     (incf skips)))))
    (format t "------ fft-and-sum-slices: skipped ~a of ~a slices.~%"
            skips (length slices-list)))
  *loop-fft-sums*)

;; ffts a list of slices and returns a list of their results (no summing)
@export
(defun fft-slices (slices-list) ;; used only by .fmcw:format-json-waterfall
  (assert slices-list)

  (format t "------ fft-slices: got ~a slices~%" (length slices-list))

  (let ((r '())
        (this-r))
    (loop for n from 0 below (length slices-list)
          do
             (let* ((this-ar (nth n slices-list))
                    ;;(slice-len (array-dimension this-ar 0))
                    (slice-len 512)
                    (fft-out (bordeaux-fft:windowed-fft
                              this-ar (/ slice-len 2) slice-len
                              'bordeaux-fft:blackman-harris)))
               (setf this-r
                     (make-array (array-dimension (first slices-list) 0)
                                 :initial-element 0.0d0))
               (complex-ar-mags fft-out this-r)
               (push this-r r)))
    (nreverse r)))

@export
(defvar *last-summed-ar* nil) ;; TODO: is this used?

;;;;;;;;;;;;;;;;;;;; array utils ;;;;;;;;;;;

@export
(defun sum-arrays (ar-list)
  (let ((r (make-array (array-dimension (first ar-list) 0) :initial-element 0.0d0)))
    (mapcar
     (lambda (ar)
       (loop for i from 0 below (array-dimension ar 0)
             do
                (incf (aref r i)
                      (aref ar i))))
     ar-list)
    (setf *last-summed-ar* r)))

@export
(defun subseq-array (ar start &optional len)
  (let* ((rlen (or len
                   (- (array-dimension ar 0)
                      start)))
         (r (make-array rlen :initial-element (aref ar 0))))

    (loop for i from 0 below rlen
          do
             (setf (aref r i)
                   (aref ar (+ i start))))
    r))

@export
(defun array-add-offset (ar offset)
  (loop for i from 0 below (array-dimension ar 0)
        do
           (incf (aref ar i)
                 offset))
  ar)

@export
(defun array-multiply-scalar (ar m)
  (loop for i from 0 below (array-dimension ar 0)
        do
           (setf (aref ar i)
                 (* m (aref ar i))))
  ar)

@export
(defun array-average (ar)
  (let ((sum 0.0))
    (loop for i from 0 below (array-dimension ar 0)
          do
             (incf sum (aref ar i)))
    (/ sum
       (float
        (array-dimension ar 0)))))

@export
(defun array-max-min (ar)
  (let ((max (aref ar 0))
        (min (aref ar 0)))
    (loop for i from 1 below (array-dimension ar 0)
          do
             (progn
               (when (> (aref ar i) max)
                 (setf max (aref ar i)))
               (when (< (aref ar i) min)
                 (setf min (aref ar i)))))
    (values max min)))

@export
(defun array-list-max (ar-list)
  (reduce #'max
          (mapcar #'array-max-min
                  ar-list)))

;; works for complex too
@export
(defun ar-incf (ar-added-into ar-to-add)
  (assert (= (length ar-added-into) (length ar-to-add)))
  (dotimes (i (length ar-added-into))
    (incf (aref ar-added-into i)
          (aref ar-to-add i))))

@export
(defun ar-scaleby! (ar scale)
  (dotimes (i (length ar))
    (setf (aref ar i)
          (* scale
             (aref ar i))))
  ar)

@export
(defun mags! (c-ar)
  (dotimes (i (array-dimension c-ar 0))
    (setf (aref c-ar i)
          (let ((v (aref c-ar i)))
            (sqrt
             (+ (expt (realpart v) 2)
                (expt (imagpart v) 2)))))))

@export
(defun reals! (c-ar)
  (dotimes (i (array-dimension c-ar 0))
    (setf (aref c-ar i)
          (realpart (aref c-ar i)))))

@export
(defun rand-bytes! (ar)
  (dotimes (i (array-dimension ar 0))
    (setf (aref ar i)
          (random 255))))

@export
(defun next-power-of-two (n)
  (let ((power (ceiling
                (/ (float (log n))
                   (float (log 2))))))
    (values
     (expt 2
           power)
     power)))


;; grab from buffer into power-of-two sized array
@export
(defun zero-pad-and-copy (src-ar start-i end-i &optional loud-p)
  ;; TODO: this would be a good place to apply highpass fir filter
  (if (<= end-i start-i)
      (progn
        (format t "--- zero-pad-and-copy: !!! rejecting slice of length ~a (~a to ~a)~%"
                (- end-i start-i) start-i end-i))
      (progn
        (let* ((src-len (- end-i start-i))
               (next-power-of-two
                 (expt 2 (ceiling (/ (float (log src-len)) (float (log 2))))))
               (return-ar (make-array next-power-of-two :initial-element 0.0)))

          (when loud-p
            (format t "--- zero-pad-and-copy: size requested is ~a samples (~a to ~a), padding to ~a.~%"
                    src-len start-i end-i next-power-of-two))

          (loop for i from 0 below src-len
                do
                   (progn
                     (setf (aref return-ar i)
                           (aref src-ar (+ start-i i)))

                     ;; TODO: this doesnt appear needed
                     (if (string= (type-of (aref src-ar (+ start-i i)))
                                  "REAL")
                         (format t "wtf: ~a~%" (aref src-ar (+ start-i i))))))
          return-ar))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; future: float divisor (weighted means)

@export
(defun mean-downsampler (div-n in-ar &optional out-ar debug-p)
  (assert (> div-n 1))

  (let* ((len-in-ar (array-dimension in-ar 0))
         (len-out-ar (floor (/ len-in-ar div-n)))
         (trim-to-fit (mod len-in-ar div-n)))

    (when debug-p
      (format t "- in ar has ~a samples.~%" len-in-ar)
      (format t "- out ar should have ~a samples (trimming ~a of input) ~%"
              len-out-ar trim-to-fit))

    (if out-ar
        (let ((len-passed-out-ar (array-dimension out-ar 0)))
          (when debug-p
            (format t "-- out-ar passed in: ~a~%" (type-of out-ar)))

          ;; the t case is that they are exactly equal
          (cond
            ((< len-passed-out-ar len-out-ar)
             (when debug-p
               (format
                t
                "-- out-ar passed in is smaller than it could be, truncating! ~a vs ~a~%"
                len-passed-out-ar len-out-ar))

               (setf len-out-ar len-passed-out-ar))
          ((> len-passed-out-ar len-out-ar)
           (when debug-p
             (format t "-- out-ar passed larger than needed! ~a vs ~a~%"
                     len-passed-out-ar len-out-ar)))))
        (setf out-ar
              (make-array len-out-ar :initial-element 0.0)))

    ;; for each out-ar ind
    (loop for out-i from 0 below len-out-ar
          do
             (let* ((in-ar-idx (* div-n out-i))
                    (ind-list (loop for i from in-ar-idx
                                      below (+ in-ar-idx div-n)
                                    collecting i))
                    (sum
                      (apply
                       #'+
                       (mapcar
                        (lambda (i)
                          (aref in-ar i))
                        ind-list)))
                    (new-val (/ sum div-n)))

               (when debug-p
                 (format t "-- ind-list ~a~%" ind-list)
                 (format t "-- sum ~a~%" sum)
                 (format t "-- new-val ~a~%" new-val))

               (setf (aref out-ar out-i)
                     new-val)))
    out-ar))


#|


(defun integer-downsample (in-ar n)
;; returns new ar of size in-ar / n

(assert (integerp n))

(let* ((in-ar-len (array-dimension in-ar 0))
(out-ar-len (floor (/ in-ar-len n)))
(out-ar (make-array out-ar-len
:initial-element (aref in-ar 0))))
(dotimes (i out-ar-len)
(setf (aref out-ar i)
(avg-of-samples in-ar (* i n) n)))
(values
out-ar
out-ar-len)))

CL-USER> (avg-of-samples #(3.0 4.0 5.0 6.0 7.0 8.0) 0 2)
3.5
CL-USER> (avg-of-samples #(3.0 4.0 5.0 6.0 7.0 8.0) 0 3)
4.0
CL-USER> (avg-of-samples #(3.0 4.0 5.0 6.0 7.0 8.0) 1 3)
5.0
CL-USER> (avg-of-samples #(3.0 4.0 5.0 6.0 7.0 8.0) 1 2)
4.5
CL-USER> (integer-downsample #(3.0 4.0 5.0 6.0 7.0 8.0) 2)
#(3.5 5.5 7.5)
3
CL-USER> (integer-downsample #(3.0 4.0 5.0 6.0 7.0 8.0) 3)
#(4.0 7.0)
2

|#


#|


;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun avg-of-samples (ar idx n)
;; ar[idx] + ar[idx+1] ... ar[idx+(n-1)]  /  n
;; simple window avg

(assert (< (+ idx (- n 1))
(array-dimension ar 0)))
(assert (integerp n))

(let ((sum (aref ar idx)))
(dotimes (i (- n 1))
(incf sum
(aref ar (+ idx i 1))))
(/ sum (float n))))

|#
