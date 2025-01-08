;; o1-pro

;;;; =============================================================================
;;;; FILE: doa-example.lisp
;;;; =============================================================================
;;;; A demonstration of translating Python array processing and DOA algorithms
;;;; (MUSIC, ESPRIT) into idiomatic Common Lisp with MAGICL.
;;;;
;;;; REQUIRES:
;;;;    - SBCL
;;;;    - MAGICL (https://github.com/rigetti/magicl)
;;;; =============================================================================

(defpackage :doa-example
  (:use :cl)
  (:import-from :magicl
                :all                   ; For convenience, import all MAGICL symbols
                :linalg               ; For eig, pinv, etc.
                :special-functions))  ; For asin, etc.

(in-package :doa-example)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun submatrix (m col-start col-end)
  "Return the submatrix of M taking all rows and columns from COL-START to COL-END (inclusive)."
  (let* ((rows (dimension m 0))
         (cols (dimension m 1))
         (num-cols (1+ (- col-end col-start)))
         (result (make-array (list rows num-cols)
                             :element-type (array-element-type m))))
    (dotimes (i rows)
      (dotimes (j num-cols)
        (setf (aref result i j)
              (aref m i (+ col-start j)))))
    result))

(defun conj-transpose (m)
  "Return the conjugate transpose of matrix M."
  (conjugate-transpose m))

(defun vector-min (vec)
  "Return the minimum value in vector VEC."
  (let ((m (aref vec 0)))
    (dotimes (i (1- (length vec)) m)
      (let ((val (aref vec (1+ i))))
        (when (< val m) (setf m val))))
    m))

(defun find-peaks (vec &key (height 0.0d0) (distance 1))
  "A naive peak-finding function that returns indices of local maxima in VEC,
   subject to a minimal peak value of HEIGHT and minimal separation DISTANCE."
  (let ((peaks '()))
    (loop for i from 1 below (1- (length vec)) do
      (let ((v0 (aref vec (1- i)))
            (v1 (aref vec i))
            (v2 (aref vec (1+ i))))
        (when (and (> v1 height)
                   (> v1 v0)
                   (> v1 v2))
          (push i peaks))))
    ;; Very naive approach to enforce `distance`: just filter out
    ;; peaks that are within DISTANCE of an already accepted peak.
    (let ((final-peaks '()))
      (dolist (pk (reverse peaks))  ; process in ascending order
        (when (every (lambda (accepted)
                       (>= (abs (- pk accepted)) distance))
                     final-peaks)
          (push pk final-peaks)))
      (reverse final-peaks))))

(defun random-complex-gaussian (&optional (std-dev 1.0d0))
  "Return a single complex random sample with real and imaginary parts ~ N(0, STD-DEV^2)."
  (let ((r (random-normal std-dev))
        (i (random-normal std-dev)))
    (complex r i)))

(defun random-normal (&optional (std-dev 1.0d0))
  "Return a single real random sample from N(0, STD-DEV^2) using Box-Muller."
  ;; or use any other method as you like
  (let* ((u1 (random 1.0d0))
         (u2 (random 1.0d0))
         (mag (sqrt (* -2.0d0 (log u1)))))
    (* std-dev mag (cos (* 2 pi u2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Array Response Vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-response-vector (array theta)
  "Compute the array response vector for a uniform linear array at angle THETA.
   ARRAY is a 1D vector of element positions, THETA is a real angle (radians)."
  (let* ((n (length array))
         ;; We'll compute exp(i * 2*pi * array * sin(theta))
         (pos-sin-theta (multiply array (sin theta)))  ; elementwise
         (phase (multiply #C(0d0 2d0 pi) pos-sin-theta)) ; i*(2*pi*...) for each element
         (v (exp phase)))
    (scale (/ 1d0 (sqrt n)) v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MUSIC Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun music (covmat l n array angles)
  "Perform the MUSIC algorithm.

   - COVMAT: NxN signal covariance matrix
   - L: number of sources
   - N: number of array elements
   - ARRAY: position of antenna elements (length N)
   - ANGLES: 1D vector of candidate angles (in radians)

   Returns two values:
     1) indices of the discovered DoAs in the ANGLES vector
     2) the raw pseudo-spectrum."
  (multiple-value-bind (vals vecs)
      (eig covmat :values t :vectors t)
    ;; Qn is the noise subspace spanning the columns from L to N-1
    (let* ((qn (submatrix vecs l (1- n)))
           (num-angles (length angles))
           (pspectrum (make-array num-angles :element-type 'double-float)))
      (dotimes (i num-angles)
        (let* ((av (array-response-vector array (aref angles i)))
               ;; Qn^H is the conjugate transpose
               (den (norm (mmul (conj-transpose qn) av))))
          (setf (aref pspectrum i)
                (/ 1d0 den))))
      ;; Convert to log10( 10 * pspectrum / min(pspectrum) )
      (let* ((p-min (vector-min pspectrum))
             (psindb (make-array num-angles :element-type 'double-float)))
        (dotimes (i num-angles)
          (setf (aref psindb i)
                (log10 (* 10d0 (/ (aref pspectrum i) p-min))))))
        ;; find peaks
        (let ((doas-music (find-peaks psindb :height 1.35d0 :distance 2)))
          (values doas-music psindb)))))  ; Return indices + pseudo-spectrum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ESPRIT Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esprit (covmat l n)
  "Perform the ESPRIT algorithm.

   - COVMAT: NxN signal covariance matrix
   - L: number of sources
   - N: number of array elements

   Returns a vector of DoAs (in radians)."
  (multiple-value-bind (vals u)
      (eig covmat :values t :vectors t)
    ;; S is the signal subspace from column 0..(L-1)
    (let* ((s (submatrix u 0 (1- l))))
      ;; Split into two subarrays [0..N-2], [1..N-1]
      ;; Magicl indexing: row dimension is dimension 0
      ;; We'll do row slices to simulate the shift:
      (let* ((s1 (submatrix s 0 (1- l)))           ; all rows
             (s2 (submatrix s 0 (1- l)))           ; will fix row slicing
             ;; Actually we must drop the last row from s1 and first row from s2
             ;; to replicate S[0:N-1], S[1:N] in Python
             )
        (setf s1 (slice s1 :rows (list 0 (1- (dimension s1 0)))) :columns :all)
        (setf s2 (slice s2 :rows (list 1 (1- (dimension s2 0)))) :columns :all)
        ;; Now compute Phi = pinv(s1) * s2
        (let* ((phi (mmul (pinv s1) s2))
               (phi-vals (eig phi :values t :vectors nil))) ; eigenvalues only
          ;; DoAs = arcsin( angle(eigenvalues)/ pi )
          (let ((angles (make-array l :element-type 'double-float)))
            (dotimes (i l)
              (let* ((eigv (aref (first phi-vals) i)) ; a complex number
                     (ph (phase eigv))                ; argument of a complex
                     (asin-val (asin (/ ph pi))))
                (setf (aref angles i) asin-val)))
            angles))))))

(defun phase (z)
  "Return the argument (angle) of the complex number Z in the range (-pi, pi]."
  (atan (imagpart z) (realpart z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Demonstration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  "Main driver that replicates the rough steps from the Python demonstration."
  ;; For reproducibility; seed the RNG
  (setf *random-state* (make-random-state 6))

  (let* ((lambda 1.0d0)
         (kappa (/ pi lambda))
         (l 5)                       ; number of sources
         (n 32)                      ; number of array elements
         (snr 10.0d0)               ; SNR
         ;; Our ULA positions: 0..(N-1)/2
         (array (linspace 0.0d0 (/ (1- n) 2.0d0) n))
         ;; Random directions in [-pi/2, +pi/2], but the Python code picks them
         ;; from np.pi*(np.random.rand(L)-1/2)
         (thetas (make-array l :element-type 'double-float))
         (alphas (make-array l :element-type 'cl:complex-double-float))
         ;; angles grid for scanning
         (angles (linspace (- (/ pi 2.0d0)) (/ pi 2.0d0) 360)))

    ;; Generate random Thetas and Alphas (source directions and powers)
    (dotimes (i l)
      (setf (aref thetas i) (* pi (- (random 1.0d0) 0.5d0)))
      ;; random complex Gaussian, scaled by sqrt(1/2)
      (setf (aref alphas i) (scale (sqrt 0.5d0) (random-complex-gaussian))))

    ;; Construct h = sum_{i=1..L} Alphas[i] * a(array, Thetas[i])
    (let ((h (make-array n :initial-element 0d0 :element-type 'complex-double-float)))
      (dotimes (i l)
        (let ((avi (array-response-vector array (aref thetas i))))
          (v+ h (scale (aref alphas i) avi) :in-place t)))

      ;; Just as in Python, we can measure correlation w.r.t. the angles
      (let ((hv (make-array (length angles) :element-type 'double-float)))
        (dotimes (j (length angles))
          (let ((av (array-response-vector array (aref angles j))))
            (setf (aref hv j) (abs (inner-product h (conjugate av))))))
        ;; We can also measure powers at the actual Thetas
        (let ((powers (make-array l :element-type 'double-float)))
          (dotimes (j l)
            (let ((av (array-response-vector array (aref thetas j))))
              (setf (aref powers j) (abs (inner-product h (conjugate av))))))

          ;; Now replicate the random realizations part:
          (let* ((num-realization 100)
                 (H (make-array (list n num-realization)
                                :element-type 'complex-double-float)))
            (dotimes (iter num-realization)
              (let ((htmp (make-array n :initial-element 0d0
                                      :element-type 'complex-double-float)))
                (dotimes (i l)
                  (let ((pha (exp (complex 0d0 (* 2d0 pi (random 1.0d0))))))
                    (v+ htmp (scale (* pha (aref alphas i))
                                    (array-response-vector array (aref thetas i)))
                         :in-place t)))
                ;; Add complex Gaussian noise
                (dotimes (i2 n)
                  (incf (aref htmp i2)
                        (scale (sqrt (/ 0.5d0 snr)) (random-complex-gaussian))))
                ;; Place in the matrix
                (dotimes (rr n)
                  (setf (aref H rr iter) (aref htmp rr)))))

            ;; CovMat = H * H^H
            (let* ((H-H (conjugate-transpose H))
                   (covmat (mmul H H-H)))
              ;; --- MUSIC ---
              (multiple-value-bind (doas-music psindb)
                  (music covmat l n array angles)
                ;; Indices in DOAS-MUSIC are the positions in the ANGLES array
                (let ((estimated-doas-music
                        (map 'list (lambda (idx) (aref angles idx)) doas-music)))

                  ;; --- ESPRIT ---
                  (let ((doas-esprit (esprit covmat l n)))
                    ;; Print results
                    (format t "Actual DoAs: ~a~%" (sort (coerce thetas 'list) #'<))
                    (format t "MUSIC DoAs (estimated): ~a~%"
                            (sort estimated-doas-music #'<))
                    (format t "ESPRIT DoAs (estimated): ~a~%"
                            (sort (coerce doas-esprit 'list) #'<))
                    ;; For a real system, we might want to do actual plotting here
                    ;; using external libraries. We'll just finish gracefully.
                    (values)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple LINSPACE helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linspace (start end count)
  "Return a vector of COUNT linearly spaced values from START to END inclusive."
  (let ((v (make-array count :element-type 'double-float)))
    (dotimes (i count)
      (setf (aref v i) (lerp start end (/ (float i) (1- count)))))
    v))

(defun lerp (a b t)
  "Linear interpolation between A and B with parameter T in [0,1]."
  (+ a (* t (- b a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN: Evaluate (main) at the REPL or load the file to replicate the steps.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
