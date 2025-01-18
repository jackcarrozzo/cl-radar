(in-package :cl-user)
(defpackage cl-radar.gen
  (:use :cl
   :cl-radar.util
   :cl-radar.math))
(in-package :cl-radar.gen)
(cl-syntax:use-syntax :annot)

;; code that was at least partially generated - useful but suspect

#|
CL-USER> (let ((antenna1 (vector #C(0 0) #C(1 1) #C(2 2)))
(antenna2 (vector #C(0 0) #C(1 2) #C(2 3)))
(antenna3 (vector #C(0 0) #C(2 1) #C(3 2))))
;; Simulate a 3-antenna array. We'll steer the beam to angle = 0.2 radians.
(cl-radar.gen::beam-angle-sum (list antenna1 antenna2 antenna3) 0.2d0
:element-spacing 0.05d0
:carrier-freq 77e9
:speed-of-light 3e8))
#(#C(0.0d0 0.0d0) #C(1.701745496094935d0 0.7689853574759731d0)
#C(2.284890037712121d0 1.9062141893507523d0))
|#
@export
(defun beam-angle-sum (antenna-data angle
                       &key
                         (element-spacing 0.5d0)         ; meters between elements
                         (carrier-freq 24.125e9)         ; Hz
                         (speed-of-light 3.0d8))
  "
BEAM-SUM applies a phase shift to each antenna's complex samples to 'steer'
the beam to the specified ANGLE (radians) and sums them into a single array.

ARGUMENTS:
  - ANTENNA-DATA: A list of complex-valued arrays (vectors), one per antenna.
  - ANGLE: Steering angle in radians relative to broadside (0.0 = orthogonal).

KEY PARAMETERS (keyword args):
  - ELEMENT-SPACING: Distance (meters) between adjacent antenna elements.
  - CARRIER-FREQ: Center frequency in Hz.
  - SPEED-OF-LIGHT: Wave propagation speed (m/s).

RETURNS:
  A one-dimensional vector of complex values, whose length = length of
  the per-antenna input vectors, containing the phase-aligned sum of signals.
"
  (let* ((num-antennas (length antenna-data))
         ;; We assume all antenna arrays have the same length:
         (num-samples (length (first antenna-data)))
         ;; Allocate the result array (one complex sample per time index)
         (result (make-array num-samples
                             :initial-element #C(0.0d0 0.0d0)))
         ;; Compute wave number k = 2π / λ = 2π * f / c
         (two-pi 6.283185307179586d0)
         (wave-number (/ (* two-pi carrier-freq)
                         speed-of-light)))
    ;; For each sample index in 0..(num-samples - 1)
    (dotimes (s num-samples)
      (let ((sum-sample 0d0))  ; Accumulate complex sum across antennas
        ;; Sum over all antennas
        (dotimes (a num-antennas)
          (let* ((antenna-array (nth a antenna-data))
                 (original-sample (aref antenna-array s))
                 (phase-shift (* wave-number a element-spacing (sin angle)))
                 ;; Multiply by e^{j * phase-shift}
                 (phaser (complex (cos phase-shift)
                                  (sin phase-shift)))
                 (rotated-sample (* original-sample phaser)))
            (incf sum-sample rotated-sample)))
        ;; Store the final sum in the result
        (setf (aref result s) sum-sample)))
    ;; Return the summed array
    result))

@export
(defun beam-angle-sum-2d-array
    (samples angle-deg
             &key
               (element-spacing 0.03d0)       ; m
               (wavelength 0.1d0))            ; m
  "Perform beam summation on a 2D array of complex SAMPLES at the given
ARRIVAL-ANGLE (ANGLE-DEG in degrees) assuming a linear antenna array.

The 2D array SAMPLES is indexed as:
  (aref samples antenna-index time-index)

ELEMENT-SPACING is the distance between antenna elements.
WAVELENGTH is the signal wavelength.  Defaults assume normalized spacing=1,
wavelength=1 for demonstration.

Returns a 1D array of complex samples representing the beamformed output
at the specified arrival angle."
  (let* ((num-antennas (array-dimension samples 0))
         (num-time-samples (array-dimension samples 1))
         ;; Convert angle from degrees to radians:
         (theta-rad (* angle-deg Pi (/ 1.0d0 180.0d0)))
         ;; Wave number k = 2π / λ
         (k (/ (* 2.0d0 Pi) wavelength))
         ;; Prepare output array
         (output (make-array num-time-samples
                             :initial-element #C(0.0d0 0.0d0))))
    ;; For each time sample, sum across antennas with the appropriate phase shift
    (dotimes (ts num-time-samples)
      (let ((sum 0d0))  ;; Accumulate a complex sum at this time index
        (dotimes (ant num-antennas)
          ;; Phase shift for antenna element 'ant':
          ;;
          ;;     exp(-j * k * d * ant * sin(theta))
          ;;
          (let ((phase (exp (complex 0
                                     (- (* k element-spacing ant
                                           (sin theta-rad)))))))
            (incf sum (* (aref samples ant ts) phase))))
        (setf (aref output ts) sum)))
    output))



#|
CL-USER> (let* ((samples #( #C(1d0 0d0)  ; e.g. 1 + j0
#C(0d0 1d0)  ;      0 + j1
#C(1d0 1d0) )) ;    1 + j1
(phi (/ pi 4)))           ; phase increment: π/4 per index
(cl-radar.gen::rotating-phase-shift samples phi))
#(#C(1.0d0 0.0d0) #C(0.7071067811865475d0 0.7071067811865476d0)
#C(1.0d0 -0.9999999999999999d0))
|#

@export
(defun rotating-phase-shift (samples phi)
  "Return a new array of SAMPLES where each element is multiplied by e^{-j * n * PHI}.
   SAMPLES is a vector of complex numbers.
   PHI is the per-element phase shift (in radians)."
  (let* ((len (length samples))
         ;; We'll store the rotated samples in a new vector:
         (result (make-array len :initial-element #C(0.0d0 0.0d0))))
    (loop for n from 0 below len do
      (let* ((s   (aref samples n))
             ;; Compute e^{-j * n * phi} = cos(n*phi) - j sin(n*phi):
             (rot (complex (cos (* n phi))
                           (- (sin (* n phi))))))
        ;; Multiply the original sample by the rotation:
        (setf (aref result n) (* s rot))))
    result))

#|
CL-USER> (cl-radar.gen::fractional-interpolating-delay '#(#C(0.0 0.0) #C(1.0 0.1) #C(2.0 0.2) #C(3.0 0.3)) 0.1)
#(#C(0.0d0 0.0d0) #C(0.9 0.089999996) #C(1.9 0.19) #C(2.9 0.29000002))
CL-USER> (cl-radar.gen::fractional-interpolating-delay '#(#C(0.0 0.0) #C(1.0 0.1) #C(2.0 0.2) #C(3.0 0.3)) 0.5)
#(#C(0.0d0 0.0d0) #C(0.5 0.05) #C(1.5 0.15) #C(2.5 0.25))
CL-USER> (cl-radar.gen::fractional-interpolating-delay '#(#C(0.0 0.0) #C(1.0 0.1) #C(2.0 0.2) #C(3.0 0.3)) 0.8)
#(#C(0.0d0 0.0d0) #C(0.19999999 0.02) #C(1.2 0.120000005) #C(2.2 0.22000001))
|#

@export
(defun fractional-interpolating-delay (samples delay)
  "
Return a new array of the same length as SAMPLES, where each output
sample is the input delayed by DELAY (a positive fractional number
less than one sample period). Uses simple linear interpolation.

Arguments:
  - SAMPLES: A vector (array) of complex (or real) time-domain samples.
  - DELAY: Fractional delay in samples, 0 < DELAY < 1.

Returns:
  A vector of the same length as SAMPLES, containing the time-shifted data.
"
  (assert (and (floatp delay) (> delay 0) (< delay 1))
          (delay)
          "DELAY (~A) must be between 0 and 1" delay)
  (let* ((n-samples (length samples))
         ;; Output array
         (result (make-array n-samples
                             :initial-element #C(0.0d0 0.0d0))))

    (labels ((safe-sample (idx)
               "Return sample from SAMPLES if in range, else 0."
               (if (or (< idx 0) (>= idx n-samples))
                   0d0
                   (aref samples idx))))

      ;; For each integer time index n, we want y[n] = x[n - delay].
      ;; We'll do linear interpolation around floor(n - delay).
      (dotimes (n n-samples)
        (let* ((float-index (- n delay))
               (i (floor float-index))  ; integer index
               (alpha (- float-index i))) ; fractional part
          ;; Weighted combination of x[i] and x[i+1].
          (setf (aref result n)
                (let ((val1 (safe-sample i))
                      (val2 (safe-sample (1+ i))))
                  (complex
                   (+ (* (- 1 alpha) (realpart val1))
                      (* alpha (realpart val2)))
                   (+ (* (- 1 alpha) (imagpart val1))
                      (* alpha (imagpart val2)))))))))
    result))


;;;;;;;;;

;;;; ================================================================
;;;; Minimal Matrix Utilities
;;;; ================================================================

(defun make-matrix (rows cols &optional (initial-value 0.0))
  "Create a 2D array of size ROWS x COLS, filled with INITIAL-VALUE."
  (make-array (list rows cols) :element-type 'double-float
                              :initial-element (coerce initial-value 'double-float)))

(defun matrix-rows (mat)
  (array-dimension mat 0))

(defun matrix-cols (mat)
  (array-dimension mat 1))

(defun matrix-transpose (mat)
  "Return the transpose of MAT."
  (let* ((r (matrix-rows mat))
         (c (matrix-cols mat))
         (res (make-matrix c r)))
    (dotimes (i r)
      (dotimes (j c)
        (setf (aref res j i) (aref mat i j))))
    res))

(defun matrix-multiply (a b)
  "Return the product A*B. A is (r x n), B is (n x c)."
  (let* ((r (matrix-rows a))
         (n (matrix-cols a))
         (n2 (matrix-rows b))
         (c (matrix-cols b)))
    (unless (= n n2)
      (error "Incompatible dimensions in matrix-multiply: (~A x ~A) * (~A x ~A)"
             r n n2 c))
    (let ((result (make-matrix r c 0.0)))
      (dotimes (i r)
        (dotimes (j c)
          (let ((sum 0.0))
            (dotimes (k n)
              (incf sum (* (aref a i k) (aref b k j))))
            (setf (aref result i j) sum))))
      result)))

(defun matrix-scale (mat scalar)
  "Scale every entry of MAT by SCALAR in place."
  (dotimes (i (matrix-rows mat))
    (dotimes (j (matrix-cols mat))
      (setf (aref mat i j) (* scalar (aref mat i j)))))
  mat)

(defun matrix-add! (a b)
  "In-place add B into A (A += B). Returns A."
  (let* ((r (matrix-rows a))
         (c (matrix-cols a)))
    (unless (and (= r (matrix-rows b))
                 (= c (matrix-cols b)))
      (error "Incompatible dimensions in matrix-add!: (~A x ~A) + (~A x ~A)"
             r c (matrix-rows b) (matrix-cols b)))
    (dotimes (i r)
      (dotimes (j c)
        (incf (aref a i j) (aref b i j))))
    a))

(defun copy-matrix (mat)
  "Return a copy of MAT."
  (let* ((r (matrix-rows mat))
         (c (matrix-cols mat))
         (res (make-matrix r c)))
    (dotimes (i r)
      (dotimes (j c)
        (setf (aref res i j) (aref mat i j))))
    res))

(defun identity-matrix (n)
  "Return an N x N identity matrix."
  (let ((id (make-matrix n n 0d0)))
    (dotimes (i n)
      (setf (aref id i i) 1d0))
    id))


;;;; ================================================================
;;;; Jacobi Eigen-decomposition for real symmetric matrices
;;;; ================================================================
(defun jacobi-rotate (a p q angle)
  "Perform a Jacobi rotation on matrix A in rows/cols p and q with ANGLE."
  (let* ((cos-angle (cos angle))
         (sin-angle (sin angle))
         (app (aref a p p))
         (aqq (aref a q q))
         (apq (aref a p q))
         ;; Diagonal elements after rotation
         (app-new (- (* app cos-angle cos-angle)
                     (* 2 apq sin-angle cos-angle)
                     (* aqq sin-angle sin-angle)))
         (aqq-new (- (* aqq cos-angle cos-angle)
                     (* -2 apq sin-angle cos-angle)
                     (* app sin-angle sin-angle))))
    ;; Update diagonal elements
    (setf (aref a p p) app-new)
    (setf (aref a q q) aqq-new)
    ;; Off-diagonal becomes 0
    (setf (aref a p q) 0d0)
    (setf (aref a q p) 0d0)

    ;; Update other elements
    (dotimes (r (matrix-rows a))
      (unless (or (= r p) (= r q))
        (let ((arp (aref a r p))
              (arq (aref a r q)))
          (setf (aref a r p)
                (- (* arp cos-angle) (* arq sin-angle)))
          (setf (aref a p r) (aref a r p)) ;; symmetric

          (setf (aref a r q)
                (+ (* arp sin-angle) (* arq cos-angle)))
          (setf (aref a q r) (aref a r q))))))
  a)

(defun jacobi-eigen-decomposition (mat &key (max-iter 1000) (tol 1e-10))
  "Compute eigenvalues and eigenvectors of real symmetric MAT using Jacobi.
Returns a list (evalues evectors), where
 evalues is a list of eigenvalues,
 evectors is a matrix whose columns are the corresponding eigenvectors."
  (let* ((n (matrix-rows mat))
         (a (copy-matrix mat))   ;; we'll destroy 'a' in-place
         (v (identity-matrix n)))
    (unless (= n (matrix-cols a))
      (error "Matrix for Jacobi must be square."))
    (loop
      for iter-count upfrom 1 to max-iter do
        (let ((p -1)
              (q -1)
              (max-offdiag 0.0))
          ;; Find largest off-diagonal element
          (dotimes (i n)
            (dotimes (j i)
              (let ((val (abs (aref a i j))))
                (when (> val max-offdiag)
                  (setf max-offdiag val
                        p i
                        q j)))))
          (if (< max-offdiag tol)
              (return))  ;; Converged

          ;; Compute the Jacobi rotation angle
          (let ((app (aref a p p))
                (aqq (aref a q q))
                (apq (aref a p q)))
            (let ((theta (/ (- aqq app)
                            (* 2 apq))))
              (let ((tm (if (> theta 0)
                           (/ 1.0 (+ (abs theta) (sqrt (+ 1.0 (* theta theta)))))
                           (/ -1.0 (+ (abs theta) (sqrt (+ 1.0 (* theta theta)))))))
                    (angle 0.0))
                (setf angle (atan tm))
                ;; Rotate 'a'
                (jacobi-rotate a p q angle)
                ;; Update eigenvector matrix 'v'
                (dotimes (r n)
                  (let ((vrp (aref v r p))
                        (vrq (aref v r q)))
                    (setf (aref v r p) (- (* vrp (cos angle))
                                          (* vrq (sin angle))))
                    (setf (aref v r q) (+ (* vrp (sin angle))
                                          (* vrq (cos angle)))))))))))
    ;; Extract eigenvalues and reorder
    (let ((evalues (make-array n :initial-element 0.0))
          (evectors v))
      (dotimes (i n)
        (setf (aref evalues i) (aref a i i)))

      ;; We want to return them sorted by descending eigenvalue.
      (let ((idxs (sort (loop for i from 0 to (1- n) collect i)
                         #'>
                         :key (lambda (i) (aref evalues i)))))
        (values
         (map 'list (lambda (i) (aref evalues i)) idxs)
         (let ((sorted-v (make-matrix n n)))
           (dotimes (col n)
             (dotimes (row n)
               (setf (aref sorted-v row col) (aref evectors row (nth col idxs)))))
           sorted-v))))))


;;;; ================================================================
;;;; MUSIC Algorithm
;;;; ================================================================
(defun build-data-matrix (samples m)
  "Build an M x (N - M + 1) data matrix from 1D SAMPLES,
   where each row is a shifted version of the data."
  (let* ((n (length samples))
         (num-cols (- n m 1)))
    (when (< num-cols 1)
      (error "Not enough samples ~A to build data matrix with dimension M=~A"
             n m))
    (let ((X (make-matrix m num-cols)))
      (dotimes (col num-cols)
        (dotimes (row m)
          (setf (aref X row col)
                (elt samples (+ col row)))))  ;; shift
      X)))

(defun correlation-matrix (samples m)
  "Compute MxM correlation matrix from SAMPLES using a sliding window of size M."
  (let* ((X (build-data-matrix samples m))
         (Xt (matrix-transpose X))
         ;; R = (1/(num-cols)) * X * X^T
         (num-cols (matrix-cols X)))
    (let ((R (matrix-multiply X Xt)))
      (matrix-scale R (/ 1.0 num-cols))
      R)))


(defun compute-pseudospectrum (noise-subspace freq sample-rate)
  "Compute the MUSIC pseudospectrum for a given frequency FREQ (Hz).
   NOISE-SUBSPACE is an M x (M-K) matrix (the eigenvectors corresponding to
   the smallest M-K eigenvalues)."
  ;; We treat the steering vector as complex.  For real data, you can
  ;; consider an alternate approach or simply treat it as complex with
  ;; zero imaginary part in the data.
  (let* ((m (matrix-rows noise-subspace))
         (steer (make-array m :initial-element #C(0.0d0 0.0d0)))
         (omega (* 2d0 pi freq (/ 1.0 sample-rate))))
    (dotimes (i m)
      ;; a(i) = e^(-j * omega * i)
      (setf (aref steer i)
            (exp (* #C(0.0 -1.0) omega i))))
    ;; Compute a^H * E_n
    ;; We'll do it by explicit multiplication:
    (let ((aHEn (make-array (matrix-cols noise-subspace)
                            :initial-element #C(0d0 0d0))))
      (dotimes (col (matrix-cols noise-subspace))
        (dotimes (row m)
          (incf (aref aHEn col)
                (* (conjugate (aref steer row))
                   (aref noise-subspace row col)))))
      ;; Now compute (a^H * E_n) (E_n^H * a) = |a^H E_n|^2
      ;; E_n^H * a would be the complex conjugate of aHEn
      (let ((mag-sq 0d0))
        (dotimes (col2 (matrix-cols noise-subspace))
          (let ((val (aref aHEn col2)))
            (incf mag-sq (* val (conjugate val)))))
        ;; P(f) = 1 / mag-sq
        (if (zerop mag-sq)
            1e15  ; guard against divide by zero
            (/ 1d0 mag-sq))))))

(defun find-k-largest-local-maxima (values k)
  "Return indices of the K largest local maxima in VALUES (1D array).
   A local maximum is defined in a simple sense: value[i] > value[i-1] and value[i] > value[i+1].
   We do a naive approach here; you can refine as needed."
  (let (candidates)
    (dotimes (i (- (length values) 2))
      (let ((i1 (1+ i)))
        (when (and (> (aref values i1) (aref values i))
                   (> (aref values i1) (aref values (+ i1 1))))
          (push (cons (aref values i1) i1) candidates))))
    ;; Now sort candidates by the pseudospectrum value, descending
    (setf candidates (sort candidates #'> :key #'car))
    ;; Return the top K indices
    (mapcar #'cdr (subseq candidates 0 (min k (length candidates))))))

(defun music-frequencies (samples sample-rate k
                                  &key (subspace-size nil)
                                       (num-freq-points 1024))
  "Apply the MUSIC algorithm to real 1D SAMPLES at SAMPLE-RATE,
   searching for K signals. Return a list of (peak-value frequency)."
  (format t "samples is type: ~a~%" (type-of samples))
  (let* ((n (length samples))
         ;; If not specified, choose subspace-size somewhat larger than k.
         ;; The user might tune this.  Must be <= n/2 realistically.
         (m (or subspace-size
                (max (+ k 5) (floor (/ n 2)))))
         ;; 1) Build correlation matrix
         (R (correlation-matrix samples m))
         ;; 2) Eigen-decompose R
         (evalues nil)
         (evectors nil) ;; multiple values (???)
         (evalues+evectors (jacobi-eigen-decomposition R))) ;; ???
    (setf evalues (first evalues+evectors)) ;; ??? insane
    (setf evectors (second evalues+evectors))
    ;; evalues is sorted in descending order, evectors has columns of sorted eigenvectors
    ;; 3) Signal subspace dimension = k => noise subspace dimension = m - k
    ;;    The last (m-k) columns of evectors correspond to the smallest eigenvalues => noise subspace
    (let ((noise-subspace (make-matrix m (- m k))))
      (dotimes (col (- m k))
        (dotimes (row m)
          (setf (aref noise-subspace row col)
                (aref evectors row (+ k col)))))  ;; columns after k
      ;; 4) Compute MUSIC pseudospectrum over a frequency grid
      (let ((p (make-array num-freq-points :element-type 'double-float)))
        (dotimes (i num-freq-points)
          (let ((freq (* (/ (float i) num-freq-points) (/ sample-rate 2.0))))
            (setf (aref p i)
                  (coerce (compute-pseudospectrum noise-subspace freq sample-rate)
                          'double-float))))
        ;; 5) Find the top K peaks
        (let ((peak-indices (find-k-largest-local-maxima p k))
              (results '()))
          (dolist (idx peak-indices)
            (let* ((freq (* (/ (float idx) num-freq-points) (/ sample-rate 2.0)))
                   (val (aref p idx)))
              (push (list val freq) results)))
          ;; Sort final results by descending peak value
          (sort results #'> :key #'first))))))

;; Example top-level function that just returns the peaks
(defun estimate-music-peaks (samples sample-rate k
                                     &key (subspace-size nil)
                                          (num-freq-points 1024))
  "Return the list of (peak-magnitude frequency-Hz) for the top K signals
   estimated by MUSIC from SAMPLES at SAMPLE-RATE."
  (music-frequencies samples sample-rate k
                     :subspace-size subspace-size
                     :num-freq-points num-freq-points))
