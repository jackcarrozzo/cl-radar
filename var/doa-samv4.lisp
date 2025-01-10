;;;; samv-doa.lisp
;;;; A robust SAMV-based DoA estimation pipeline in Common Lisp,
;;;; using '1d-9' (etc.), loop-based reverse iteration, and carefully balanced parentheses.

(defvar +debug-mode+ t
  "If T, you could disable heavy optimization or add safety/debug checks.
   If NIL, we declare speed-optimized settings with minimal runtime checks.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless +debug-mode+
    ;; If not in debug mode, use speed-optimized declarations
    (declaim (optimize (speed 3) (safety 0) (debug 0)))))

(defpackage :samv-doa
  (:use :cl))
(in-package :samv-doa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1) Utility Functions: Complex, Matrix, and Array Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conj* (z)
  "Return the complex conjugate of z."
  (declare (type complex z))
  (complex (realpart z) (- (imagpart z))))

(defun cabs2 (z)
  "Return the magnitude squared of a complex number z."
  (declare (type complex z))
  (let ((r (realpart z))
        (i (imagpart z)))
    (declare (type double-float r i))
    (+ (* r r) (* i i))))

(defun make-cmatrix (num-rows num-cols &key (initial-element #C(0.0d0 0.0d0)))
  "Create a NUM-ROWS x NUM-COLS 2D array of complex-like data.
   We do NOT rely on :element-type 'complex' (which can cause issues in SBCL)."
  (declare (type (integer 1 *) num-rows num-cols))
  (make-array (list num-rows num-cols)
              :initial-element initial-element))

(defun cmatrix-transpose (mat)
  "Return the transpose of MAT (no conjugation)."
  (declare (type (array t (* *)) mat))
  (let* ((row-count (array-dimension mat 0))
         (col-count (array-dimension mat 1))
         (res (make-cmatrix col-count row-count)))
    (dotimes (row-idx row-count)
      (dotimes (col-idx col-count)
        (setf (aref res col-idx row-idx) (aref mat row-idx col-idx))))
    res))

(defun cmatrix-hermitian (mat)
  "Return the Hermitian (conjugate transpose) of MAT."
  (declare (type (array t (* *)) mat))
  (let* ((row-count (array-dimension mat 0))
         (col-count (array-dimension mat 1))
         (res (make-cmatrix col-count row-count)))
    (dotimes (row-idx row-count)
      (dotimes (col-idx col-count)
        (setf (aref res col-idx row-idx)
              (conj* (aref mat row-idx col-idx)))))
    res))

(defun cmatrix-mult (mat-a mat-b)
  "Multiply two complex matrices MAT-A (MxN) and MAT-B (NxP) => MxP."
  (declare (type (array t (* *)) mat-a mat-b))
  (let* ((row-count-a (array-dimension mat-a 0))
         (col-count-a (array-dimension mat-a 1))
         (row-count-b (array-dimension mat-b 0))
         (col-count-b (array-dimension mat-b 1)))
    (assert (= col-count-a row-count-b) ()
            "Matrix dimension mismatch in cmatrix-mult: ~A != ~A" col-count-a row-count-b)
    (let ((res (make-cmatrix row-count-a col-count-b)))
      (dotimes (row-idx row-count-a)
        (dotimes (col-idx col-count-b)
          (let ((sum #C(0.0d0 0.0d0)))
            (dotimes (mid-idx col-count-a)
              (incf sum (* (aref mat-a row-idx mid-idx)
                           (aref mat-b mid-idx col-idx))))
            (setf (aref res row-idx col-idx) sum))))
      res)))

(defun cmatrix-scale (mat scalar)
  "Scale each element of MAT by SCALAR, returning a new matrix."
  (declare (type (array t (* *)) mat)
           (type complex scalar))
  (let* ((row-count (array-dimension mat 0))
         (col-count (array-dimension mat 1))
         (res (make-cmatrix row-count col-count)))
    (dotimes (row-idx row-count)
      (dotimes (col-idx col-count)
        (setf (aref res row-idx col-idx)
              (* scalar (aref mat row-idx col-idx)))))
    res))

(defun cmatrix-add (mat-a mat-b &optional (alpha #C(1.0d0 0.0d0)))
  "Return MAT-A + alpha * MAT-B elementwise."
  (declare (type (array t (* *)) mat-a mat-b)
           (type complex alpha))
  (let* ((row-count (array-dimension mat-a 0))
         (col-count (array-dimension mat-a 1))
         (res (make-cmatrix row-count col-count)))
    (dotimes (row-idx row-count)
      (dotimes (col-idx col-count)
        (setf (aref res row-idx col-idx)
              (+ (aref mat-a row-idx col-idx)
                 (* alpha (aref mat-b row-idx col-idx))))))
    res))

(defun cmatrix-copy (mat)
  "Return a deep copy of the matrix MAT."
  (declare (type (array t (* *)) mat))
  (let* ((row-count (array-dimension mat 0))
         (col-count (array-dimension mat 1))
         (res (make-cmatrix row-count col-count)))
    (dotimes (row-idx row-count)
      (dotimes (col-idx col-count)
        (setf (aref res row-idx col-idx) (aref mat row-idx col-idx))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2) LU Inversion with Partial Pivot; Using 'loop' for Reverse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmatrix-identity (dim-size)
  "Return a dim-size x dim-size identity matrix (complex)."
  (declare (type (integer 1 *) dim-size))
  (let ((id (make-cmatrix dim-size dim-size)))
    (dotimes (idx dim-size)
      (setf (aref id idx idx) #C(1.0d0 0.0d0)))
    id))

(defun cmatrix-swap-rows! (mat row1 row2)
  "In-place swap row row1 and row2 of MAT."
  (declare (type (array t (* *)) mat)
           (type (integer 0 *) row1 row2))
  (when (/= row1 row2)
    (let ((col-count (array-dimension mat 1)))
      (dotimes (col-idx col-count)
        (rotatef (aref mat row1 col-idx)
                 (aref mat row2 col-idx)))))
  mat)

(defun cmatrix-lu-invert (mat-in)
  "Return the inverse of mat-in via naive LU factorization with partial pivot."
  (declare (type (array t (* *)) mat-in))
  (let* ((dim-size (array-dimension mat-in 0)))
    (assert (= dim-size (array-dimension mat-in 1)) ()
            "Matrix must be square for inversion.")
    (let ((mat-u (cmatrix-copy mat-in))
          (mat-l (cmatrix-identity dim-size))
          (mat-p (cmatrix-identity dim-size)))
      ;; Factor
      (dotimes (pivot-idx dim-size)
        (let ((pivot-row pivot-idx)
              (max-abs (cabs2 (aref mat-u pivot-idx pivot-idx))))
          (dotimes (offset (- dim-size pivot-idx))
            (let ((check-row (+ pivot-idx offset)))
              (when (> (cabs2 (aref mat-u check-row pivot-idx)) max-abs)
                (setf max-abs (cabs2 (aref mat-u check-row pivot-idx)))
                (setf pivot-row check-row))))
          (when (/= pivot-row pivot-idx)
            (cmatrix-swap-rows! mat-u pivot-idx pivot-row)
            (cmatrix-swap-rows! mat-l pivot-idx pivot-row)
            (cmatrix-swap-rows! mat-p pivot-idx pivot-row)
            ;; fix L below pivot row
            (dotimes (col-idx pivot-idx)
              (rotatef (aref mat-l pivot-idx col-idx)
                       (aref mat-l pivot-row col-idx))))
          (dotimes (offset (- dim-size (1+ pivot-idx)))
            (let* ((row-idx (+ pivot-idx 1 offset))
                   (alpha (/ (aref mat-u row-idx pivot-idx)
                             (aref mat-u pivot-idx pivot-idx))))
              (setf (aref mat-l row-idx pivot-idx) alpha)
              (dotimes (col-idx (- dim-size pivot-idx 1))
                (incf (aref mat-u row-idx (+ col-idx pivot-idx 1))
                      (- (* alpha (aref mat-u pivot-idx (+ col-idx pivot-idx 1))))))))))

      ;; Solve for inverse(A) = inverse(U) * inverse(L) * P
      (let ((invA (make-cmatrix dim-size dim-size)))
        (dotimes (col-idx dim-size)
          (let ((basis-col (make-cmatrix dim-size 1 :initial-element #C(0.0d0 0.0d0))))
            (setf (aref basis-col col-idx 0) #C(1.0d0 0.0d0))
            (let ((y (cmatrix-mult mat-p basis-col)))
              ;; Solve Lz = y
              (dotimes (row-idx dim-size)
                (dotimes (c2 row-idx)
                  (decf (aref y row-idx 0)
                        (* (aref mat-l row-idx c2)
                           (aref y c2 0))))
                (setf (aref y row-idx 0)
                      (/ (aref y row-idx 0)
                         (aref mat-l row-idx row-idx))))
              ;; Solve Ux = z (reverse loop via 'loop')
              (loop for r-idx from (1- dim-size) downto 0 do
                (loop for c2 from 0 below (- dim-size r-idx 1) do
                  (decf (aref y r-idx 0)
                        (* (aref mat-u r-idx (+ r-idx 1 c2))
                           (aref y (+ r-idx 1 c2) 0))))
                (setf (aref y r-idx 0)
                      (/ (aref y r-idx 0)
                         (aref mat-u r-idx r-idx))))
              ;; place result in invA
              (dotimes (row-idx dim-size)
                (setf (aref invA row-idx col-idx) (aref y row-idx 0))))))
        invA))))

(defun cmatrix-invert-regularized (mat-in &optional (reg 1d-9))
  "Invert mat-in with Tikhonov reg => (mat-in + reg*I)^-1."
  (declare (type (array t (* *)) mat-in)
           (type double-float reg))
  (let* ((dim-size (array-dimension mat-in 0)))
    (assert (= dim-size (array-dimension mat-in 1)) ()
            "Matrix must be square for inversion.")
    (let ((mat-reg (cmatrix-add mat-in (cmatrix-identity dim-size)
                                (complex reg 0.0d0))))
      (cmatrix-lu-invert mat-reg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3) Steering Vectors & Outer Product
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-steering-vector (num-antennas element-spacing wavelength angle-rad)
  "Return a 1D array of length NUM-ANTENNAS with e^{-j * phase}."
  (declare (type (integer 1 *) num-antennas)
           (type double-float element-spacing wavelength angle-rad))
  (let ((v (make-array num-antennas :initial-element #C(0.0d0 0.0d0))))
    (dotimes (antenna-idx num-antennas)
      (let ((phase (* 2d0 pi (/ element-spacing wavelength)
                     (sin angle-rad)
                     antenna-idx)))
        (setf (aref v antenna-idx)
              (complex (cos (- phase))
                       (sin (- phase))))))
    v))

(defun outer-product (vec1 vec2)
  "Compute the NxM outer product of two 1D arrays => NxM matrix."
  (declare (type (array t (*)) vec1 vec2))
  (let* ((length1 (length vec1))
         (length2 (length vec2))
         (res (make-cmatrix length1 length2)))
    (dotimes (r-idx length1)
      (dotimes (c-idx length2)
        (setf (aref res r-idx c-idx)
              (* (aref vec1 r-idx)
                 (conj* (aref vec2 c-idx))))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4) Covariance Computation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-covariance-matrix (antenna-data)
  "Compute the sample covariance from a list of arrays (num-antennas of length T each)."
  (declare (type list antenna-data))
  (let* ((num-antennas (length antenna-data))
         (num-snapshots (length (first antenna-data))))
    (let ((data-mat (make-cmatrix num-antennas num-snapshots
                                  :initial-element #C(0.0d0 0.0d0))))
      (dotimes (ant-idx num-antennas)
        (let ((samples (nth ant-idx antenna-data)))
          (dotimes (snap-idx num-snapshots)
            (setf (aref data-mat ant-idx snap-idx)
                  (elt samples snap-idx)))))
      (let ((r-matrix (cmatrix-mult data-mat (cmatrix-hermitian data-mat))))
        (dotimes (r-idx num-antennas)
          (dotimes (c-idx num-antennas)
            (setf (aref r-matrix r-idx c-idx)
                  (/ (aref r-matrix r-idx c-idx) num-snapshots))))
        r-matrix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5) SAMV Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vector->matrix-vertical (vec)
  "Convert a 1D array of length M into an (M x 1) 2D array."
  (let* ((m (length vec))
         (res (make-cmatrix m 1 :initial-element #C(0.0d0 0.0d0))))
    (dotimes (i m)
      (setf (aref res i 0) (aref vec i)))
    res))

(defun samv-iterate (cov-matrix a-matrix
                                &key (noise-power 1d-3)
                                     (max-iter 20)
                                     (reg 1d-9))
  "Perform a simplified iterative SAMV-like solver using 1D a-col vectors."
  (let* ((num-rows (array-dimension cov-matrix 0))
         (num-angles (array-dimension a-matrix 1))
         (p-array (make-array num-angles :initial-element #C(1.0d0 0.0d0)))
         (sigma2 noise-power))
    (dotimes (iteration max-iter)
      ;; Step 1: invert (cov-matrix + reg*I)
      (let ((inv-matrix (cmatrix-invert-regularized cov-matrix reg)))
        (dotimes (angle-idx num-angles)
          (let ((a-col (make-array num-rows
                                   :initial-element #C(0.0d0 0.0d0))))
            (dotimes (row-idx num-rows)
              (setf (aref a-col row-idx)
                    (aref a-matrix row-idx angle-idx)))
            (let* ((a-col-m (vector->matrix-vertical a-col))
                   (a-col-h (cmatrix-hermitian a-col-m))
                   (temp1 (cmatrix-mult a-col-h
                                        (cmatrix-mult inv-matrix a-col-m)))
                   (den (aref temp1 0 0)))
              (if (<= (cabs2 den) 1d-20)
                  (setf (aref p-array angle-idx) #C(0.0d0 0.0d0))
                  (setf (aref p-array angle-idx)
                        (/ #C(1.0d0 0.0d0) den)))))))
      ;; Step 2: estimate sigma2 from residual
      (let ((signal-part (make-cmatrix num-rows num-rows
                                       :initial-element #C(0.0d0 0.0d0))))
        (dotimes (angle-idx num-angles)
          (let ((a-col (make-array num-rows
                                   :initial-element #C(0.0d0 0.0d0))))
            (dotimes (row-idx num-rows)
              (setf (aref a-col row-idx)
                    (aref a-matrix row-idx angle-idx)))
            (setf signal-part
                  (cmatrix-add signal-part
                               (outer-product a-col a-col)
                               (aref p-array angle-idx)))))
        (let ((resid (cmatrix-add cov-matrix
                                  (cmatrix-scale signal-part
                                                 #C(-1.0d0 0.0d0)))))
          (let ((accum 0d0))
            (dotimes (idx num-rows)
              (incf accum (realpart (aref resid idx idx))))
            (setf sigma2 (/ accum num-rows))))))
    ;; Step 3: rebuild cov-matrix => sum_{g} p[g]*outer(...) + sigma2*I
    (let ((rnew (make-cmatrix num-rows num-rows
                              :initial-element #C(0.0d0 0.0d0))))
      (dotimes (angle-idx num-angles)
        (let ((a-col (make-array num-rows
                                 :initial-element #C(0.0d0 0.0d0))))
          (dotimes (row-idx num-rows)
            (setf (aref a-col row-idx)
                  (aref a-matrix row-idx angle-idx)))
          (setf rnew
                (cmatrix-add rnew
                             (outer-product a-col a-col)
                             (aref p-array angle-idx)))))
      (dotimes (diag-idx num-rows)
        (incf (aref rnew diag-idx diag-idx) sigma2))
      (setf cov-matrix rnew))
    (values p-array sigma2)))

(defun samv-solve (cov-matrix num-antennas element-spacing wavelength angle-list
                   &key (noise-power 1d-3) (max-iter 20) (reg 1d-9))
  "Main SAMV solve routine. Returns a list of (angle-radians power)."
  (let* ((num-rows (array-dimension cov-matrix 0))
         (num-angles (length angle-list))
         (a-matrix (make-cmatrix num-rows num-angles
                                 :initial-element #C(0.0d0 0.0d0))))
    ;; Build the steering matrix, columns = a(theta)
    (dotimes (angle-idx num-angles)
      (let ((steer (compute-steering-vector num-antennas element-spacing
                                            wavelength
                                            (elt angle-list angle-idx))))
        (dotimes (row-idx num-rows)
          (setf (aref a-matrix row-idx angle-idx)
                (aref steer row-idx)))))
    (multiple-value-bind (p-array final-noise)
        (samv-iterate cov-matrix a-matrix
                      :noise-power noise-power
                      :max-iter max-iter
                      :reg reg)
      (declare (ignore final-noise))
      (let ((results ()))
        (dotimes (angle-idx num-angles)
          (push (list (elt angle-list angle-idx)
                      (aref p-array angle-idx))
                results))
        (nreverse results)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6) Peak-Finding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-samv-peaks-simple (angle-power-list &key (threshold 1d-3))
  "Simple local maxima detection in (angle, power) list."
  (declare (type list angle-power-list)
           (type double-float threshold))
  (let ((peaks '()))
    (loop for i-idx from 1 below (1- (length angle-power-list)) do
          (let* ((prev (nth (1- i-idx) angle-power-list))
                 (curr (nth i-idx angle-power-list))
                 (next (nth (1+ i-idx) angle-power-list)))
            (when (and curr prev next)
              (let ((p0 (abs (second prev)))
                    (p1 (abs (second curr)))
                    (p2 (abs (second next))))
                (when (and (> p1 p0) (> p1 p2) (> p1 threshold))
                  (push curr peaks))))))
    (nreverse peaks)))

(defun find-samv-peaks-advanced (angle-power-list
                                 &key (min-group-size 1)
                                      (amp-threshold 1d-3)
                                      (window-size 2))
  "A more complex peak-finding that groups adjacent bins above a threshold."
  (declare (type list angle-power-list)
           (type (integer 1 *) min-group-size window-size)
           (type double-float amp-threshold))
  (let ((above-thresh '()))
    ;; 1) Filter to bins above threshold
    (loop for i-idx from 0 below (length angle-power-list) do
          (let ((ap (nth i-idx angle-power-list)))
            (when (> (abs (second ap)) amp-threshold)
              (push (list i-idx ap) above-thresh))))
    (setf above-thresh (nreverse above-thresh))
    ;; 2) Group adjacent
    (let ((groups '())
          (current-group '())
          (last-idx nil))
      (dolist (ix-ap above-thresh)
        (destructuring-bind (the-idx apval) ix-ap
          (declare (ignore apval))
          (if (or (null last-idx)
                  (> (- the-idx last-idx) window-size))
              (progn
                (unless (null current-group)
                  (push (reverse current-group) groups))
                (setf current-group (list ix-ap)))
              (push ix-ap current-group))
          (setf last-idx the-idx)))
      (unless (null current-group)
        (push (reverse current-group) groups))
      (setf groups (nreverse groups))
      ;; 3) For each group, pick bin with max amplitude
      (let ((peaks '()))
        (dolist (grp groups)
          (when (>= (length grp) min-group-size)
            (let ((best nil)
                  (best-amp 0d0))
              (dolist (ix-ap grp)
                (let ((candidate-power (abs (second (second ix-ap)))))
                  (when (> candidate-power best-amp)
                    (setf best-amp candidate-power)
                    (setf best ix-ap))))
              (when best
                (push (second best) peaks)))))
        (nreverse peaks)))))

(defun rad2deg (radians)
  (declare (type double-float radians))
  (* 180d0 (/ radians pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7) Main Pipeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun samv-doa (antenna-data
                 &key (d 0.5d0)
                      (lambda 1.0d0)
                      (ang-min -90d0)
                      (ang-max 90d0)
                      (ang-step 1d0)
                      (noise-power 1d-3)
                      (max-iter 20)
                      (reg 1d-9)
                      (peak-threshold 1d-3)
                      (peak-fn :simple))
  "Full pipeline to compute DoA from antenna data.
   Returns a list of (:angle-deg X :power Y)."
  (declare (type list antenna-data)
           (type double-float d lambda ang-min ang-max ang-step noise-power reg peak-threshold)
           (type (integer 1 *) max-iter)
           (type symbol peak-fn))
  (let* ((num-antennas (length antenna-data))
         (cov-matrix (compute-covariance-matrix antenna-data))
         (angle-deg-list (loop for angle-deg from ang-min to ang-max by ang-step collect angle-deg))
         (angle-rad-list (map 'list (lambda (degval) (* degval (/ pi 180d0))) angle-deg-list)))
    (let ((angle-power (samv-solve cov-matrix
                                   num-antennas
                                   d
                                   lambda
                                   angle-rad-list
                                   :noise-power noise-power
                                   :max-iter max-iter
                                   :reg reg)))
      (let ((peaks
              (ecase peak-fn
                (:simple
                 (find-samv-peaks-simple angle-power :threshold peak-threshold))
                (:advanced
                 (find-samv-peaks-advanced angle-power
                                           :amp-threshold peak-threshold
                                           :min-group-size 1
                                           :window-size 1))
                (otherwise
                 (error "Unknown peak-fn: ~a" peak-fn)))))
        ;; Convert each peak from rad->deg, keep power
        (mapcar (lambda (pk)
                  (destructuring-bind (the-angle-rad the-power) pk
                    (list :angle-deg (rad2deg the-angle-rad)
                          :power (abs the-power))))
                peaks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8) Data Simulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-normal (mean std)
  "Return a double-float sample from N(mean, std^2) using Box-Muller."
  (declare (type double-float mean std))
  (let ((u1 (random 1.0d0))
        (u2 (random 1.0d0)))
    (declare (type double-float u1 u2))
    (let ((r (sqrt (* -2d0 (log u1))))
          (theta (* 2d0 pi u2)))
      (declare (type double-float r theta))
      (+ mean (* std r (cos theta))))))

(defun simulate-array-data (num-antennas num-snapshots element-spacing wavelength sources
                              &key (noise-level 1d-3))
  "Generate synthetic data for a uniform linear array with multiple sources.
   Each entry = complex sum of signals + noise.
   Returns (list of length num-antennas), each a 1D array of length num-snapshots."
  (declare (type (integer 1 *) num-antennas num-snapshots)
           (type double-float element-spacing wavelength noise-level)
           (type list sources))
  (let ((data-collect
         (loop for a-idx from 0 below num-antennas
               collect (make-array num-snapshots
                                   :initial-element #C(0.0d0 0.0d0)))))
    (let ((source-info
           (mapcar
            (lambda (src)
              (destructuring-bind (ang-deg pwr) src
                (declare (type double-float ang-deg pwr))
                (let* ((ang-rad (* ang-deg (/ pi 180d0)))
                       (sv (compute-steering-vector num-antennas
                                                    element-spacing
                                                    wavelength
                                                    ang-rad)))
                  (list sv pwr))))
            sources)))
      ;; Fill data
      (dotimes (snap-idx num-snapshots)
        (dolist (src source-info)
          (destructuring-bind (sv pwr) src
            (dotimes (ant-idx num-antennas)
              (incf (aref (nth ant-idx data-collect) snap-idx)
                    (* pwr (aref sv ant-idx))))))
        ;; Add Gaussian noise
        (dotimes (ant-idx num-antennas)
          (let ((nr (random-normal 0d0 noise-level))
                (ni (random-normal 0d0 noise-level)))
            (incf (aref (nth ant-idx data-collect) snap-idx)
                  (complex nr ni))))))
    data-collect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 9) Example Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-samv-doa ()
  "Quick test with multiple sources.
   Should see peaks near -10 deg and +30 deg in the results."
  (let* ((num-ant 4)
         (num-snap 50)
         (d-spacing 0.5d0)
         (wl 1.0d0)
         ;; two sources: one at -10 deg, pwr=2.0; one at +30 deg, pwr=1.0
         (sources (list (list -10d0 2d0)
                        (list 30d0 1.6d0)))
         (data (simulate-array-data num-ant num-snap d-spacing wl sources
                                    :noise-level 0.01d0))
         ;; Run SAMV
         (results-simple
           (samv-doa data
                     :d d-spacing
                     :lambda wl
                     :ang-min -90d0
                     :ang-max 90d0
                     :ang-step 2d0
                     :noise-power 1d-6
                     :max-iter 50
                     :reg 1d-5 ;; increaseing reg from 1d-9 to 1d-5 made it much better
                     :peak-threshold 1d-4
                     :peak-fn :simple))
         (results-adv
           (samv-doa data
                     :d d-spacing
                     :lambda wl
                     :ang-min -90d0
                     :ang-max 90d0
                     :ang-step 2d0
                     :noise-power 1d-6
                     :max-iter 50
                     :reg 1d-5
                     :peak-threshold 1d-4
                     :peak-fn :advanced)))
    ;;(format t "-- data: ~a~%" data)
    (format t "Simple Peak-Finding => ~a~%" results-simple)
    (format t "Advanced Peak-Finding => ~a~%" results-adv)))

;; 677cab9f-ee74-8000-b02e-cf286d387fdc

;; todo: fft fmcw returns, ifft each single bin of data as radius from rx,
;;  samv or music that ifft'd data, plot as a "band" at once on radial graph
