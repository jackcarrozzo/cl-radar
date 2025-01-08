(in-package :cl-user)
(defpackage cl-radar.gen
  (:use :cl
   :cl-radar.util
   :cl-radar.math))
(in-package :cl-radar.gen)
(cl-syntax:use-syntax :annot)

;; code that was at least partially generated - useful but suspect



@export
(defun simulate-fmcw-returns-with-carrier (targets
                              &key
                              (num-antennas 4)
                              (element-spacing 0.01d0)
                              (carrier-freq 10.100e9)        ; [Hz]
                              (bandwidth 800e6)              ; [Hz]
                              (chirp-time 1e-3)              ; [s]
                              (sample-rate 1e6)              ; [samples/s]
                              (speed-of-light 3.0d8))
  "
SIMULATE-FMCW-RETURNS computes a 2D array of complex samples representing
the received FMCW signals at each of several antennas in a linear array.

ARGUMENTS:
  - TARGETS: A list of (distance angle amplitude) for each target.
      distance -> [meters]
      angle    -> [radians] (broadside = 0.0)
      amplitude-> dimensionless scaling factor for the echo

KEY PARAMETERS:
  - NUM-ANTENNAS: Number of elements in the linear array
  - ELEMENT-SPACING: Spacing (in meters) between adjacent array elements
  - CARRIER-FREQ: Center frequency f0 of the chirp in Hz
  - BANDWIDTH: Frequency sweep range in Hz
  - CHIRP-TIME: Duration of one FMCW chirp in seconds
  - SAMPLE-RATE: Baseband sampling rate in samples/sec
  - SPEED-OF-LIGHT: Speed of wave propagation (m/s); defaults to 3e8

RETURNS:
  A 2D array of complex-double-float. Dimensions:
    (num-antennas x number-of-time-samples)

EXAMPLE CALL:
  (simulate-fmcw-returns '((100.0d0 0.0d0 1.0d0) (150.0d0 0.1d0 0.8d0))
                         :num-antennas 8
                         :element-spacing 0.075d0
                         :carrier-freq 77.0e9
                         :bandwidth 1.0e9
                         :chirp-time 5.0e-5
                         :sample-rate 2.0e6)
"
  ;; Number of complex samples in time to simulate for one full chirp
  (let* ((num-samples (round (* chirp-time sample-rate)))
         ;; Allocate a 2D array: (antenna-index, time-sample-index)
         (rx-array (make-array (list num-antennas num-samples)
                               ;;:element-type 'complex-double-float
                               :initial-element #C(0.0d0 0.0d0)))
         ;; Precompute some constants
         (dt (/ 1d0 sample-rate))       ; time step between samples
         (chirp-slope (/ bandwidth chirp-time))  ; Hz per second
         (two-pi 6.283185307179586d0)
         (lambda (/ speed-of-light carrier-freq)) ; approximate wavelength
         ;; For convenience, precompute wave number factor = (2π/λ)
         (wave-number (/ (* 2d0 pi) lambda)))

    ;; ----------------------------------------------------------------
    ;; Fill in the 2D array with the sum of returns from all targets.
    ;; ----------------------------------------------------------------
    (dotimes (n num-samples)
      (let* ((tm (* n dt))  ;; current time for this sample
             ;; instantaneous chirp frequency at time t
             ;; f(t) = f0 + chirp-slope * t
             (inst-freq (+ carrier-freq (* chirp-slope tm)))
             ;; baseband phase offset of the transmit signal so far
             ;; phase(t) = 2π ∫(from 0 to t) f(τ) dτ
             ;; = 2π [f0·t + (chirp-slope/2)·t^2]
             (tx-phase (* two-pi
                          (+ (* carrier-freq tm)
                             (* 0.5 chirp-slope tm tm)))))
        (declare (ignore inst-freq))
        (declare (ignore tx-phase))

        ;; Loop over each antenna element
        (dotimes (ant num-antennas)
          (let ((sum-of-targets 0d0))  ;; accumulate complex returns
            (dolist (tg targets)
              (destructuring-bind (distance angle amplitude) tg
                ;; Round-trip delay for the target (distance -> round-trip 2R/c)
                (let* ((delay (/ (* 2d0 distance) speed-of-light))
                       ;; The time at which this path is "observed"
                       (time-delay (- tm delay)))
                  (when (plusp time-delay)
                    ;; The instantaneous freq for the delayed signal
                    ;; (assuming same chirp slope, just delayed)
                    (let* ((inst-freq-target
                            (+ carrier-freq (* chirp-slope time-delay)))
                           ;; The transmit phase at the delayed time
                           (tx-phase-target
                            (* two-pi
                               (+ (* carrier-freq time-delay)
                                  (* 0.5 chirp-slope time-delay time-delay)))))
                      (declare (ignore inst-freq-target))
                      ;; Phase shift due to propagation angle for each antenna
                      ;; additional spatial phase = k * d_antenna
                      ;; d_antenna = ant * element-spacing * sin(angle),
                      ;; but strictly it’s the projection along the wavefront:
                      (let ((spatial-phase (* wave-number
                                              ant
                                              element-spacing
                                              (sin angle))))
                        ;; The total phase = TX-chirp-phase(delayed) + spatial
                        (let ((phase-total (+ tx-phase-target spatial-phase)))
                          (incf sum-of-targets
                                ;; amplitude * e^{j·(phase_total)}
                                (complex
                                 (* amplitude (cos phase-total))
                                 (* amplitude (sin phase-total)))))))))))
            ;; Add sum-of-targets to the baseband reference. One might
            ;; also factor out the reference chirp’s own baseband mixing
            ;; (i.e., multiply by exp(-j·tx-phase)), depending on the
            ;; modeling approach. For demonstration, we omit that step
            ;; or you can uncomment the next two lines:

            ;; (let ((ref-mix (exp #C(0d0 -1d0 * tx-phase))))
            ;;   (setf sum-of-targets (* sum-of-targets ref-mix)))

            ;; Store in the array
            (setf (aref rx-array ant n) sum-of-targets)))))

    rx-array))

;;;; without reference phased and unused vars:
@export
(defun simulate-fmcw-returns (targets
                              &key
                              (num-antennas 4)
                              (element-spacing 0.5d0)
                              (carrier-freq 24.125e9)        ; [Hz]
                              (bandwidth 200e6)              ; [Hz]
                              (chirp-time 1e-3)              ; [s]
                              (sample-rate 2e6)              ; [samples/s]
                              (speed-of-light 3.0d8))
  "
SIMULATE-FMCW-RETURNS computes a 2D array of complex samples representing
the received FMCW signals at each of several antennas in a linear array.

ARGUMENTS:
  - TARGETS: A list of (distance angle amplitude) for each target.
      distance -> [meters]
      angle    -> [radians] (broadside = 0.0)
      amplitude-> dimensionless scaling factor for the echo

KEY PARAMETERS:
  - NUM-ANTENNAS: Number of elements in the linear array
  - ELEMENT-SPACING: Spacing (in meters) between adjacent array elements
  - CARRIER-FREQ: Center frequency f0 of the chirp in Hz
  - BANDWIDTH: Frequency sweep range in Hz
  - CHIRP-TIME: Duration of one FMCW chirp in seconds
  - SAMPLE-RATE: Baseband sampling rate in samples/sec
  - SPEED-OF-LIGHT: Speed of wave propagation (m/s); defaults to 3e8

RETURNS:
  A 2D array of complex-double-float. Dimensions:
    (num-antennas x number-of-time-samples)
"
  (let* ((num-samples (round (* chirp-time sample-rate)))
         ;; Allocate a 2D array: (antenna-index, time-sample-index)
         (rx-array (make-array (list num-antennas num-samples)
                               ;;:element-type 'complex-double-float
                               :initial-element #C(0.0d0 0.0d0)))
         ;; Precompute some constants
         (dt (/ 1d0 sample-rate))       ; time step between samples
         (chirp-slope (/ bandwidth chirp-time))  ; Hz per second
         (two-pi 6.283185307179586d0)
         (lambda (/ speed-of-light carrier-freq)) ; approximate wavelength
         (wave-number (/ (* 2d0 pi) lambda)))      ; wave number = 2π/λ

    (dotimes (n num-samples)
      (let ((tm (* n dt)))  ;; current time for this sample
        ;; Loop over each antenna element
        (dotimes (ant num-antennas)
          (let ((sum-of-targets 0d0))  ;; accumulate complex returns
            (dolist (tg targets)
              (destructuring-bind (distance angle amplitude) tg
                ;; Round-trip delay for the target (distance -> 2R/c)
                (let* ((delay (/ (* 2d0 distance) speed-of-light))
                       (time-delay (- tm delay)))
                  (when (plusp time-delay)
                    ;; The transmit phase at the delayed time
                    ;; φ_delayed(t) = 2π [f0·(t-τ) + 0.5·chirp-slope·(t-τ)^2]
                    (let* ((tx-phase-target
                             (* two-pi
                                (+ (* carrier-freq time-delay)
                                   (* 0.5 chirp-slope time-delay time-delay))))
                           ;; Additional spatial phase for each antenna
                           (spatial-phase
                             (* wave-number ant element-spacing (sin angle))))
                      (incf sum-of-targets
                            (complex
                             (* amplitude (cos (+ tx-phase-target spatial-phase)))
                             (* amplitude (sin (+ tx-phase-target spatial-phase))))))))))
            (setf (aref rx-array ant n) sum-of-targets)))))
    rx-array))

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
         (num-samples (length (car antenna-data)))
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
