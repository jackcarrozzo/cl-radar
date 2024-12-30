(in-package :cl-user)
(defpackage cl-radar.test-wavegen
  (:use :cl :clunit))
(in-package :cl-radar.test-wavegen)

(cl-syntax:use-syntax :annot)

(defsuite WavegenSuite (CL-RADAR.TEST-MAIN::MAINSUITE))

(deftest test-sines-fill-2ch-array (WavegenSuite)
  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:sines-fill-2ch-array (make-array '(2 10) :initial-element 0.0))
       #2A((0.0 0.0065447977 0.013088474 0.019629909 0.026167978 0.032701563
                0.03922955 0.045750808 0.052264232 0.058768697)
           (0.0 6.544798e-4 0.0013088475 0.001962991 0.0026167978 0.0032701564
                0.003922955 0.0045750807 0.0052264235 0.00587687)))))


(deftest test-signals-fill-2ch-array (WavegenSuite)
  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:signals-fill-2ch-array
        (make-array '(2 10) :initial-element 0.0)
        :left-sig-next-fn (lambda () (cl-radar.wavegen:ramp-get-next 48000 4800)))
       #2A((-0.25 -0.17337579 -0.09675158 -0.020127371 0.05649684 0.13312104
                  0.20974526 0.28636947 0.3629937 0.4396179)
           (0.0065447977 0.019629909 0.032701563 0.045750808 0.058768697 0.07174631
                         0.08467475 0.09754516 0.11034872 0.12307665))))

  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:signals-fill-2ch-array
        (make-array '(2 10) :initial-element 0.0)
        :left-sig-next-fn (lambda () (cl-radar.wavegen:ramp-get-next 48000 4800))
        :right-sig-next-fn (lambda () (cl-radar.wavegen:sines-get-next 48000 4800))
        :left-ampl 0.7
        :right-ampl 2.1)
       #2A((-0.175 -0.12136305 -0.067726105 -0.014089159 0.039547786 0.09318473
                0.14682168 0.20045863 0.25409558 0.30773252)
        (0.6171745 0.9986093 5.9488156e-16 -0.9986093 -0.6171745 0.6171745
                   0.9986093 -3.3445854e-15 -0.9986093 -0.6171745))))

  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:signals-fill-2ch-array (make-array '(2 24) :initial-element 0.0) :left-sig-next-fn (lambda () (cl-radar.wavegen:ramp-get-next 48000 4800)) :right-sig-next-fn (lambda () (cl-radar.wavegen:sines-get-next 48000 4800)) :left-ampl 0.7 :right-ampl 2.1)
       #2A((-0.175 -0.12136305 -0.067726105 -0.014089159 0.039547786 0.09318473
                   0.14682168 0.20045863 0.25409558 0.30773252 0.36136946 -0.14818153
                   -0.094544575 -0.040907633 0.012729314 0.06636626 0.12000321 0.17364015
                   0.2272771 0.28091404 0.33455098 0.38818794 -0.14818153 -0.094544575)
           (0.6171745 0.9986093 5.9488156e-16 -0.9986093 -0.6171745 0.6171745
                      0.9986093 -3.3445854e-15 -0.9986093 -0.6171745 0.0 0.9986093 0.6171745
                      -0.6171745 -0.9986093 6.7541147e-16 0.9986093 0.6171745 -0.6171745
                      -0.9986093 5.0811724e-15 0.0 0.9986093 0.6171745))))



  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:signals-fill-2ch-array (make-array '(2 24) :initial-element 0.0) :left-sig-next-fn (lambda () (cl-radar.wavegen:ramp-get-next 48000 4800)) :right-sig-next-fn (lambda () (cl-radar.wavegen:sines-get-next 48000 4800)))
       #2A((-0.25 -0.17337579 -0.09675158 -0.020127371 0.05649684 0.13312104
                  0.20974526 0.28636947 0.3629937 0.4396179 0.5162421 -0.2116879 -0.13506368
                  -0.058439475 0.018184735 0.09480894 0.17143315 0.24805737 0.32468158
                  0.4013058 0.47792998 0.5545542 -0.2116879 -0.13506368)
           (0.29389262 0.47552827 2.8327693e-16 -0.47552827 -0.29389262 0.29389262
                       0.47552827 -1.5926599e-15 -0.47552827 -0.29389262 0.0 0.47552827
                       0.29389262 -0.29389262 -0.47552827 3.2162452e-16 0.47552827 0.29389262
                       -0.29389262 -0.47552827 2.419606e-15 0.0 0.47552827 0.29389262))))

  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:signals-fill-2ch-array
        (make-array '(2 24) :initial-element 0.0)
        :left-sig-next-fn #'cl-radar.wavegen:sines-get-next
        :right-sig-next-fn #'cl-radar.wavegen:ramp-get-next)
       #2A((0.0 0.013088474 0.026167978 0.03922955 0.052264232 0.06526309 0.07821723
                0.09111776 0.10395584 0.11672268 0.12940952 0.14200768 0.1545085
                0.16690344 0.17918397 0.19134171 0.20336832 0.21525554 0.22699524
                0.23857938 0.25 0.26124927 0.27231953 0.28320313)
           (-0.24920183 -0.24760549 -0.24600916 -0.24441282 -0.24281648 -0.24122015
                        -0.2396238 -0.23802747 -0.23643112 -0.23483479 -0.23323846 -0.23164211
                        -0.23004578 -0.22844943 -0.2268531 -0.22525677 -0.22366042 -0.2220641
                        -0.22046775 -0.21887141 -0.21727507 -0.21567874 -0.2140824 -0.21248606))))

  (setf cl-radar.wavegen::*phase* 0.0)
  (assert-true
      (cl-radar.math:float-multidim-array-mostly-equal-p
       (cl-radar.wavegen:signals-fill-2ch-array
        (make-array '(2 24) :initial-element 0.0)
        :left-sig-next-fn #'cl-radar.wavegen:sines-get-next
        :right-sig-next-fn #'cl-radar.wavegen:ramp-get-next)
       #2A((0.0 0.013088474 0.026167978 0.03922955 0.052264232 0.06526309 0.07821723
                0.09111776 0.10395584 0.11672268 0.12940952 0.14200768 0.1545085
                0.16690344 0.17918397 0.19134171 0.20336832 0.21525554 0.22699524
                0.23857938 0.25 0.26124927 0.27231953 0.28320313)
           (-0.24920183 -0.24760549 -0.24600916 -0.24441282 -0.24281648 -0.24122015
                        -0.2396238 -0.23802747 -0.23643112 -0.23483479 -0.23323846 -0.23164211
                        -0.23004578 -0.22844943 -0.2268531 -0.22525677 -0.22366042 -0.2220641
                        -0.22046775 -0.21887141 -0.21727507 -0.21567874 -0.2140824 -0.21248606)))))
