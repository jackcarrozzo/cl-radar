(in-package :cl-user)
(defpackage cl-radar.fmcw
  (:use :cl))
(in-package :cl-radar.fmcw)
(cl-syntax:use-syntax :annot)

;;;;;;;; spot to run it is near :450
;;;; also some stuff around :250
;;;; around 30 for live audio
;;;;

;; TODO: use new downsampler

;; TODO: really need to pick an array size per run
;;   and stick to it

;; TODO: trigger and sizing and chopper and zero pad stuff need tests

;;; moved these defs up here to fix compile

(defparameter +default-path+ "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/")

;; TODO:
(defvar *last-wav*)
(defvar *last-left-samps*)
(defvar *last-right-samps*)
;;(defvar *last-sample-rate*)
(defvar *last-timedomain-slices*)
(defvar *last-fmcw-edges*)
(defvar *fft-data*)
(defvar *fft-data-real*)
(defvar *last-fft-sum*)
(defvar *last-fft-slices*)

@export
(defvar *last-stats* nil)

;;;;;;;;;;;;;;;;;;

;; make new array half lenth but not-complex values
;; TODO: is this used?
(defun make-real-copy-half (complex-src)
  (make-array
   (/ (array-dimension complex-src 0) 2)
   :initial-element 0.0d0))

;; TODO: these need (cl-radar.math:dc-center-slices ...)

(defun slurp-and-plot-fft ()
  (snag-audio) ;; sets *r2*
  (setf *fft-data*
        (bordeaux-fft:windowed-fft (first *r2*) 512 1024 'bordeaux-fft:blackman-harris))
  (setf *fft-data-real* (make-real-copy-half *fft-data*))
  (cl-radar.math:complex-ar-mags *fft-data* *fft-data-real* :incf)
  (vgplot:plot *fft-data-real*))

(defun slurp-and-plot-fft-all ()
  (snag-audio) ;; sets *r2*
  (setf *fft-data-real* (make-array 512 :initial-element 0.0d0))

  ;; loop for each chunk of samples in *r2* summing the fft real mags
  (loop for chunk from 0 below (length *r2*)
        do
           (progn
             (setf *fft-data*
                   (bordeaux-fft:windowed-fft (nth chunk *r2*) 512 1024 'bordeaux-fft:blackman-harris))
             (cl-radar.math:complex-ar-mags *fft-data* *fft-data-real* :incf)))
  (vgplot:plot *fft-data-real*))

#|
CL-USER> (slurp-and-plot-fft)

=== Wire on. Will run 5 seconds . ===
-- 215 chunks of 1024 samples
-- ( 220160 samples total, which at 44100.0d0 hz is 4.99 sec. )
-- max of maxes is 0.0142224105.
""
CL-USER> (vgplot:close-plot)
NIL
CL-USER>
|#

#|
TODO:
- this works fine for constant freq stuff
- needs triggering and stacking
- write plots to png
|#



;;;;;;;;

;; run down stereo or two channels, at rising edge trigger,
;;   assemble frames of samples with some defined pre and post periods

;; next step is fft each one and sum ffts

;; look into summing each triggered frame in time domain

#|
CL-USER> (nth 300 *last-chopped-edges*)

(364107 . 364553)
|#

;; TODO: fft summing
;; the time domain summing, they look identical

#|
CL-USER> (read-and-slice-all)
----- 1: (CHUNK-ID RIFF
                                                     CHUNK-DATA-SIZE 2376036
                                                     FILE-TYPE WAVE)
----- 2: (CHUNK-ID fmt  CHUNK-DATA-SIZE 16 CHUNK-DATA
          (COMPRESSION-CODE 1 NUMBER-OF-CHANNELS 2 SAMPLE-RATE 48000
           AVERAGE-BYTES-PER-SECOND 192000 BLOCK-ALIGN 4
           SIGNIFICANT-BITS-PER-SAMPLE 16))
----- 2 inner: (COMPRESSION-CODE 1 NUMBER-OF-CHANNELS 2 SAMPLE-RATE 48000
                AVERAGE-BYTES-PER-SECOND 192000 BLOCK-ALIGN 4
                SIGNIFICANT-BITS-PER-SAMPLE 16)
----- 3: (CHUNK-ID data CHUNK-DATA-SIZE 2376000)
-- oh, this is a stereo wav, deinterleaving...
----- chunk reports size of 2376000 samples- half of that is 1188000.
----- array we got in chunk has 1188000 elements.
-- ok. now we have two arrays of length 1188000 samples or 24.75 seconds.
--- Found 777 chunks (windows? frames? triggers?) with start and end.
--- (Data ended while searching for close to chunk starting at 593946.
)--- Average samples per chunk/window/frame/trigger: 509.59717 ( 0.010616608 sec at 48000 hz)
--- post-chopper: average samples per chunk/window/frame/trigger: 445.59717 ( 0.009283274 sec at 48000 hz)
--- zero pad: size requested is 446 samples, padding to 512.
---- Array avg is 0.33107424; correcting.
---- rezeroing end len: setting 66 spots to 0.
--- zero pad: size requested is 446 samples, padding to 512.
---- Array avg is 0.33195758; correcting.
---- rezeroing end len: setting 66 spots to 0.
--- zero pad: size requested is 445 samples, padding to 512.
---- Array avg is 0.3299892; correcting.
[...]
---- Array avg is 0.32987678; correcting.
---- rezeroing end len: setting 66 spots to 0.
--- zero pad: size requested is 445 samples, padding to 512.
---- Array avg is 0.32797974; correcting.
---- rezeroing end len: setting 67 spots to 0.
--- zero pad: size requested is 446 samples, padding to 512.
---- Array avg is 0.32810563; correcting.
---- rezeroing end len: setting 66 spots to 0.
--- zero pad: size requested is 445 samples, padding to 512.
---- Array avg is 0.32651287; correcting.
---- rezeroing end len: setting 67 spots to 0.
-- Trigger-aligned, zero-padded, and removed dc bias from 777 slices.
CL-USER> (sum-arrays *last-timedomain-slices*)
NIL
CL-USER> (vgplot:plot *last-summed-ar*)
""

CL-USER>

|#

;; TODO:



(defun looping-capture-and-plot ()
  (loop for i from 0 below 1000
        do
           (let ((ffmpeg-cmd (cl-radar.audio:ffmpeg-cmd-for-platform))
                 (+wav-write-path+ cl-radar.audio::+wav-write-path+) ;; TODO:
                 (+wav-process-path+ cl-radar.audio::+wav-process-path+))
             (format t "-- Running capture cmd: ~a~%" ffmpeg-cmd)

             (multiple-value-bind (errstr outstr retval)
                 (uiop:run-program ffmpeg-cmd
                                   :output :string
                                   :error-output :string
                                   :ignore-error-status t)

               (format t "-- Capture return val: ~a~%" retval)
               (format t "----- stdout: ~%~a~%" outstr)
               (format t "----- stderr: ~%~a~%" errstr)

               (when (= 0 retval)
                 (format t "-- Looks like capture succeeded, moving ~a to ~a and processing.~%"
                         +wav-write-path+ +wav-process-path+)
                 (uiop:rename-file-overwriting-target +wav-write-path+ +wav-process-path+)

                 (process-and-plot-single-wav +wav-process-path+ :log-scale-p nil)
                 (format t "----- done!~%"))))))

;; this is just single-capture-and-plot with less fancy graph (TODO:)
(defun process-and-plot-single-wav (wav-path &key (log-scale-p nil))
  ;; parse wav, find edges, slice into triggered arrays
  (read-and-slice-all wav-path)

  (cl-radar.math:dc-center-slices *last-timedomain-slices*)

  ;; fft each slice, calc mags
  ;; sum fft of slice with the rest of this capture
  (let ((loop-fft-sums
          (cl-radar.math:fft-and-sum-slices *last-timedomain-slices*)))
    (assert loop-fft-sums)

    (when log-scale-p
      (cl-radar.math:dB-ulize-array loop-fft-sums))

    ;; plot with correct x axis labels
    (vgplot:plot
     (make-vgplot-x-axis
      loop-fft-sums
      #'fmcw-dist-from-bin
      ;;#'fft-bin-num-to-hz
      )
     loop-fft-sums
     ";radar dist returns (ft);"))

  ;; TODO: save as png
  )

(defun file-mtime (fpath)
  (osicat-posix:stat-mtime
   (osicat-posix:stat fpath)))

(defparameter +watched-wav-path+ "/Users/jackc/out.wav")
(defvar *last-wav-mtime* nil)

(defvar *n* 0) ;; frame counter

@export
(defun looping-dir-watcher-and-plot ()
  (loop for i from 0 below 1000
        do
           (let ((this-mtime (file-mtime +watched-wav-path+)))
             (when (not (eql this-mtime *last-wav-mtime*))
               (format t "~%-- Found new wav, mtime ~a sec after prev.~%"
                       (- this-mtime (or *last-wav-mtime* (1+ this-mtime))))
               (setf *last-wav-mtime* this-mtime)

               (single-capture-and-plot :log-scale-p nil))

             (princ ".")
             (when (= 0 (mod i 10))
               (format t "~%"))
             (sleep 1.0))))

(defvar *n* 0) ;; for below

;; wraps single fft slice in an object as fft_bin_sums[]
@export
(defun format-json-frame (&optional loop-fft-sums n)
  (when (not loop-fft-sums)
    (assert *last-timedomain-slices*)
    (setf loop-fft-sums
          (cl-radar.math:fft-and-sum-slices
           (cl-radar.math:dc-center-slices
            *last-timedomain-slices*))))

  (cl-json:encode-json-plist-to-string
   `(:n ,(or n (incf *n*))
     :type "fft" ;; ie, single
     :fft--length ,(length loop-fft-sums)
     :sample--rate--hz ,cl-radar.audio:*last-sample-rate*
     ;;:x--axis--m ,(make-vgplot-x-axis loop-fft-sums #'fmcw-dist-from-bin)
     ;;:x--axis--hz ,(make-vgplot-x-axis loop-fft-sums #'fft-bin-num-to-hz)
     :fft--bin--mags ,loop-fft-sums)))

;; wraps a list of fft mag arrays into an object as fft_bin_slices[[]]
@export
(defun format-json-waterfall (&optional (fft-slices-list *last-fft-slices*) n)
  #|(when (not fft-slices-list)
    (assert *last-timedomain-slices*)
    (setf fft-slices-list
          (cl-radar.math:fft-slices
           (cl-radar.math:dc-center-slices
             *last-timedomain-slices*))))
  |#
  (assert fft-slices-list)

  (cl-json:encode-json-plist-to-string
   `(:n ,(or n (incf *n*))
     :type "waterfall"
     :num--fft--slices ,(length fft-slices-list)
     :sample--rate--hz ,cl-radar.audio:*last-sample-rate*
     :x--axis--m ,(make-vgplot-x-axis (first fft-slices-list) #'fmcw-dist-from-bin)
     :x--axis--hz ,(make-vgplot-x-axis (first fft-slices-list) #'fft-bin-num-to-hz)
     :fft--bin--slices ,fft-slices-list
     :win--length--sec ,(getf *last-stats* :avg-offset-trig-period-sec)
     :win--length--samps ,(getf *last-stats* :avg-offset-trig-period)
     :trig--freq--hz ,(getf *last-stats* :avg-rising-trig-freq)
     :trigs--in--chunk ,(getf *last-stats* :trig-periods-kept))))

@export
(defun format-signal-and-trigger-frame (edge-list trigger-samples signal-samples-ars-list from-i to-i)
  (let ((relevant-edges (filter-edges-by-bound edge-list from-i to-i))
        (trig-ar (subseq trigger-samples from-i to-i)) ;; TODO:
        ;;(trig-ar (subseq trigger-samples 0 (- to-i from-i)))
        (signal-ars (mapcar
                     (lambda (s-ar)
                       (subseq s-ar from-i to-i) ;;TODO:
                       ;;(subseq s-ar 0 (- to-i from-i))
                       )
                     signal-samples-ars-list)))

    ;; wtf TODO:
    ;;(format t "-- trig 1 2 3: ~a~%"
    ;;        (subseq trigger-samples 0 3))
    ;;(format t "-- trig 1 2 3: ~a~%"
    ;;        (subseq trigger-samples (- from-i 500) (- (+ from-i 3) 500)))

    (cl-json:encode-json-plist-to-string
     `(:n ,(incf *n*)
       :type "wave"
       :trigger--edges ,relevant-edges
       :trigger--samples ,trig-ar
       :signal--samples ,signal-ars))))

@export
(defun filter-edges-by-bound (edge-list from-i to-i)
  (remove-if-not
   (lambda (e)
     (and
      (>= (first e) from-i)
      (<= (rest e) to-i)))
   edge-list))

(defvar *looper-thread* nil)

;; CL-USER> (cl-radar.fmcw:threaded-looping-ws-stream)
;; CL-USER> (cl-radar.fmcw:stop-looper)
;; CL-USER> (sb-thread:list-all-threads )

@export
(defun threaded-looping-ws-stream (&key (fft-slices-list *last-fft-slices*)
                                     (max-loops-thru 10000)
                                     (delay-s 0.2)
                                     (log-p t))

  (if *looper-thread*
      (progn
        (format t "-- not starting another looping thread! one exists.~%")
        *looper-thread*)
      (progn

        (format t "-- starting looping-ws-fft-stream in thread... ~%")

        (cl-radar.websocket:run-if-not-already) ;; blows up if ws is started from other thread

        (setf
         *looper-thread*
         (sb-thread:make-thread
          (lambda ()
            (write-line "---- hello from looper thread!~%")
            (looping-ws-fft-stream fft-slices-list
                                   :max-loops-thru max-loops-thru
                                   :delay-s delay-s
                                   :log-p log-p
                                   :include-waves-every 100)
            (write-line "--- looper thread all done, leaving.")
            (setf *looper-thread* nil))))

        (format t "-- ok, seems to have worked.~%")
        (sb-thread:list-all-threads)
        *looper-thread*)))

@export
(defun stop-looper (&optional (looper-thread *looper-thread*))
  (sb-thread:list-all-threads)

  (format t "-- stop on: ~a~%" looper-thread)

  (sb-thread:terminate-thread looper-thread)

  (setf *looper-thread* nil))

#|
(progn (cl-radar.fmcw:read-and-slice-and-fft-stereo-wav :single-out-p nil) 7)
(cl-radar.fmcw:looping-ws-fft-stream cl-radar.fmcw::*last-fft-slices*)

(cl-radar.fmcw:looping-ws-fft-stream
  cl-radar.fmcw::*last-fft-slices* :max-loops-thru 10)
|#

;; TODO: send chunks of slices at a time (and tell the ui)

@export
(defun looping-ws-fft-stream (fft-slices-list &key (max-loops-thru 1000)
                                              (slices-at-once 1)
                                                (delay-s 0.1) (marker-p t)
                                                (log-p t) (include-waves-every nil)
                                                (wave-samples-len 700))
  (dotimes (i max-loops-thru)
    (let ((curr-slice 0))
      (loop while (< curr-slice (- (length fft-slices-list) slices-at-once))
            do
               (let ((these-slices
                       (subseq fft-slices-list
                               curr-slice (+ curr-slice slices-at-once))))
                 (cl-radar.websocket:send-to-all-clients
                  (format-json-waterfall
                   (if log-p
                       (mapcar #'(lambda (s)
                                   (cl-radar.math:array-add-offset
                                    (cl-radar.math:dB-ulize-array s)
                                    30))
                               these-slices)
                       these-slices)
                   (incf curr-slice slices-at-once)))
                 (when (or (= 0 curr-slice)
                           (and include-waves-every
                                (= 0 (mod curr-slice include-waves-every))))
                   (let* (
                          ;;(to-i (1- (array-dimension cl-radar.audio:*last-left-samps* 0))) ;; at end
                          ;;(from-i (- to-i wave-samples-len))
                          (from-i 0)
                          (to-i (+ from-i wave-samples-len))
                          )
                     (cl-radar.websocket:send-to-all-clients
                      (format-signal-and-trigger-frame
                       *last-fmcw-edges* cl-radar.audio:*last-left-samps*
                       (list cl-radar.audio:*last-right-samps*)
                       from-i to-i))))
                 (sleep delay-s)))
      (when marker-p
        (cl-radar.websocket:send-to-all-clients
         (format-json-waterfall
          (list (make-array 256 :initial-element 0.1)
                (make-array 256 :initial-element 0.1)) ;; TODO:
          curr-slice)))))
  (format t "-- looping-ws-fft-stream done.~%"))

@export
(defun write-js-waterfall (&optional (fpath
                                      "~/Projects/cl-radar/data/wfdata.js"))
  (cl-radar.util:write-string-to-js-file
   fpath "waterfalldata"
   (format-json-waterfall)))

@export
(defun single-capture-and-plot (&key (log-scale-p nil))

  ;; capture audio and stick somewhere

  ;;(read-and-slice-all "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/1-fullchain_stereo.wav")
  ;;(read-and-slice-all "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/2-twothirds_stereo.wav")

  ;;(read-and-slice-all "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/2ft-open-mdl-linearsection_leveled_stereo.wav")
  ;;(read-and-slice-all "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/6ft-open-mdl-linearsection_leveled_stereo.wav")

  (read-and-slice-all "/Users/jackc/out.wav")

  #|
  [jackc@nichor] ~/Projects/radarstuff/data/fmcw_bb/out :) $ ls|grep stereo
  1-fullchain_stereo.wav
  2-twothirds_stereo.wav
  2ft-open-mdl-linearsection_leveled_stereo.wav
  2ft-open-mdl-linearsection_untouched_stereo.wav
  6ft-open-mdl-linearsection_leveled_stereo.wav
  6ft-open-mdl-linearsection_untouched_stereo.wav
  |#

  ;; print sweet freq and stuff TODO:

  (cl-radar.math:dc-center-slices *last-timedomain-slices*)

  ;; fft each chunk, calc mags
  ;; sum fft of slice with the rest of this capture
  (let ((loop-fft-sums
          (cl-radar.math:fft-and-sum-slices *last-timedomain-slices*))) ;; TODO:

    (assert loop-fft-sums)

    (when log-scale-p
      (cl-radar.math:dB-ulize-array loop-fft-sums))

    ;; plot with correct x axis labels

    (vgplot:close-all-plots)
    (vgplot:new-plot)

    (vgplot:xlabel "dist (m)")
    ;;(vgplot:ylabel "magnitude")
    (vgplot:text 10.2 10.6 (format nil "Seq: ~a" (mod (incf *n*) 16)) :fontsize 14)
    (vgplot:format-plot t "set x2label 'meese'")
    (vgplot:format-plot t "set link x via 0.2./x inverse 0.2./x")
    ;;(vgplot:format-plot t "set link x2 via 1./x-273.15 inverse 1./(x+273.15)")

    (vgplot:format-plot t "set tics in")
    (vgplot:format-plot t "set xtics nomirror")
    (vgplot:format-plot t "set x2tics 0.1 format \"%.1f\" nomirror")
    ;;(vgplot:format-plot t "set x2tics nomirror")

    (vgplot:format-plot t "set xrange [ * : * ] noreverse nowriteback")
    (vgplot:format-plot t "set x2range [ * : * ] noreverse nowriteback")
    (vgplot:format-plot t "set yrange [ * : * ] noreverse writeback")
    (vgplot:format-plot t "set y2range [ * : * ] noreverse writeback")

    ;;(vgplot:format-plot t "set xtics border out scale 1,0.5 mirror norotate  autojustify norangelimit autofreq")
    ;;(vgplot:format-plot t "set x2tics border out scale 1,0.5 mirror norotate  autojustify norangelimit autofreq")
    (vgplot:format-plot t "set ytics border out scale 1,0.5 nomirror norotate  autojustify norangelimit autofreq")

    ;;(vgplot:format-plot t "set y2range [ * : * ] noreverse writeback")
    ;;(vgplot:format-plot t "set ")
    ;;(vgplot:format-plot t "set ")
    ;;(vgplot:format-plot t "set ")
    ;;(vgplot:format-plot t "set ")


    #|
    (make-vgplot-x-axis
    loop-fft-sums
    #'fmcw-dist-from-bin
    ;;#'fft-bin-num-to-hz
    )
    |#


    (vgplot:plot
     (make-vgplot-x-axis
      loop-fft-sums
      #'fmcw-dist-from-bin
      ;;#'fft-bin-num-to-hz
      )
     loop-fft-sums

     #|
     (make-vgplot-x-axis
     *loop-fft-sums*
     ;;#'fmcw-dist-from-bin
     #'fft-bin-num-to-hz
     )
     loop-fft-sums
     |#
     ";axes x1y1;")

    ;; save as png



    ))

(defvar *slice-n* 0)

#|
(cl-radar.fmcw::graph-fft-result
  (cl-radar.fmcw:read-and-slice-and-fft-stereo-wav :single-out-p t))

(cl-radar.image:write-slices-to-png
(cl-radar.fmcw:read-and-slice-and-fft-stereo-wav :single-out-p nil))

(cl-radar.image:write-slices-to-png (cl-radar.fmcw:read-and-slice-and-fft-stereo-wav :wav-path "/Users/jackc/Projects/cl-radar/data/captrigfast_stereo.wav" :single-out-p nil :ac-trig-p t))
|#

@export
(defun graph-fft-result (fft-ar)
  (vgplot:close-all-plots)
  (vgplot:new-plot)

  (vgplot:xlabel "dist (m)")
  ;;(vgplot:ylabel "magnitude")
  (vgplot:text 10.2 10.6 (format nil "Seq: ~a" (mod (incf *n*) 16)) :fontsize 14) ;; doesnt work

  ;;(vgplot:format-plot t "set x2label 'meese'")
  (vgplot:format-plot t (format nil "set x2label 'meese, seq: ~a'" (incf *slice-n*)))

  ;;(vgplot:format-plot t "set link x via 0.2./x inverse 0.2./x")
  (vgplot:format-plot t "set link x2 via 0.2./x inverse 0.2./x")

  (vgplot:format-plot t "set tics in")
  (vgplot:format-plot t "set xtics nomirror")
  (vgplot:format-plot t "set x2tics 0.5 format \"%.1f\" nomirror")
  ;;(vgplot:format-plot t "set x2tics nomirror")

  (vgplot:format-plot t "set xrange [ * : * ] noreverse nowriteback")
  (vgplot:format-plot t "set x2range [ * : * ] noreverse nowriteback")
  (vgplot:format-plot t "set yrange [ * : * ] noreverse writeback")
  (vgplot:format-plot t "set y2range [ * : * ] noreverse writeback")

  ;;(vgplot:format-plot t "set xtics border out scale 1,0.5 mirror norotate  autojustify norangelimit autofreq")
  ;;(vgplot:format-plot t "set x2tics border out scale 1,0.5 mirror norotate  autojustify norangelimit autofreq")
  (vgplot:format-plot t "set ytics border out scale 1,0.5 nomirror norotate  autojustify norangelimit autofreq")

  ;;(vgplot:format-plot t "set y2range [ * : * ] noreverse writeback")


    #|
    (make-vgplot-x-axis
    loop-fft-sums
    #'fmcw-dist-from-bin
    ;;#'fft-bin-num-to-hz
    )
    |#

  (vgplot:plot
     (make-vgplot-x-axis
      fft-ar
      #'fmcw-dist-from-bin
      ;;#'fft-bin-num-to-hz
      )
     fft-ar))

(defvar *last-trigger-samples-raw* nil)
(defvar *last-trigger-samples-filtered* nil)
(defvar *last-trigger-padded* nil)

;; this is the good new prefered method since it uses a constant ar size chosen at start
;;   (obv wont work for variable trigger timing)
@export
(defun read-and-slice-and-fft-stereo-wav (&key (wav-path "/Users/jackc/out.wav")
                                            (debug-p t) (single-out-p t) (ac-trig-p nil))
  (format t "-- read-and-slice-stereo-wav: ~a~%" wav-path)

  (cl-radar.audio:read-wav wav-path t)

  (setf (getf *last-stats* :sample-rate)
        cl-radar.audio:*last-sample-rate*)

  (let* ((trigger-edges
           (apply-edge-offsets
            (filter-wack-edges
             (if ac-trig-p
                 (find-ac-edges cl-radar.audio:*last-left-samps*)
                 (find-edges cl-radar.audio:*last-left-samps*)))
            :start-offset 3 :end-offset 3))
         (avg-trig-len (getf *last-stats* :avg-offset-trig-period))
         (ar-sample-len (cl-radar.math:next-power-of-two avg-trig-len))
         (sample-ar (make-array ar-sample-len :initial-element 0.0d0))
         (slices nil))
    (setf *last-fmcw-edges* trigger-edges)

    (when debug-p
      (format t "--- read-and-slice-stereo-wav: ~a edges in, avg trigger len is ~a, using ar len ~a.~%"
              (length trigger-edges) avg-trig-len ar-sample-len))

    ;; - copy from right chan into sample-ar from each edge start to end
    ;; - fft arr
    ;; - complex mags into result ar
    ;; - sum or push result ar

    ;; bordeaux-fft does allow you do pull from a large ar by center and len,
    ;;   but that wont let us pad

    (setf *last-timedomain-slices* nil)

    (if single-out-p ;; TODO: there must be a prettier way to represent this
        ;; define result ar outside loop and bring it each time
        (let ((result-ar (make-array (/ ar-sample-len 2) :initial-element 0.0d0)))
          (loop for n from 0 below (length trigger-edges)
                do
                   (let* ((this-edge (nth n trigger-edges))
                          (this-start (first this-edge))
                          (this-end (rest this-edge))
                          (samples-actual-ar (subseq cl-radar.audio:*last-right-samps* this-start this-end)))
                     (cl-radar.math:dc-center-slice samples-actual-ar)
                     (cl-radar.math:array-copy-into
                      samples-actual-ar
                      0 (array-dimension samples-actual-ar 0)
                      sample-ar)

                     (push sample-ar slices)

                     (let ((fft-r
                             (bordeaux-fft:windowed-fft
                              sample-ar (/ ar-sample-len 2) ar-sample-len)))
                       (cl-radar.math:complex-ar-mags fft-r result-ar t))))
          (setf *last-fft-sum*
                result-ar))
        ;; define result-ar inside loop and push it to results list each time
        (let ((result-ar-list nil)
              ;;(hpf (cl-radar.filter:make-fir-highpass-filter 48000 500 :order 101))
              (hpf (cl-radar.filter:make-iir-highpass-filter 48000 500)) ;; iir looks a lot better
              )
          (loop for n from 0 below (length trigger-edges)
                do
                   (let* ((this-edge (nth n trigger-edges))
                          (this-start (first this-edge))
                          (this-end (rest this-edge))
                          (result-ar (make-array (/ ar-sample-len 2) :initial-element 0.0d0))
                          ;;(result-ar (make-array ar-sample-len :initial-element 0.0d0))
                          (samples-actual-ar
                            (subseq cl-radar.audio:*last-right-samps* this-start this-end)))
                     ;;(format t "-- this trig period is from ~a to ~a, ~a samples, ar ~a.~%"
                     ;;        this-start this-end (- this-end this-start)
                     ;;        (array-dimensions test-ar))
                     ;;(setf cl-user::*a* test-ar)
                     ;;(vgplot:plot (nth 300 cl-radar.fmcw::*last-timedomain-slices*))

                     (setf *last-trigger-samples-raw*
                           (alexandria:copy-array samples-actual-ar))

                     ;; make sure to center slice before zero padding or it goes to hell
                     (cl-radar.math:dc-center-slice samples-actual-ar)

                     ;; filter smaller samples-actual-ar into sample-ar, continuing past end of former
                     (dotimes (i (array-dimension sample-ar 0))
                       (setf (aref sample-ar i)
                             (funcall hpf
                                      (if (< i (array-dimension samples-actual-ar 0))
                                          (aref samples-actual-ar i)
                                          0.0))))

                     (setf *last-trigger-samples-filtered*
                           (alexandria:copy-array samples-actual-ar))

                     ;; copy the samples into the longer, initially 0.0, power-of-two sample array
                     ;;(cl-radar.math:array-copy-into
                     ;; samples-actual-ar
                     ;; 0 (array-dimension samples-actual-ar 0)
                     ;; sample-ar)

                     (push (setf *last-trigger-padded* sample-ar) slices)

                     (let ((fft-r
                             (bordeaux-fft:windowed-fft
                              sample-ar (/ ar-sample-len 2) ar-sample-len)))
                       (cl-radar.math:complex-ar-mags fft-r result-ar nil)
                       (push result-ar result-ar-list))))
          (setf *last-timedomain-slices*
                (nreverse slices))
          (setf *last-fft-slices*
                result-ar-list)))))

#|
sbcl --eval "(ql:quickload 'cl-radar)" --eval '(cl-radar.image:write-slices-to-png (cl-radar.fmcw:read-and-slice-and-fft-stereo-wav :wav-path "/Users/jackc/Projects/cl-radar/data/rad1longernocapnorfblankbetter_stereo_vshort.wav" :single-out-p nil :ac-trig-p t))' --eval "(cl-radar.fmcw::plot-triggered-chunk)"

why does hpf'd data have that giant discontinuity at i=poles/2
|#

;; TODO: all these vars should be better named; they are the signal samples not trigger samps
(defun plot-triggered-chunk ()
  (vgplot:subplot 3 1 0)
  (vgplot:plot *last-trigger-samples-raw*)
  (vgplot:subplot 3 1 1)
  (vgplot:plot *last-trigger-samples-filtered*)
  (vgplot:subplot 3 1 2)
  (vgplot:plot *last-trigger-padded*))


@export
(defun read-and-slice-all (&optional wav-path preserve-globals-p)
  (format t "-- read-and-slice-all: ~a~%" wav-path)

  (cl-radar.audio:read-wav wav-path t) ;; TODO:

  (when (not preserve-globals-p)
    (format t "--- read-and-slice-all: using globals from .audio:~%")
    (setf *last-left-samps* cl-radar.audio:*last-left-samps*)
    (setf *last-right-samps* cl-radar.audio:*last-right-samps*)) ;; TODO:

  (assert *last-left-samps*)
  (assert *last-right-samps*)
  (setf *last-timedomain-slices* nil)
  (setf *slice-n* 0)

  (let ((trigger-samps *last-right-samps*) ;; TODO: this is the reversed config of basement
        (signal-samps *last-left-samps*))
    (when t
      (setf trigger-samps *last-left-samps*)
      (setf signal-samps *last-right-samps*))

    (let* ((edges
             (apply-edge-offsets
              (filter-wack-edges
               (find-edges
                trigger-samps)))))

      (mapcar (lambda (some-edge)
                ;;(format t "--- read and slice all: slice ~a, edge ~a ~%"
                ;;        some-edge (incf *slice-n*))

                ;; TODO: use one ar length the whole time
                (let* ((r (cl-radar.math:zero-pad-and-copy signal-samps
                                                           (first some-edge)
                                                           (rest some-edge)
                                                           nil))
                       (this-edge-width-samples (- (rest some-edge) (first some-edge)))
                       (this-array-len (array-dimension r 0)))
                  (declare (ignore this-edge-width-samples))
                  (declare (ignore this-array-len))

                  (assert r)

                  ;; TODO:
                  ;;(cl-radar.audio:remove-dc-bias r (- this-array-len this-edge-width-samples))

                  (push
                   r
                   *last-timedomain-slices*))
                nil)
              edges)
      (format t "-- Trigger-aligned, zero-padded, and removed dc bias from ~a slices.~%"
              (length *last-timedomain-slices*))
      *last-timedomain-slices*))) ;; TODO:


(defvar *last-some-chunk* nil)

(defun read-and-graph-single-chunk (&optional (chunk-index 400))
  (cl-radar.audio:read-wav) ;; TODO: path

  (assert *last-left-samps*)
  (assert *last-right-samps*)

  (let* ((edges
           (apply-edge-offsets
            (filter-wack-edges
             (find-edges
              *last-left-samps*))))
         (some-edge (nth chunk-index edges))
         (some-chunk
           (setf *last-some-chunk*
                 (cl-radar.math:zero-pad-and-copy *last-right-samps*
                                    (first some-edge)
                                    (rest some-edge))))
         (some-chunk-samples (- (rest some-edge) (first some-edge)))
         (some-chunk-len (array-dimension some-chunk 0)))

    ;;(remove-dc-bias some-chunk (- some-chunk-len some-chunk-samples))
    (vgplot:plot some-chunk)))

;; 'bordeaux-fft:hann
(defun fft-and-graph-last-single-chunk (&optional (chunk *last-some-chunk*))
  (assert *last-some-chunk*)

  (let* ((chunk-len (array-dimension chunk 0))
         (fft-out (bordeaux-fft:windowed-fft
                   chunk (/ chunk-len 2)
                   chunk-len 'bordeaux-fft:blackman-harris))
         (fft-reals (make-real-copy-half fft-out))
         )
    (cl-radar.math:complex-ar-mags fft-out fft-reals) ;; complex-ar-mags-incf for layering

    (vgplot:plot fft-reals ";radar dist returns;")

    (when t
      (vgplot:print-plot (pathname "~/ourplot.png"))

      ;;(vgplot:close-plot)
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter +sweep-width-ghz+ 1.0)
(defparameter +sweep-time-sec+ (/ 446 480000.0)) ;; TODO: these are real but measure live from trigger
(defparameter +sweep-ghz-per-sec+ (/ +sweep-width-ghz+ +sweep-time-sec+))

(defparameter +phase-velocity-m-per-sec+ (* 3.0 (expt 10 8))) ;; TODO: coef

(defparameter +m-per-hz+ (/ +phase-velocity-m-per-sec+
                            (* +sweep-ghz-per-sec+ 1000000000)))

;; wrong data domain but works:
;; (vgplot:plot (make-vgplot-x-axis *last-summed-ar* #'fmcw-dist-from-hz ) *last-summed-ar* ";radar dist returns;")

(defvar *last-fft-length* 512) ;; input length, not (half) output len
;;(defvar *last-sample-rate* nil)

;; example: fft len 512, at 48,000 hz:
;; - bin 0: 0 hz
;; - bin 1: 93.75 hz // (* (/ 1 512) 48000.0)
;; - bin 51: 2.4 khz
;; - bin 128: 12 khz
;; - bin 196: 18 khz
;; - bin 256: 24 khz
(defun fft-bin-num-to-hz (bin-idx) ;; TODO: util pkg
  ;;(setf *last-sample-rate* cl-radar.audio:*last-sample-rate*) ;; TODO: !!

  (assert cl-radar.audio:*last-sample-rate*)
  (assert *last-fft-length*) ;; the full length in, not the half length out

  (*
   (/ bin-idx (float *last-fft-length*))
   cl-radar.audio:*last-sample-rate*))

(defun fmcw-dist-from-hz (signal-hz)
  (assert +m-per-hz+)
  (* signal-hz +m-per-hz+))

(defun fmcw-dist-from-bin (bin-idx)
  (fmcw-dist-from-hz
   (fft-bin-num-to-hz
    bin-idx)))

(defun make-vgplot-x-axis (y-ar x-label-fn)
  (let ((r (make-array (array-dimension y-ar 0) :initial-element 0.0d0)))
    (loop for i from 0 below (array-dimension r 0)
          do
             (setf (aref r i)
                   (apply x-label-fn (list i))))
    r))

(defvar *this-last-fft-res* nil)

(defun plot-fft-of-slice (ar)
  (let* ((r (setf *this-last-fft-res*
                  (bordeaux-fft:windowed-fft ar 256 512)))
         (fft-good-half (make-real-copy-half r)))

    (cl-radar.math:complex-ar-mags r fft-good-half)

    (setf *last-fft-length* 512)

    (vgplot:plot
     (make-vgplot-x-axis
      fft-good-half
      #'fmcw-dist-from-bin
      ;;#'fft-bin-num-to-hz
      )
     fft-good-half
     ";radar dist returns;")))


(defparameter +trigger-offset-start-samples+ 16) ;; samples between trigger edge and chunk start
(defparameter +trigger-offset-end-samples+ 0) ;; samples back to end before trigger cross found
;; jc 2024-12-24 seems ok

(defvar *last-edges* nil)

(defparameter +filter-edgeavg-diff+ 0.25) ;; 0.1 == 10% diff from avg trigger

;;(defparameter +min-edge-pos-difference+ 100) ;; deprecated

(defun edge-list-avg-length (edges-list)
  (/ (reduce #'+
             (mapcar (lambda (e) (- (rest e) (first e)))
                     edges-list))
     (float (length edges-list))))

(defun filter-wack-edges (edges-list &optional debug-p)
  (let* ((pos-edge-lens ;; average length in samples of non-negative or zero periods
           (remove-if
            (lambda (n)
              (<= n 0.0))
            (mapcar
             (lambda (e) (- (rest e) (first e)))
             edges-list)))
         (edge-len-avg
           (/ (reduce #'+ pos-edge-lens)
              (float (length pos-edge-lens)))))

    (when (not (= (length edges-list) (length pos-edge-lens)))
      (format t "-- filter-wack-edges: !!! got ~a edges with <=0 length.~%"
              (- (length edges-list) (length pos-edge-lens))))

    (let* ((good-edges
            (remove-if
             #'not
             (loop for n from 0 below (length edges-list)
                   collecting
                   (let* ((this-e (nth n edges-list))
                          (e-start (first this-e))
                          (e-end (rest this-e))
                          (e-len (- e-end e-start))
                          (diff-from-mean-samps (abs (- e-len edge-len-avg)))
                          (diff-from-mean-ratio (/ diff-from-mean-samps edge-len-avg)))

                     (if (> diff-from-mean-ratio +filter-edgeavg-diff+)
                         (progn
                           (when debug-p
                             (format t "---- FWE: pos-mean ~a, this-e len ~a, diff: ~a samples, ~a ratio~%"
                                     edge-len-avg e-len diff-from-mean-samps
                                     diff-from-mean-ratio))
                           nil)
                         this-e)

                     ;; this does the same as that but just with a constant min length
                     #|(if (>= +min-edge-pos-difference+ (- e-end e-start))
                         (progn
                           (format t "---- filter-wack-edges: n ~a: ~a - avg ~a, this one ~a, diff from mean ~a, diff-ratio ~a.~%"
                                   n this-e edge-len-avg e-len diff-from-mean-samps diff-from-mean-ratio)
                           nil)
                         this-e)|#

                     ))))
           (avg-samps-per-good-trig (edge-list-avg-length good-edges)))
      (when debug-p
        (format t "-- filter-wack-edges done:  ~a edge pairs in, ~a out, ~a filtered by avg, ~a removed because len <=0.~%"
                (length edges-list) (length good-edges)
                (- (length pos-edge-lens) (length good-edges))
                (- (length edges-list) (length pos-edge-lens)))
        (format t "-- filter-wack-edges:       avg period in was ~a, good edge avg is ~a.~%"
                edge-len-avg avg-samps-per-good-trig))

      (setf (getf *last-stats* :avg-raw-trig-period) edge-len-avg)
      (setf (getf *last-stats* :avg-good-trig-period) avg-samps-per-good-trig)
      (setf (getf *last-stats* :trig-periods-in) (length edges-list))
      (setf (getf *last-stats* :trig-periods-kept) (length good-edges))
      (setf (getf *last-stats* :trig-periods-filtered)
            (- (length edges-list) (length good-edges)))

      (values
       good-edges
       edge-len-avg avg-samps-per-good-trig))))

;; apply trigger offsets to each edge pair
@export
(defun apply-edge-offsets (edges-list &key (start-offset +trigger-offset-start-samples+)
                                        (end-offset +trigger-offset-end-samples+)
                                        (debug-p t))
  (assert (> (length edges-list) 0))

  ;; add start-offset to each edge start index (start . end)
  ;; subtract end-offset from each end index
  (let* ((adjusted-edges
           (setf
            *last-edges*
            (mapcar
             (lambda (es) (cons (+ (first es) start-offset)
                                (- (rest es) end-offset)))
             edges-list)))
         (avg-samps-per-chunk
           (edge-list-avg-length
            adjusted-edges))
         (avg-sec-per-chunk
           (/ avg-samps-per-chunk (float cl-radar.audio:*last-sample-rate*))))

    (when debug-p
      (format t "--- post apply-edge-offsets: average samples per chunk/window/frame/trigger: ~a ( ~a sec at ~a hz)~%"
              avg-samps-per-chunk avg-sec-per-chunk cl-radar.audio:*last-sample-rate*)
      (let ((pad-len (+ start-offset end-offset)))
        (format t "--- post apply-edge-offsets:   (offset pad total is ~a samples; ~a + ~a is ~a.)~%"
                pad-len avg-samps-per-chunk pad-len (+ avg-samps-per-chunk pad-len))))

    (setf (getf *last-stats* :avg-offset-trig-period) avg-samps-per-chunk)
    (setf (getf *last-stats* :avg-offset-trig-period-sec) avg-sec-per-chunk)

    adjusted-edges))

#|

chopper is now apply-edge-offsets!


CL-USER> (progn (chopper *last-found-edges*) 7)
--- post-chopper: average samples per chunk/window/frame/trigger: 445.59717 ( 0.009283274 sec at 48000 hz)
7
CL-USER>
|#

(defvar *last-found-edges* nil)

;; return list of (rising-i . falling-i)
(defun find-edges (trigger-ar &optional debug-p)
  (assert
   (= 1 (length (array-dimensions trigger-ar))))

  ;; TODO: skip fwd by trigger offset at edges?
  ;; maybe skip 90% expected blank time too?
  ;; edge searching like this is slow.

  #| ;; tldr its fine ^:

  CL-USER> (time (progn (find-edges *last-left-samps*) 7))
  --- Found 777 chunks (windows? frames? triggers?) with start and end.
  --- (Data ended while searching for close to chunk starting at 593946.
  )--- Average samples per chunk/window/frame/trigger: 509.59717 ( 0.010616608 sec at 48000 hz)
  Evaluation took:
  0.019 seconds of real time
  0.019112 seconds of total run time (0.019098 user, 0.000014 system)
  100.00% CPU
  44,029,800 processor cycles
  32,512 bytes consed

  7
  CL-USER>
|#

  (let ((r '())
        (this-start nil)
        (last-start nil)
        (total-starts 0)
        (start-period-sum 0))
    (loop for i from 2 below (array-dimension trigger-ar 0)
          do
             (if (not this-start)
                 (when (and (< (aref trigger-ar (- i 1)) 0.0) ;; rising edges
                            (> (aref trigger-ar i) 0.0))
                   (setf this-start i)

                   (when last-start
                     (incf start-period-sum
                           (- this-start last-start))
                     (incf total-starts))

                   (setf last-start this-start))
                 (when (and (> (aref trigger-ar (- i 1)) 0.0) ;; falling edges
                            (< (aref trigger-ar i) 0.0))
                   (push (cons this-start i) r)
                   (setf this-start nil))))

    (when debug-p
      (format t "--- Found ~a chunks (windows? frames? triggers?) with start and end.~%" (length r)))
    (when (and this-start debug-p)
      (format t "--- (Data ended while searching for close to chunk starting at ~a.~%)"
              this-start))

    (assert cl-radar.audio:*last-sample-rate*)
    ;;(setf *last-sample-rate* cl-radar.audio:*last-sample-rate*)

    (let* (;;(avg-samps-per-chunk (edge-list-avg-length r))
           ;;(avg-sec-per-chunk
           ;; (/ avg-samps-per-chunk (float cl-radar.audio:*last-sample-rate*)))
           (avg-samps-per-trig (/ start-period-sum (float total-starts)))
           (avg-sec-per-trig
             (/ avg-samps-per-trig (float cl-radar.audio:*last-sample-rate*)))
           (avg-trig-freq (/ 1.0 avg-sec-per-trig)))

      (setf (getf *last-stats* :avg-rising-trig-period) avg-samps-per-trig)
      (setf (getf *last-stats* :avg-rising-trig-period-sec) avg-sec-per-trig)
      (setf (getf *last-stats* :avg-rising-trig-freq) avg-trig-freq)


      ;; TODO: might be better for calcs to go after filter-wack-edges
      ;;(format t "--- Average samples per chunk/window/frame/trigger: ~a ( ~a sec at ~a hz)~%"
      ;;        avg-samps-per-chunk avg-sec-per-chunk cl-radar.audio:*last-sample-rate*)
      (when debug-p
        (format t "--- Average samples between rising edges: ~a ( ~a sec at ~a hz, or ~a Hz.)~%"
                avg-samps-per-trig avg-sec-per-trig cl-radar.audio:*last-sample-rate* avg-trig-freq))

      (values
       (setf *last-found-edges*
             (reverse r))
       avg-samps-per-trig))))


@export
(defun find-ac-edges (trigger-ar &key (trig-rising-thresh 0.5) ;; as ratios of max and min
                                   (trig-falling-thresh 0.3)   ;;   respectively
                                   (min-slope 0.1)            ;; val per sample
                                   (debug-p nil))
  (multiple-value-bind
        (max min)
      (cl-radar.math:array-max-min trigger-ar)
    (let* ((rising-thresh-val (* max trig-rising-thresh))
           (falling-thresh-val (* min trig-falling-thresh))
           (starting-edge nil)
           (r nil))
      (assert (< rising-thresh-val max))
      (assert (< falling-thresh-val rising-thresh-val))
      (assert (< min falling-thresh-val))

      (format t "-- max ~a, thresh ~a~%"
              max rising-thresh-val)
      (format t "-- min ~a, thresh ~a~%"
              min falling-thresh-val)

      (loop for i from 1 below (array-dimension trigger-ar 0)
            do
               (let ((this-val (aref trigger-ar i))
                     (last-val (aref trigger-ar (1- i))))
                 (when (and (< this-val falling-thresh-val)
                            (> last-val (+ this-val min-slope))) ;; slope trails current val
                   (cond
                     (starting-edge
                      (push (cons starting-edge i) r)
                      (setf starting-edge nil)
                      (when debug-p
                        (format t "-- found falling edge at ~a.~%" i)))
                     (t
                      (when debug-p
                        (format t "-- found falling edge at ~a but not triggered.~%" i)))))
                 (when (and (> this-val rising-thresh-val)
                            (< last-val (- this-val min-slope)))
                   (cond
                     ((not starting-edge)
                      (setf starting-edge i)
                      (when debug-p
                        (format t "-- found rising edge at ~a.~%" i)))
                     (t
                      (when debug-p
                        (format t "-- found rising edge at ~a but currently triggered.~%" i)))))))
      (nreverse r))))

(defvar *last-plotted-chunk* nil)

;; plots array subseq of *last-right-samps* defined by edges (start . end)
(defun plot-chunk (edge-cons)
  (let ((ar
          (setf *last-plotted-chunk*
                (cl-radar.math:subseq-array *last-right-samps*
                              (first edge-cons)
                              (- (rest edge-cons) (first edge-cons))))))
    (vgplot:plot ar)))



;;;;;;;;;;;;;;;;

;; '((10.0 0.7 10.0)) angle in radians, + is cw

(defun array-slice (arr row)
  (make-array (array-dimension arr 1)
              :displaced-to arr
              :displaced-index-offset (* row (array-dimension arr 1))))

(defun carray-reals (complex-ar)
  (let ((r (make-array (array-dimension complex-ar 0) :initial-element 0.0d0)))
    (loop for i from 0 below (array-dimension complex-ar 0)
          do
             (setf (aref r i)
                   (realpart (aref complex-ar i))))
    r))

;; TODO: move all this crap to math or util

#|
CL-USER> (cl-radar.fmcw::carray-just-reals #(#c(3.3 4.4) #C(2.1 2.3)))
#(#C(3.3 0.0) #C(2.1 0.0))
|#

;; return a complex array of same size but 0.0j
(defun carray-just-reals (complex-ar)
  (let ((r (make-array (array-dimension complex-ar 0) :initial-element #C(0.0d0 0.0d0))))
    (loop for i from 0 below (array-dimension complex-ar 0)
          do
             (setf (aref r i)
                   (complex (realpart (aref complex-ar i)) 0)))
    r))

#|
CL-USER> (make-array '(4 6) :initial-element #C(1.1 2.2))
#2A((#C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2))
(#C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2))
(#C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2))
(#C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2) #C(1.1 2.2)))
CL-USER> (cl-radar.fmcw::2d-carray-just-reals (make-array '(4 6) :initial-element #C(1.1 2.2)))
#2A((#C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0))
(#C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0))
(#C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0))
(#C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0) #C(1.1 0.0)))
|#

(defun 2d-carray-just-reals (complex-ar)
  (let ((r (make-array (array-dimensions complex-ar) :initial-element #C(0.0d0 0.0d0))))
    (loop for i from 0 below (array-dimension complex-ar 0)
          do
             (loop for j from 0 below (array-dimension complex-ar 1)
                   do
                      (setf (aref r i j)
                            (complex (realpart (aref complex-ar i j)) 0))))
    r))

;; TODO: if window length if over some limit by x, downsample it so we dont
;;  get too many fft bins
(defun make-radial-graph-data (&key (num-radials 17)
                                 (carrier-freq-hz 10.0d9)
                                 (element-spacing-m 0.01)
                                 (reals-only-p nil))
  (let* ((wavelength-m (/ 3.0d8 carrier-freq-hz))
         (rx-data (cl-radar.gen::generate-fmcw-if-samples
                   ;;'((10.0 -55.0 10.0) (5.0 45.0 10.0))
                   '((10.0 0.0 10.0)
                     (30.0 45.0 5.0)
                     (50 -50 8.0)
                     (70 -12 10.0)
                     )
                   :element-spacing element-spacing-m
                   :center-frequency carrier-freq-hz
                   :bandwidth 1d9
                   :sweep-time 0.02
                   :sample-frequency 48000
                   :num-elements 24
                   :noise-std 20.0))
         (rx-data-reals (2d-carray-just-reals rx-data))
         (buflen (array-dimension rx-data 1))
         (fftlen
           (max (expt 2 (1- (nth-value 1 (cl-radar.math:next-power-of-two buflen))))
                1024))
         (radial-angle (/ 180.0 num-radials)) ; sector width
         (end-angle-offset (/ radial-angle 2))
         (results nil))
    (format t "---- radial-angle ~a, end-angle-offset ~a. wavelen is ~a m, cf ~a mhz.~%"
            radial-angle end-angle-offset wavelength-m (/ carrier-freq-hz 1d6))
    ;; angle should be the center of the sector
    (loop for angle from (- end-angle-offset 90.0) to (- 90.0 end-angle-offset) by radial-angle
          do
             (let* ((angle-data
                      (cl-radar.gen::beam-angle-sum-2d-array
                       (if reals-only-p
                           rx-data-reals
                           rx-data)
                       angle
                       :element-spacing element-spacing-m
                       :wavelength wavelength-m))
                    (fft-data (bordeaux-fft:windowed-fft angle-data (/ fftlen 2) fftlen))
                    (fft-mags (make-array (/ fftlen 2) :initial-element 0.0d0)))
               (format t "-- adding data at angle ~a (~a samples)~%"
                       angle (array-dimensions fft-data))

               (push (cl-radar.math:complex-ar-mags fft-data fft-mags) results)))
    (nreverse results)))

;; TODO: with reals only, radial graph shows mirror images across centerline. looks like
;;  its in beam sum? try interpolating delay for reals-only case instead of complex rotating delay

(defun write-radial-graph-data ()
  (cl-radar.util:write-string-to-js-file
   "/Users/jackc/Projects/html5-display-widgets/data/radialfft.js"
   "radialfft"
   (cl-json:encode-json-to-string
    (make-radial-graph-data :reals-only-p t)))
  (format t "k.~%"))

(defun gen-thing ()
  (let* ((rx-data (cl-radar.gen::generate-fmcw-if-samples
                  '((3.0 45.0 10.0)
                    ;;(0.5 -0.65 10.0)
                    )
                  :element-spacing 0.01
                  :center-frequency 11d9
                  :bandwidth 1d9
                  :sweep-time 0.02
                  :sample-frequency 48000
                  :num-elements 4))
         (ant2 (array-slice rx-data 2))
         (mags
           (cl-radar.math:complex-ar-mags
            ant2
            (make-array (/ (array-dimension ant2 0) 2) :initial-element 0.0d0))))
    (format t "--- rxdata ~a~%" (array-dimensions rx-data))
    (format t "--- ant2 ~a.~%" (array-dimensions ant2))
    (format t "---mags ~a~%" (array-dimensions mags))

    (vgplot:plot (subseq (carray-reals ant2) 0 100))
    ;;(vgplot:plot (subseq mags 0 100))
    ))
