(ql:quickload '(cl-portaudio cl-wav bordeaux-fft vgplot osicat))


;;;;;;;; spot to run it is near :450
;;; new mean downsampler at :570


;;; moved these defs up here to fix compile


(defparameter +default-path+ "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/")

;; TODO: why do these need to be up here to compile??
(defvar *last-wav* nil)
(defvar *last-left-samps* nil)
(defvar *last-right-samps* nil)
(defvar *last-sample-rate* nil)
(defvar *last-timedomain-slices* nil)
(defvar *loop-fft-sums* nil)



;;;;;;;;;;;;;

;; see clvar/var/pa-printdevices.lisp- still broken upsteam 2022-11-13
(defconstant +frames-per-buffer+ 1024)
(defconstant +sample-rate+ 44100d0)
(defconstant +seconds+ 15)
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 1) ; 2

(defparameter +run-secs+ 5)

(defvar *r2* '())
(defvar *maxes* '())

(defun snag-audio ()
  (setf *maxes* nil)
  (setf *r2* nil)

  (pa:with-audio
    (let ((input-parameters (pa:make-stream-parameters))
          (n 0))

      ;; --- same as example ---
      (setf (pa:stream-parameters-device input-parameters)
            (pa:get-default-input-device)
            (pa:stream-parameters-channel-count input-parameters) +num-channels+
            (pa:stream-parameters-sample-format input-parameters) +sample-format+
            (pa:stream-parameters-suggested-latency input-parameters)
            (pa:device-info-default-low-input-latency
             (pa:get-device-info
              (pa:get-default-input-device))))


      (format t "~%=== Wire on. Will run ~D seconds . ===~%" +run-secs+)

      (let ((input-buffers '()))
        ;;(format t "-- inited ~%")
        (pa:with-audio-stream
            (astream input-parameters nil
             :sample-rate +sample-rate+
             :frames-per-buffer +frames-per-buffer+
             :stream-flags (:clip-off))
          ;;(format t "-- opened port ~%")
          (dotimes (i (round (/ (* +run-secs+ +sample-rate+) +frames-per-buffer+)))
            (let ((inbuf (pa:read-stream astream))
                  (thismax 0.0))

              (push inbuf input-buffers)

              (loop for i from 0 below (first (array-dimensions inbuf))
                    do
                       (when (> (aref inbuf i) thismax)
                         (setf thismax (aref inbuf i))))
              (push thismax *maxes*)

              (incf n))))
        (setf *r2* input-buffers))
      (format t "-- ~a chunks of ~a samples~%" n (array-dimension (first *r2*) 0))
      (format t "-- ( ~a samples total, which at ~a hz is ~$ sec. )~%"
              (* n (array-dimension (first *r2*) 0)) +sample-rate+
              (/ (* n (array-dimension (first *r2*) 0)) (float +sample-rate+)))
      (format t "-- max of maxes is ~a.~%"
              (apply #'max *maxes*)))))

;; (snag-audio)
;; (setf r (bordeaux-fft:windowed-fft (first *r2*) 512 1024 'bordeaux-fft:blackman-harris))
;;

(defun complex-ar-mags (complex-src real-dst)
  ;;(assert (=(array-dimension complex-src 0)
  ;;          (array-dimension real-dst 0)))

  (when (not (= (array-dimension complex-src 0)
                (array-dimension real-dst 0)))
    (format t "!!! complex-src len ~a, real-dst len ~a.~%"
            (array-dimension complex-src 0)
            (array-dimension real-dst 0)))

  ;; just copy up as far as dst goes, because we only care about the first
  ;;   half of fft data
  (loop for i from 0 below (array-dimension real-dst 0)
       do
       (setf (aref real-dst i)
             (sqrt (+ (expt (realpart (aref complex-src i)) 2)
                      (expt (imagpart (aref complex-src i)) 2))))))

(defun complex-ar-mags-incf (complex-src real-dst)
  ;; just copy up as far as dst goes, because we only care about the first
  ;;   half of fft data
  (loop for i from 0 below (array-dimension real-dst 0)
        do
           (incf (aref real-dst i)
                 (sqrt (+ (expt (realpart (aref complex-src i)) 2)
                          (expt (imagpart (aref complex-src i)) 2))))))

;; make new array half lenth but not-complex values
;; TODO: is this used?
(defun make-real-copy-half (complex-src)
  (make-array
   (/ (array-dimension complex-src 0) 2)
   :initial-element 0.0d0))

(defvar *fft-data*)
(defvar *fft-data-real*)

(defun slurp-and-plot-fft ()
  (snag-audio) ;; sets *r2*
  (setf *fft-data*
        (bordeaux-fft:windowed-fft (first *r2*) 512 1024 'bordeaux-fft:blackman-harris))
  (setf *fft-data-real* (make-real-copy-half *fft-data*))
  (complex-ar-mags-incf *fft-data* *fft-data-real*)
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
             (complex-ar-mags-incf *fft-data* *fft-data-real*)))
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



#|
Function: print-plot (filename &key terminal)

Print the actual plot into filename (a pathname).
Use the (optional) terminal or if not provided,
use the extension of filename to guess the terminal type.

Guessing of terminals works currently for: gif, pdf, png

Examples: (vgplot:print-plot #p"plot.pdf")

(vgplot:print-plot #p"plot.eps" :terminal "epscairo")

It is possible to give additional parameters inside the terminal parameter, e.g.:

(vgplot:print-plot #p"plot.pdf" :terminal "pdfcairo size \"5cm\",\"5cm\"")
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

ffmpeg:

linux:

arecord -L # from alsa utils

ffmpeg -f alsa -i "hw:CARD=CODEC,DEV=0" -ac 2 -r 4800 -t 10 out.wav

osx:

ffmpeg -f avfoundation -list_devices true -i "" 2>&1 |tail -7
[AVFoundation indev @ 0x7fbd6c6254c0] AVFoundation video devices:
[AVFoundation indev @ 0x7fbd6c6254c0] [0] FaceTime HD Camera (Built-in)
[AVFoundation indev @ 0x7fbd6c6254c0] [1] Capture screen 0
[AVFoundation indev @ 0x7fbd6c6254c0] AVFoundation audio devices:
[AVFoundation indev @ 0x7fbd6c6254c0] [0] MacBook Pro Microphone
[AVFoundation indev @ 0x7fbd6c6254c0] [1] USB Audio CODEC

ffmpeg -f avfoundation -ac 2 -r 48000 -i ":1" -t 10 out.wav

CL-USER> (uiop:run-program (list "echo" "hi moose") :output :lines)
("hi moose")
NIL
0
CL-USER> (uiop:run-program (list "echo" "hi moose") :output :string)
"hi moose
"
NIL
0
CL-USER>

https://lispcookbook.github.io/cl-cookbook/os.html
https://asdf.common-lisp.dev/asdf/Some-Utility-Functions.html
|#

#|
TERM=dumb ffmpeg -f avfoundation -i :0 -t 10 -y /Users/jackc/out.wav \
-hide_banner -nostats -nostdin -loglevel debug -report </dev/null

#ffmpeg -f avfoundation -ac 2 -r 48000 -i :1 -t 10 -y /Users/jackc/out.wav \
#    -hide_banner -nostats -nostdin </dev/null

ffmpeg -f avfoundation -ac 2 -ar 48000 -i :1 -t 10 -y /tmp/out.wav \
-hide_banner -nostdin
|#

#|
when emacs run from terminal:

CL-USER> (uiop:run-program (ffmpeg-cmd-for-platform) :output :string :error-output :string :ignore-error-status t)

""
"Input #0, avfoundation, from ':0':
Duration: N/A, start: 217327.993651, bitrate: 1411 kb/s
Stream #0:0: Audio: pcm_f32le, 44100 Hz, mono, flt, 1411 kb/s
Stream mapping:
Stream #0:0 -> #0:0 (pcm_f32le (native) -> pcm_s16le (native))
Output #0, wav, to '/tmp/out.wav':
Metadata:
ISFT            : Lavf59.27.100
Stream #0:0: Audio: pcm_s16le ([1][0][0][0] / 0x0001), 44100 Hz, mono, s16, 705 kb/s
Metadata:
encoder         : Lavc59.37.100 pcm_s16le
size=     753kB time=00:00:10.00 bitrate= 617.2kbits/s speed=0.999x
video:0kB audio:753kB subtitle:0kB other streams:0kB global headers:0kB muxing overhead: 0.010111%
"
0
|#

(defparameter +wav-write-path+ "/tmp/out-writing.wav")
(defparameter +wav-process-path+ "/tmp/out.wav")

(defun ffmpeg-cmd-for-platform ()
  #|
  [jackc@nichor] ~ :) $ ffmpeg -f avfoundation -list_devices true -i "" 2>&1 |tail -7
  [AVFoundation indev @ 0x7fb1d800b980] AVFoundation video devices:
  [AVFoundation indev @ 0x7fb1d800b980] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7fb1d800b980] [1] Capture screen 0
  [AVFoundation indev @ 0x7fb1d800b980] AVFoundation audio devices:
  [AVFoundation indev @ 0x7fb1d800b980] [0] USB Audio CODEC
  [AVFoundation indev @ 0x7fb1d800b980] [1] MacBook Pro Microphone
  : Input/output error
  [jackc@nichor] ~ :) $
  |#

  (cond
    ((member :darwin *features*)
     (format nil "/usr/local/bin/ffmpeg -f avfoundation -ac 2 -i :0 -t 1 -y -hide_banner -nostats -nostdin ~a" +wav-write-path+)
     ;;(format nil "/usr/local/bin/ffmpeg -f avfoundation -i :0 -t 10 -y -hide_banner -nostats -nostdin ~a" +wav-write-path+)
     )
    ((nth-value 2 (uiop:run-program "uname -a|grep Linux" :ignore-error-status t)) ;; TODO:
     "ffmpeg -f alsa -i \"hw:CARD=CODEC,DEV=0\" -ac 2 -ar 48000 -t 10 -y /tmp/out.wav")
    (t
     (format t "!!! unrecognized platform!~%"))))

#|
(defparameter +env+
  '(
   (:TERM_PROGRAM . "Apple_Terminal")
   (:SHELL . "/bin/bash")
   (:TERM . "xterm-256color")
   (:TMPDIR . "/tmp/")
   (:TERM_PROGRAM_VERSION . "440")
   ;;(:TERM_SESSION_ID . "37A193B0-73BB-4F03-AFD4-9EEBD6835FCB")
   (:USER . "jackc")
   ;;(:SSH_AUTH_SOCK . "/private/tmp/com.apple.launchd.uONlChOABB/Listeners")
   (:PATH . "/Users/jackc/Projects/z80asm:/Users/jackc/Projects/z80sim:/usr/local/opt/python@3.9/libexec/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/VMware Fusion.app/Contents/Public")
   ;;(:LaunchInstanceID . "E09C23B6-BE5E-4774-A381-D902BD87EC55")
   (:PWD . "/Users/jackc")
   (:LANG . "en_US.UTF-8")
   (:XPC_FLAGS . "0x0")
   (:XPC_SERVICE_NAME . "0")
   (:SHLVL . "2")
   (:HOME . "/Users/jackc")
   (:LOGNAME . "jackc")
   ;;(:SECURITYSESSIONID . "186aa")
   (:OLDPWD . "/Users/jackc/Projects")))
|#

#|
env|awk '/^[a-zA-Z]/'|sed 's/^\(.*\)=\(.*\)$/(:\1 \. |\2|)/'|sed 's,|,",g'

(:TERM_PROGRAM . "Apple_Terminal")
(:SHELL . "/bin/zsh")
(:TERM . "xterm-256color")
(:TMPDIR . "/var/folders/yx/b7gk4vcx38xg7jjk_61mkf1w0000gn/T/")
(:TERM_PROGRAM_VERSION . "440")
(:TERM_SESSION_ID . "37A193B0-73BB-4F03-AFD4-9EEBD6835FCB")
(:USER . "jackc")
(:SSH_AUTH_SOCK . "/private/tmp/com.apple.launchd.uONlChOABB/Listeners")
(:PATH . "/Users/jackc/Projects/z80asm:/Users/jackc/Projects/z80sim:/usr/local/opt/python@3.9/libexec/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/VMware Fusion.app/Contents/Public")
(:LaunchInstanceID . "E09C23B6-BE5E-4774-A381-D902BD87EC55")
(:PWD . "/Users/jackc")
(:LANG . "en_US.UTF-8")
(:XPC_FLAGS . "0x0")
(:XPC_SERVICE_NAME . "0")
(:SHLVL . "2")
(:HOME . "/Users/jackc")
(:LOGNAME . "jackc")
(:SECURITYSESSIONID . "186aa")
(:OLDPWD . "/Users/jackc/Projects/txeth/Project09_Test_Ethernet_cyc4")

|#

(defun looping-capture-and-plot ()
  (loop for i from 0 below 1000
        do
           (let ((ffmpeg-cmd (ffmpeg-cmd-for-platform)))
             (format t "-- Running capture cmd: ~a~%" ffmpeg-cmd)

             (multiple-value-bind (errstr outstr retval)
                 (uiop:run-program (ffmpeg-cmd-for-platform)
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

  ;; fft each slice, calc mags
  ;; sum fft of slice with the rest of this capture
  (fft-and-sum *last-timedomain-slices*)

  (when log-scale-p
    (dB-ulize-array *loop-fft-sums*))

  ;; plot with correct x axis labels
  (vgplot:plot
   (make-vgplot-x-axis
    *loop-fft-sums*
    #'fmcw-dist-from-bin
    ;;#'fft-bin-num-to-hz
    )
   *loop-fft-sums*
   ";radar dist returns (ft);")

  ;; TODO: save as png
  )

(defun file-mtime (fpath)
  (osicat-posix:stat-mtime
   (osicat-posix:stat fpath)))

(defparameter +watched-wav-path+ "/Users/jackc/out.wav")
(defvar *last-wav-mtime* nil)

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

(defvar *n* 0)

(defun single-capture-and-plot (&key (log-scale-p nil))

  ;; capture audio and stick somewhere

  ;;(read-and-slice-all "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/1-fullchain_stereo.wav")
  (read-and-slice-all "/Users/jackc/out.wav")

  ;; fft each chunk, calc mags
  ;; sum fft of slice with the rest of this capture
  (fft-and-sum *last-timedomain-slices*)
  (when log-scale-p
    (dB-ulize-array *loop-fft-sums*))

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
  *loop-fft-sums*
  #'fmcw-dist-from-bin
  ;;#'fft-bin-num-to-hz
  )
  |#


  (vgplot:plot
   (make-vgplot-x-axis
    *loop-fft-sums*
    #'fmcw-dist-from-bin
    ;;#'fft-bin-num-to-hz
    )
   *loop-fft-sums*

   #|
   (make-vgplot-x-axis
    *loop-fft-sums*
    ;;#'fmcw-dist-from-bin
    #'fft-bin-num-to-hz
    )
   *loop-fft-sums*
   |#
   ";axes x1y1;")

  ;; save as png

  )

;; ref its highest value
(defun dB-ulize-array (in-ar)
  (let ((ar-len (array-dimension in-ar 0))
        (ar-max (aref in-ar 0)))
    (loop for i from 0 below ar-len
          do
             (when (> (aref in-ar i) ar-max)
               (setf ar-max (aref in-ar i))))
    (loop for i from 0 below ar-len
          do
             (setf (aref in-ar i)
                   (*
                    (log
                     (/
                      (aref in-ar i)
                      ar-max)
                     10)
                    10)))))


;; future: support non-integer downsampling (weighted-mean)

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

CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1))
- in ar has 14 samples.
- out ar should have 4 samples (trimming 2 of input)
-- ind-list (0 1 2)
-- sum 6.6000004
-- new-val 2.2
-- ind-list (3 4 5)
-- sum 16.5
-- new-val 5.5
-- ind-list (6 7 8)
-- sum 26.4
-- new-val 8.8
-- ind-list (9 10 11)
-- sum 5.2
-- new-val 1.7333332
#(2.2 5.5 8.8 1.7333332)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2))
- in ar has 15 samples.
- out ar should have 5 samples (trimming 0 of input)
-- ind-list (0 1 2)
-- sum 6.6000004
-- new-val 2.2
-- ind-list (3 4 5)
-- sum 16.5
-- new-val 5.5
-- ind-list (6 7 8)
-- sum 26.4
-- new-val 8.8
-- ind-list (9 10 11)
-- sum 5.2
-- new-val 1.7333332
-- ind-list (12 13 14)
-- sum 12.4
-- new-val 4.133333
#(2.2 5.5 8.8 1.7333332 4.133333)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) #(0 0 0 0 0 0 ))
- in ar has 15 samples.
- out ar should have 5 samples (trimming 0 of input)
-- out-ar passed in: (SIMPLE-VECTOR 6)
-- out-ar passed larger than needed! 6 vs 5
-- ind-list (0 1 2)
-- sum 6.6000004
-- new-val 2.2
-- ind-list (3 4 5)
-- sum 16.5
-- new-val 5.5
-- ind-list (6 7 8)
-- sum 26.4
-- new-val 8.8
-- ind-list (9 10 11)
-- sum 5.2
-- new-val 1.7333332
-- ind-list (12 13 14)
-- sum 12.4
-- new-val 4.133333
#(2.2 5.5 8.8 1.7333332 4.133333 0)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) #(0 0 0 0 0 ))
- in ar has 15 samples.
- out ar should have 5 samples (trimming 0 of input)
-- out-ar passed in: (SIMPLE-VECTOR 5)
-- ind-list (0 1 2)
-- sum 6.6000004
-- new-val 2.2
-- ind-list (3 4 5)
-- sum 16.5
-- new-val 5.5
-- ind-list (6 7 8)
-- sum 26.4
-- new-val 8.8
-- ind-list (9 10 11)
-- sum 5.2
-- new-val 1.7333332
-- ind-list (12 13 14)
-- sum 12.4
-- new-val 4.133333
#(2.2 5.5 8.8 1.7333332 4.133333)
CL-USER> (mean-downsampler 3 #(1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 0.0 2.1 3.1 4.1 5.1 3.2) #(0 0 0 0 ))
- in ar has 15 samples.
- out ar should have 5 samples (trimming 0 of input)
-- out-ar passed in: (SIMPLE-VECTOR 4)
-- out-ar passed in is smaller than it could be, truncating! 4 vs 5
-- ind-list (0 1 2)
-- sum 6.6000004
-- new-val 2.2
-- ind-list (3 4 5)
-- sum 16.5
-- new-val 5.5
-- ind-list (6 7 8)
-- sum 26.4
-- new-val 8.8
-- ind-list (9 10 11)
-- sum 5.2
-- new-val 1.7333332
#(2.2 5.5 8.8 1.7333332)
CL-USER>
|#

(defvar *loop-fft-sums* nil)

(defun fft-and-sum (slices-list) ;; TODO: name
  (when (not *loop-fft-sums*)
    (setf *loop-fft-sums*
          (make-array (/ (array-dimension (first slices-list) 0) 2) :initial-element 0.0d0)))
  (loop for n from 0 below (length slices-list)
        do
           (let* ((this-ar (nth n slices-list))
                  (slice-len (array-dimension this-ar 0))
                  (fft-out (bordeaux-fft:windowed-fft
                            this-ar (/ slice-len 2) slice-len
                            'bordeaux-fft:blackman-harris)))
             (if (= 0 n)
                 (complex-ar-mags fft-out *loop-fft-sums*)
                 (complex-ar-mags-incf fft-out *loop-fft-sums*)))))

(defvar *last-summed-ar* nil)

(defun sum-arrays (ar-list)
  (let ((r (make-array (array-dimension (first ar-list) 0) :initial-element 0.0d0)))
    (mapcar
     (lambda (ar)
       (loop for i from 0 below (array-dimension ar 0)
             do
                (incf (aref r i)
                      (aref ar i))))
     ar-list)
    (setf *last-summed-ar* r)
    nil))

(defvar *last-timedomain-slices* nil)
(defvar *slice-n* 0)

(defun read-and-slice-all (&optional wav-path)
  (format t "-- read-and-slice-all: ~a~%" wav-path)
  (read-wav wav-path)

  (assert *last-left-samps*)
  (assert *last-right-samps*)
  (setf *last-timedomain-slices* nil)
  (setf *slice-n* 0)

  (let* ((edges
           (chopper
            (filter-wack-edges
             (find-edges
              *last-left-samps*)))))

    (mapcar (lambda (some-edge)
              ;;(format t "-- read and slice all: slice ~a~%" (incf *slice-n*))
              (let* ((r (zero-pad-and-copy *last-right-samps*
                                           (first some-edge)
                                           (rest some-edge)))
                     (this-edge-width-samples (- (rest some-edge) (first some-edge)))
                     (this-array-len (array-dimension r 0)))
                (remove-dc-bias r (- this-array-len this-edge-width-samples))

                (push
                 r
                 *last-timedomain-slices*))
              nil)
            edges)
    (format t "-- Trigger-aligned, zero-padded, and removed dc bias from ~a slices.~%"
            (length *last-timedomain-slices*))))


(defvar *last-some-chunk* nil)

(defun read-and-graph-single-chunk (&optional (chunk-index 400))
  (read-wav)

  (assert *last-left-samps*)
  (assert *last-right-samps*)

  (let* ((edges (chopper
                (find-edges
                 *last-left-samps*)))
         (some-edge (nth chunk-index edges))
         (some-chunk
           (setf *last-some-chunk*
                 (zero-pad-and-copy *last-right-samps*
                                    (first some-edge)
                                    (rest some-edge))))
         (some-chunk-samples (- (rest some-edge) (first some-edge)))
         (some-chunk-len (array-dimension some-chunk 0)))

    (remove-dc-bias some-chunk (- some-chunk-len some-chunk-samples))
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
    (complex-ar-mags fft-out fft-reals) ;; complex-ar-mags-incf for layering

    (vgplot:plot fft-reals ";radar dist returns;")

    (when t
      (vgplot:print-plot (pathname "~/ourplot.png"))
      ;;(vgplot:close-plot)
      )))

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

#|
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

;; example: fft len 512, at 48,000 hz:
;; - bin 0: 0 hz
;; - bin 1: 93.75 hz // (* (/ 1 512) 48000.0)
;; - bin 51: 2.4 khz
;; - bin 128: 12 khz
;; - bin 196: 18 khz
;; - bin 256: 24 khz
(defun fft-bin-num-to-hz (bin-idx)
  (assert *last-sample-rate*)
  (assert *last-fft-length*) ;; the full length in, not the half length out

  (*
   (/ bin-idx (float *last-fft-length*))
   *last-sample-rate*))

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

    (complex-ar-mags r fft-good-half)

    (setf *last-fft-length* 512)

    (vgplot:plot
     (make-vgplot-x-axis
      fft-good-half
      #'fmcw-dist-from-bin
      ;;#'fft-bin-num-to-hz
      )
     fft-good-half
     ";radar dist returns;")))

;; in-place modify (? doesnt make sense now with pad? or need confined i to i)
;; should go before zero pad
(defun remove-dc-bias (ar &optional rezero-end-len)
  (let* ((ar-avg (array-average ar))
         (ar-avg-inverse (* -1.0 ar-avg)))
    ;;(format t "---- Array avg is ~a; correcting. ~%" ar-avg)

    (loop for i from 0 below (array-dimension ar 0)
          do
             (incf (aref ar i)
                   ar-avg-inverse))

    ;;TODO: ordering so we dont need this
    (when rezero-end-len
      ;;(format t "---- rezeroing end len: setting ~a spots to 0.~%" rezero-end-len)
      (loop for i from (- (array-dimension ar 0) rezero-end-len)
              below (array-dimension ar 0)
            do
            (setf (aref ar i) 0.0)))))

;; grab from buffer into power-of-two sized array
(defun zero-pad-and-copy (src-ar start-i end-i &optional loud-p)
  (when loud-p
    (format t "--- ar type of 0 is ~a~%" (type-of (aref src-ar 0)))

    (format t "-- src len ~a: ~a to ~a ~%" (- end-i start-i) start-i end-i)

    (format t "so: ~a~%"
            (expt 2 (ceiling (/ (float (log (- end-i start-i))) (float (log 2))))))

    (format t "-- made it here~%"))

  (let* ((src-len (- end-i start-i))
         (next-power-of-two
           (expt 2 (ceiling (/ (float (log src-len)) (float (log 2))))))
         (return-ar (make-array next-power-of-two :initial-element 0.0)))
    (when loud-p
      (format t "--- zero pad: size requested is ~a samples, padding to ~a.~%"
              (- end-i start-i) next-power-of-two))
    (loop for i from 0 below src-len
          do
             (progn
               (setf (aref return-ar i)
                     (aref src-ar (+ start-i i)))
               (if (string= (type-of (aref src-ar (+ start-i i)))
                            "REAL")
                   (format t "wtf: ~a~%" (aref src-ar (+ start-i i)) ))))
    return-ar))

;; TODO: these making negative edges!
(defparameter +trigger-offset-start-samples+ 20) ;; samples between trigger edge and chunk start
(defparameter +trigger-offset-end-samples+ 44) ;; samples back to end before trigger cross found

(defvar *last-chopped-edges* nil)

(defparameter +filter-edgeavg-diff+ 0.25) ;; 0.1 == 10% diff from avg

(defun filter-wack-edges (edges-list)

  (let* ((pos-edge-lens ;; TODO: shouldnt have negatives
           (remove-if
            (lambda (n)
              (< n 0.0))
            (mapcar
             (lambda (e) (- (rest e) (first e)))
             edges-list)))
         (edge-len-avg
           (/ (reduce #'+ pos-edge-lens)
              (float (length pos-edge-lens)))))
    #|
    (remove-if
     (lambda (e)
       (>
        (/ (abs (- (rest e) (first e)))
         edge-len-avg)
        +filter-edgeavg-diff+))
     edges-list)
    |#

    (let ((good-edges
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
                         (format t "---- FWE: pos-mean ~a, this-e len ~a, diff: ~a samples, ~a ratio~%"
                                 edge-len-avg e-len diff-from-mean-samps
                                 diff-from-mean-ratio)
                         this-e))))))
      (format t "-- FWE done: avg trigger len ~a samples; ~a edge pairs in, ~a out, ~a filtered.~%"
              edge-len-avg (length edges-list) (length good-edges)
              (- (length edges-list) (length good-edges)))
      good-edges)))

;; apply offsets to edges
(defun chopper (edges-list)
  (assert (> (length edges-list) 0))

  (let* ((filtered-edges (filter-wack-edges edges-list))
         (r
           (setf
            *last-chopped-edges*
            (mapcar
             (lambda (es) (cons (+ (first es) +trigger-offset-start-samples+)
                                (- (rest es) +trigger-offset-end-samples+)))
             filtered-edges)))
         (avg-samps-per-chunk
           (float
            (/
             (reduce #'+
                     (mapcar
                      (lambda (es)
                        (- (rest es) (first es)))
                      r))
             (length r))))
         (avg-sec-per-chunk
           (/ avg-samps-per-chunk (float *last-sample-rate*))))
    (format t "--- post-chopper: average samples per chunk/window/frame/trigger: ~a ( ~a sec at ~a hz)~%"
            avg-samps-per-chunk avg-sec-per-chunk *last-sample-rate*)
    r))

#|
CL-USER> (progn (chopper *last-found-edges*) 7)
--- post-chopper: average samples per chunk/window/frame/trigger: 445.59717 ( 0.009283274 sec at 48000 hz)
7
CL-USER>
|#

(defvar *last-found-edges* nil)

;; return list of (rising-i . falling-i)
(defun find-edges (trigger-ar)
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
        (this-start nil))
    (loop for i from 2 below (array-dimension trigger-ar 0)
          do
             (if (not this-start)
                 (when (and (< (aref trigger-ar (- i 1)) 0.0) ;; rising edges
                            (> (aref trigger-ar i) 0.0))
                   (setf this-start i))
                 (when (and (> (aref trigger-ar (- i 1)) 0.0) ;; falling edges
                            (< (aref trigger-ar i) 0.0))
                   (push (cons this-start i) r)
                   (setf this-start nil))))

    (format t "--- Found ~a chunks (windows? frames? triggers?) with start and end.~%" (length r))
    (when this-start
      (format t "--- (Data ended while searching for close to chunk starting at ~a.~%)"
              this-start))

    (assert *last-sample-rate*)

    (let* ((avg-samps-per-chunk
            (float
             (/
              (reduce #'+
                      (mapcar
                       (lambda (es)
                         (- (rest es) (first es)))
                       r))
              (length r))))
          (avg-sec-per-chunk
            (/ avg-samps-per-chunk (float *last-sample-rate*)))

           )
      (format t "--- Average samples per chunk/window/frame/trigger: ~a ( ~a sec at ~a hz)~%"
              avg-samps-per-chunk avg-sec-per-chunk *last-sample-rate*))

    (setf *last-found-edges*
          r)))
#|
???

CL-USER> (find "fmt" *last-wav* :test (lambda (a b) (format t "~a ~a ~a =? ~a ~a~%" (type-of a) a (type-of b) (getf b :chunk-id) (subseq b 0 4))(string= a (getf b :chunk-id))))
(SIMPLE-ARRAY CHARACTER (3)) fmt CONS =? RIFF (CHUNK-ID RIFF CHUNK-DATA-SIZE
1776710)
(SIMPLE-ARRAY CHARACTER (3)) fmt CONS =? fmt  (CHUNK-ID fmt  CHUNK-DATA-SIZE 16)
(SIMPLE-ARRAY CHARACTER (3)) fmt CONS =? LIST (CHUNK-ID LIST CHUNK-DATA-SIZE 26)
(SIMPLE-ARRAY CHARACTER (3)) fmt CONS =? ISFT (CHUNK-ID ISFT CHUNK-DATA-SIZE 14)
(SIMPLE-ARRAY CHARACTER (3)) fmt CONS =? data (CHUNK-ID data CHUNK-DATA-SIZE
1776640)
NIL


|#




;; expects stereo structure (TODO:)
(defun read-wav (&optional (fpath "/Users/jackc/Projects/radarstuff/data/fmcw_bb/out/1-fullchain_stereo.wav"))
  (format t "-- read-wav: ~a~%" fpath)

  (let* ((wav-obj (setf *last-wav* (wav:read-wav-file
                             fpath
                             :chunk-data-reader (wav:wrap-data-chunk-data-samples-reader))))
         (header-plist (first wav-obj))
         (metadata-plist (second wav-obj))
         (metadata-plist-inner (getf metadata-plist :chunk-data))
         ;;(data-plist (third wav-obj)) ;; for wavs from audacity
         (data-plist (fifth wav-obj)) ;; wavs from ffmpeg
         ;; TODO: nab the data plist automatically (not working for some reason)
         ;;(samples-ar (getf (third wav-obj) :chunk-data)) ;; if single chan?
         ;;(num-samples (first (array-dimensions samples-ar)))
         (sample-rate (setf *last-sample-rate*
                            (getf metadata-plist-inner :sample-rate))))

    ;;(format t "-- loaded ~a samples- at ~a hz thats ~a seconds.~%"
    ;;        num-samples sample-rate (/ num-samples (float sample-rate)))
    (format t "----- 1: ~a~%" header-plist)
    (format t "----- 2: ~a~%" metadata-plist)
    (format t "----- 2 inner: ~a~%" metadata-plist-inner)
    (format t "----- 3: ~a~%" (subseq data-plist 0 (- (length data-plist) 2)))

    ;; TODO:
    (assert (string= "data" (getf data-plist :chunk-id)))
    (assert (= 2 (getf metadata-plist-inner :NUMBER-OF-CHANNELS)))
    (assert (= 16 (getf metadata-plist-inner :SIGNIFICANT-BITS-PER-SAMPLE)))
    (assert (= 1 (getf metadata-plist-inner :COMPRESSION-CODE)))
    (assert (= 4 (getf metadata-plist-inner :BLOCK-ALIGN)))

    (format t "-- oh, this is a stereo wav, deinterleaving... ~%")
    ;;(assert (= 0 (mod num-samples 2)))

    (let* ((reported-samples-in-chunk (getf data-plist :chunk-data-size))
           (samples-ar (getf data-plist :chunk-data))
           (left-ar (make-array (/ reported-samples-in-chunk 2) :initial-element 0.0d0))
           (right-ar (make-array (/ reported-samples-in-chunk 2) :initial-element 0.0d0)))

      (format t "----- chunk reports size of ~a samples- half of that is ~a.~%"
              reported-samples-in-chunk (/ reported-samples-in-chunk 2))
      (format t "----- array we got in chunk has ~a elements.~%" (array-dimension samples-ar 0))

      (loop for i from 0 below (/ (array-dimension samples-ar 0) 2)
            do
               (progn
                 (setf (aref left-ar i)
                       (aref samples-ar (* 2 i)))
                 (setf (aref right-ar i)
                       (aref samples-ar (1+ (* 2 i))))))

      (setf *last-left-samps* left-ar)
      (setf *last-right-samps* right-ar)

      (format t "-- read wav: removing dc bias on left chan!~%")
      (remove-dc-bias *last-left-samps*) ;; TODO!

      (format t "-- ok. now we have two arrays of length ~a samples or ~a seconds.~%"
              (array-dimension left-ar 0)
              (/ (array-dimension left-ar 0) (float sample-rate))))))

(defun subseq-array (ar start len)
  (let ((r (make-array len :initial-element (aref ar 0))))
    (loop for i from 0 below len
          do
             (setf (aref r i)
                   (aref ar (+ i start))))
    r))

(defvar *last-plotted-chunk* nil)

;; plots array subseq of *last-right-samps* defined by edges (start . end)
(defun plot-chunk (edge-cons)
  (let ((ar
          (setf *last-plotted-chunk*
                (subseq-array *last-right-samps*
                              (first edge-cons)
                              (- (rest edge-cons) (first edge-cons))))))
    (vgplot:plot ar)))

(defun array-average (ar)
  (let ((sum 0.0))
    (loop for i from 0 below (array-dimension ar 0)
          do
             (incf sum (aref ar i)))
    (/ sum (array-dimension ar 0))))


#|
reading stereo wav:

CL-USER> (subseq-array *in-ar* 100 10)
#(0.15530396 0.32836914 0.1550293 0.3095398 0.15493774 0.26190186 0.15493774
0.2539673 0.15478516 0.25891113)

appears interleaved
|#

;; TODO: check trigger polarity!

;;;;;;;;;

#|
CL-USER> (read-wav)
----- 1: (CHUNK-ID RIFF CHUNK-DATA-SIZE
                                             2376036 FILE-TYPE WAVE)
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
NIL
CL-USER> (array-dimensions *last-left-samps*)
(1188000)
CL-USER> (progn (find-edges *last-left-samps*) 7)
--- Found 777 chunks with start and end.
--- (Data ended while searching for close to chunk starting at 593946.
)
7
CL-USER> (subseq *last-found-edges* 700 710)
((58628 . 59138) (57864 . 58374) (57101 . 57610) (56337 . 56847)
(55573 . 56083) (54810 . 55319) (54046 . 54556) (53283 . 53792)
(52519 . 53028) (51755 . 52265))
CL-USER> (mapcar (lambda (es) (- (rest es) (first es))) (subseq *last-found-edges* 700 710))
(510 510 509 510 510 509 510 509 509 510)
CL-USER>
CL-USER> (float (/ (reduce #'+ (mapcar (lambda (es) (- (rest es) (first es))) *last-found-edges*)) (length *last-found-edges*)))
509.59717
CL-USER>


CL-USER> (nth 300 *last-found-edges*)
(364087 . 364597)
CL-USER> (defvar *somechunk* (subseq-array *last-right-samps* 364087 (- 364597 364087)))
*SOMECHUNK*
CL-USER> (array-dimension *somechunk* 0)
510
CL-USER>

CL-USER> (vgplot:plot *somechunk*)
""
CL-USER>

CL-USER> (progn (chopper *last-found-edges*) 7)
--- post-chopper: average samples per chunk/window/frame/trigger: 445.59717 ( 0.009283274 sec at 48000 hz)
7
CL-USER>  (nth 300 *last-found-edges*)
(364087 . 364597)
CL-USER> (nth 300 *last-chopped-edges*)
(364107 . 364553)
CL-USER> (plot-chunk (nth 300 *last-chopped-edges*))

""
CL-USER> (vgplot:close-plot)
NIL
CL-USER>

TODO:
-- chop to all same len? even?

|#
