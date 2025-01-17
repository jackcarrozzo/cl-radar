(in-package :cl-user)
(defpackage cl-radar.audio
  (:use :cl
        :portaudio
        :cl-radar.util))
(in-package :cl-radar.audio)
(cl-syntax:use-syntax :annot)

;;;;;;;;; summary ;;;;;;;;;;
;; portaudio: works bidir but wont go above 48khz even on devs that support 192khz
;; ffmpeg: wont listen to sample rate setting
;; sox: doesnt seem to do anythin with sample rate setting
;; audacity: appears to work
;; cl-wav: relies on riff which only supports 1 and 2 byte samples
;;


#|
CL-USER> (cl-radar.audio:print-portaudio-devices)
PortAudio version number = 1246976
PortAudio version text = PortAudio V19.7.0-devel, revision 147dd722548358763a8b649b3e4b41dfffbcfbb6
Number of devices = 4
---------------------- device 0
[ Default Input, Default Output ]
Name                        = Scarlett 18i8 USB
Host API                    = Core Audio
Max inputs = 20, Max outputs = 8
Default low input latency   =   0.0100
Default low output latency  =   0.0044
Default high input latency  =   0.1000
Default high output latency =   0.0137
Default sample rate         = 48000.0000
---------------------- device 1
Name                        = MacBook Pro Microphone
Host API                    = Core Audio
Max inputs = 1, Max outputs = 0
Default low input latency   =   0.0551
Default low output latency  =   0.0100
Default high input latency  =   0.0652
Default high output latency =   0.1000
Default sample rate         = 44100.0000
---------------------- device 2
Name                        = MacBook Pro Speakers
Host API                    = Core Audio
Max inputs = 0, Max outputs = 2
Default low input latency   =   0.0100
Default low output latency  =   0.0122
Default high input latency  =   0.1000
Default high output latency =   0.0223
Default sample rate         = 44100.0000
---------------------- device 3
Name                        = Jack’s iPhone (2) Microphone
Host API                    = Core Audio
Max inputs = 1, Max outputs = 0
Default low input latency   =   0.1284
Default low output latency  =   0.0100
Default high input latency  =   0.1378
Default high output latency =   0.1000
Default sample rate         = 48000.0000
NIL
|#

@export
(defun print-portaudio-devices ()
  "List available sound devices, including device information."
  (pa:with-audio
    (format t "PortAudio version number = ~D~%PortAudio version text = ~A~%"
            (get-version) (get-version-text))
    (let ((num-devices (get-device-count))
          #|(input-parameters (pa:make-stream-parameters))
          (output-parameters (pa:make-stream-parameters))
          |#)
      (format t "Number of devices = ~D~%" num-devices)
      (dotimes (i num-devices)
        (print-device-details-by-idx i)))))

;; TODO:: get (default) devices plist
@export
(defun confirm-default-input (num-chans &optional default-sample-rate name-contains verbose-p)
  (portaudio:with-audio
   (let* ((input-dev-idx (portaudio:get-default-input-device))
          (device-info (get-device-info input-dev-idx))
          (max-input-chans (device-info-max-input-channels device-info))
          (device-default-sample-rate (device-info-default-sample-rate device-info))
          (device-name-string (device-info-name device-info))
          (r t))

     (when verbose-p
       (print-device-details-by-idx input-dev-idx))

     (setf r (printing-assert num-chans max-input-chans "input channels"))

     (when default-sample-rate
       (if (not (printing-assert default-sample-rate device-default-sample-rate "Hz"))
           (setf r nil)))

     (when name-contains
       (let ((upper-contains
               (string-upcase
                (format nil "~a" name-contains))) ;; in case its a keyword symbol
             (upper-name (string-upcase device-name-string)))
         (if (not
              (substringp upper-contains upper-name))
             (progn
               (format t "--- Device name assertion failed: expected to find '~a' (~a) in device string '~a' (~a) !~%"
                       name-contains upper-contains device-name-string upper-name)
               (setf r nil)))))
     r)))

@export
(defun confirm-default-output (num-chans &optional default-sample-rate name-contains verbose-p)
  (portaudio:with-audio
    (let* ((output-dev-idx (portaudio:get-default-output-device))
           (device-info (get-device-info output-dev-idx))
           (max-output-chans (device-info-max-output-channels device-info))
           (device-default-sample-rate (device-info-default-sample-rate device-info))
           (device-name-string (device-info-name device-info))
           (r t))

      (when verbose-p
        (print-device-details-by-idx output-dev-idx))

      (setf r (printing-assert num-chans max-output-chans "output channels"))

      (when default-sample-rate
        (if (not (printing-assert default-sample-rate device-default-sample-rate "Hz"))
            (setf r nil)))

      (when name-contains
        (let ((upper-contains
                (string-upcase
                 (format nil "~a" name-contains))) ;; in case its a keyword symbol
              (upper-name (string-upcase device-name-string)))
          (if (not
               (substringp upper-contains upper-name))
              (progn
                (format t "--- Device name assertion failed: expected to find '~a' (~a) in device string '~a' (~a) !~%"
                        name-contains upper-contains device-name-string upper-name)
                (setf r nil)))))
      r)))

@export
(defun print-device-details-by-idx (i)
  (let ((device-info (get-device-info i))
              (default-displayed nil))
          (format t "---------------------- device ~D~%" i)
          (if (= i (get-default-input-device))
              (progn
                (format t "[ Default Input")
                (setf default-displayed t))
              (when (= i (host-api-info-default-input-device (get-host-api-info (device-info-host-api device-info))))
                (format t "[ Default ~A Input" (get-host-api-info (device-info-host-api device-info)))
                (setf default-displayed t)))

          (if (= i (get-default-output-device))
              (progn
                (format t "~:[[~;,~]" default-displayed)
                (format t " Default Output")
                (setf default-displayed t))
              (when (= i (host-api-info-default-output-device (get-host-api-info (device-info-host-api device-info))))
                (format t "~:[[~;,~]" default-displayed)
                (format t "[ Default ~A Output" (get-host-api-info (device-info-host-api device-info)))
                (setf default-displayed t)))
          (when default-displayed
            (format t " ]~%"))

          (format t "Name                        = ~A~%" (device-info-name device-info))
          (format t "Host API                    = ~A~%" (host-api-info-name (get-host-api-info (device-info-host-api device-info))))
          (format t "Max inputs = ~D" (device-info-max-input-channels device-info))
          (format t ", Max outputs = ~D~%" (device-info-max-output-channels device-info))
          (format t "Default low input latency   = ~8,4F~%" (device-info-default-low-input-latency device-info))
          (format t "Default low output latency  = ~8,4F~%" (device-info-default-low-output-latency device-info))
          (format t "Default high input latency  = ~8,4F~%" (device-info-default-high-input-latency device-info))
          (format t "Default high output latency = ~8,4F~%" (device-info-default-high-output-latency device-info))
          (format t "Default sample rate         = ~8,4F~%" (device-info-default-sample-rate device-info))))

;; see clvar/var/pa-printdevices.lisp- still broken upsteam 2022-11-13
(defparameter +frames-per-buffer+ 1024) ;; more like buffers per frame
(defparameter +pa-sample-rate+ 48000d0)
(defparameter +pa-sample-format+ :float)
(defparameter +pa-num-channels+ 2)
(defparameter +run-secs+ 5)

;;TODO:
(defvar *r2*)

@export
(defun sines-out-in (&optional (run-secs 15))
  (pa:with-audio
    (let ((input-parameters (pa:make-stream-parameters))
          (output-parameters (pa:make-stream-parameters))
          (dev-in (pa:get-default-input-device))
          (dev-out (pa:get-default-output-device))
          (n 0))

      (setf dev-in 0)
      (setf dev-out 0)

      (format t "-- device in is idx ~a: ~a~%"
              dev-in (device-info-name (get-device-info dev-in)))
      (format t "-- device in out idx ~a: ~a~%"
              dev-out (device-info-name (get-device-info dev-out)))


      (setf
       (pa:stream-parameters-device input-parameters) dev-in
       (pa:stream-parameters-channel-count input-parameters) +pa-num-channels+
       (pa:stream-parameters-sample-format input-parameters) +pa-sample-format+
       (pa:stream-parameters-suggested-latency input-parameters)
       (pa:device-info-default-low-input-latency
        (pa:get-device-info dev-in)))

      (setf
       (pa:stream-parameters-device output-parameters) dev-out
       (pa:stream-parameters-channel-count output-parameters) +pa-num-channels+
       (pa:stream-parameters-sample-format output-parameters) +pa-sample-format+
       (pa:stream-parameters-suggested-latency output-parameters)
       (pa:device-info-default-low-output-latency
        (pa:get-device-info dev-out)))

      (format t "~%=== Wire on. Will run ~D seconds . ===~%" run-secs)

      (let ((output-buffer
              (make-array (* 2 +frames-per-buffer+)
                          :element-type 'single-float
                          :initial-element 0.0))
            (input-buffers '()))
        (pa:with-audio-stream
            (astream input-parameters output-parameters
             :sample-rate +pa-sample-rate+
             :frames-per-buffer +frames-per-buffer+
             :stream-flags (:clip-off))
          (dotimes (i (round (/ (* run-secs +pa-sample-rate+) +frames-per-buffer+)))
            (let ((inbuf (pa:read-stream astream))
                  ;;(thismax 0.0)
                  ;;(maxes '())
                  )

              (push inbuf input-buffers)

              #|(loop for i from 0 below (first (array-dimensions inbuf))
                    do
                       (when (> (aref inbuf i) thismax)
                         (setf thismax (aref inbuf i))))
              (push thismax maxes) ;;TODO:
              |#

              ;; sounds higher than 2k to me but works!
              (cl-radar.wavegen::sines-fill-portaudio-2ch-array output-buffer)

              (pa:write-stream astream output-buffer)
              ;;(pa:write-stream astream (pa:read-stream astream))
              (incf n))))
        (setf *r2* input-buffers))
      (format t "-- ~a chunks of ~a~%" n (type-of (first *r2*))))))



(defvar *first-inbuf*)
(defvar *last-inbuf*)
(defvar *maxes*)

(defparameter +echo-secs+ 5)
(defparameter +echo-frames-per-buffer+ 4096)

(defvar *last-in-split-ar*)

@export
(defun test-read-write-echo ()
  "Record input into an array; Playback recorded data."

  (setf *first-inbuf* nil)
  (setf *last-inbuf* nil)
  (setf *maxes* nil)

  (pa:with-audio
    (let ((input-parameters (pa:make-stream-parameters))
          (output-parameters (pa:make-stream-parameters))
          (n 0))

      (setf (pa:stream-parameters-device input-parameters) (pa:get-default-input-device)
            (pa:stream-parameters-channel-count input-parameters) +pa-num-channels+
            (pa:stream-parameters-sample-format input-parameters) +pa-sample-format+
            (pa:stream-parameters-suggested-latency input-parameters)
            (pa:device-info-default-low-input-latency
             (pa:get-device-info
              (pa:get-default-input-device))))

      (setf (pa:stream-parameters-device output-parameters) (pa:get-default-output-device)
            (pa:stream-parameters-channel-count output-parameters) +pa-num-channels+
            (pa:stream-parameters-sample-format output-parameters) +pa-sample-format+
            (pa:stream-parameters-suggested-latency output-parameters)
            (pa:device-info-default-low-output-latency
             (pa:get-device-info (pa:get-default-output-device))))

      (format t "~%=== Wire on. Will run ~D seconds . ===~%" +echo-secs+)

      (pa:with-audio-stream
          (astream input-parameters output-parameters
           :sample-rate +pa-sample-rate+
           :frames-per-buffer +echo-frames-per-buffer+
           :stream-flags (:clip-off))

        (let ((out-ar nil))
          (dotimes (i (round (/ (* +echo-secs+ +pa-sample-rate+) +echo-frames-per-buffer+)))
            (let* ((r (pa:read-stream astream))
                   (rr (pa:separate-array-to-channels astream r))
                   ;;(read-avail (pa:get-stream-read-available astream))
                   (thismax 0.0))

              (setf *last-in-split-ar* rr)

              #|(format t "-- r array dims: ~a, rr: ~a~%"
                      (array-dimensions r)
                      (array-dimensions rr))|#

              ;; -- r array dims: (2048), rr: (2 1024)

              ;; make outbuf
              (when (not out-ar) ;; make sure out ar same shape as in
                (setf out-ar
                      (make-array
                       (array-dimensions rr)
                       :element-type 'single-float
                       :initial-element 0.0))
                ;;(format t "-- made out-ar, dims: ~a~%" (array-dimensions out-ar))
                )

              ;; save inbufs
              (setf *last-inbuf* r)
              (when (not *first-inbuf*)
                (setf *first-inbuf* r))

              ;; calc max
              (loop for i from 0 below (first (array-dimensions r))
                    do
                       (when (> (aref r i) thismax)
                         (setf thismax (aref r i))))
              ;;(format t "-- thismax: ~a~%" thismax)
              (push thismax *maxes*)

              ;; sounds higher than 2k to me but works!
              (sines-fill-2ch-array out-ar) ;; TODO: right chan amplitude
              ;;(format t "--- out-ar is ~a~%" (array-dimensions out-ar))

              (signals-fill-2d-array out-ar :left-sig-next-fn #'ramp-get-next
                                     :right-ampl 0.2)

              (pa:write-stream
               astream
               (pa:merge-channels-into-array astream out-ar))
              ;;(pa:write-stream astream out-ar)
              ;;(pa:write-stream astream (pa:read-stream astream))
              (incf n)))))
      (format t "-- ~a chunks of ~a~%" n (type-of *last-inbuf*)))))


#|
CL-USER> (cl-radar.audiodev::test-read-write-echo)

=== Wire on. Will run 5 seconds . ===
-- 234 chunks of (SIMPLE-ARRAY SINGLE-FLOAT (2048))
NIL
CL-USER>
|#

;; comes out 400hz 1.7vpp


;;;;;;;;;;;;;

;; see clvar/var/pa-printdevices.lisp- still broken upsteam 2022-11-13 (which?? -jc)
;;(defconstant +frames-per-buffer+ 1024) ;; TODO: there are several defs of this
(defparameter +frames-per-buffer+ 1024)

;; TODO:
(defparameter +sample-rate+ 44100d0)
;;(defconstant +seconds+ 15)
(defparameter +sample-format+ :float)
(defparameter +num-channels+ 1) ; 2

(defparameter +run-secs+ 5)

(defvar *r2* '()) ;; TODO:
(defvar *maxes* '())

@export
(defun snag-audio (&key (frames-per-buffer +frames-per-buffer+)
                     (sample-rate +sample-format+)
                     (sample-format +sample-format+)
                     (num-channels +num-channels+)
                     (run-secs +run-secs+))
  (setf *maxes* nil)
  (setf *r2* nil)

  (pa:with-audio
    (let ((input-parameters (pa:make-stream-parameters))
          (n 0))

      ;; --- same as example ---
      (setf (pa:stream-parameters-device input-parameters)
            (pa:get-default-input-device)
            (pa:stream-parameters-channel-count input-parameters) num-channels
            (pa:stream-parameters-sample-format input-parameters) sample-format
            (pa:stream-parameters-suggested-latency input-parameters)
            (pa:device-info-default-low-input-latency
             (pa:get-device-info
              (pa:get-default-input-device))))

      (format t "~%=== Wire on. Will run ~D seconds . ===~%" run-secs)

      (let ((input-buffers '()))
        ;;(format t "-- inited ~%")
        (pa:with-audio-stream
            (astream
             input-parameters nil
             :sample-rate sample-rate
             :frames-per-buffer frames-per-buffer
             :stream-flags (:clip-off))
          ;;(format t "-- opened port ~%")
          (dotimes (i (round (/ (* run-secs sample-rate) frames-per-buffer)))
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
              (* n (array-dimension (first *r2*) 0)) sample-rate
              (/ (* n (array-dimension (first *r2*) 0)) (float sample-rate)))
      (format t "-- max of maxes is ~a.~%"
              (apply #'max *maxes*)))))

;; (snag-audio)
;; (setf r (bordeaux-fft:windowed-fft (first *r2*) 512 1024 'bordeaux-fft:blackman-harris))
;;





;; TODO: parameterize

(defparameter +fmcw-sample-rate+ 48000d0)
(defparameter +fmcw-chans-in+ 2)
(defparameter +fmcw-chans-out+ 2)
(defparameter +fmcw-input-device+ nil)
(defparameter +fmcw-output-device+ nil)
(defparameter +fmcw-frames-per-buffer+ 1024)


(defvar *last-fmcw-ins*) ;; array (2 1024)
(defvar *last-fmcw-outs*)
(defvar *last-fmcw-edges* nil)


;; TODO: reorg this monstrosity
@export
(defun fmcw-gen-and-record (&key (run-secs 5) (read-cb-fn nil))
  "Generate ramp(s) out, record both chans in"

  (pa:with-audio
    (when
        (not
         (and
          (confirm-default-input  +fmcw-chans-in+  +fmcw-sample-rate+  :usb t)
          (confirm-default-output +fmcw-chans-out+ +fmcw-sample-rate+ :usb t)))
      (format t "--- At least one audio device assertion failed!~%"))

    (setf +fmcw-input-device+
          (pa:get-default-input-device))
    (setf +fmcw-output-device+
          (pa:get-default-output-device))

    (format t "--- default inp device idx: ~a~%" +fmcw-input-device+)
    (format t "--- default out device idx: ~a~%" +fmcw-output-device+)


    (setf *last-fmcw-ins* nil)
    (setf *maxes* nil)

    (let ((input-parameters (pa:make-stream-parameters))
          (output-parameters (pa:make-stream-parameters))
          (n 0))

      (setf (pa:stream-parameters-device input-parameters) +fmcw-input-device+
            (pa:stream-parameters-channel-count input-parameters) +fmcw-chans-in+
            (pa:stream-parameters-sample-format input-parameters) :float
            (pa:stream-parameters-suggested-latency input-parameters)
            (pa:device-info-default-low-input-latency
             (pa:get-device-info +fmcw-input-device+)))

      (setf (pa:stream-parameters-device output-parameters) +fmcw-output-device+
            (pa:stream-parameters-channel-count output-parameters) +fmcw-chans-out+
            (pa:stream-parameters-sample-format output-parameters) :float
            (pa:stream-parameters-suggested-latency output-parameters)
            (pa:device-info-default-low-output-latency
             (pa:get-device-info +fmcw-output-device+)))

      (format t "~%=== Wire on. Will run ~D seconds . ===~%" run-secs)
      (finish-output)

      (pa:with-audio-stream
          (astream input-parameters output-parameters
           :sample-rate +fmcw-sample-rate+
           :frames-per-buffer +fmcw-frames-per-buffer+
           :stream-flags (:clip-off))

        (let ((outbuf-2d-ar ;; needs to be here to feed read-cb-fn first time
                (make-array (list 2 +fmcw-frames-per-buffer+)
                            :initial-element 0.0)))
          (dotimes (i (round (/ (* run-secs +fmcw-sample-rate+) +fmcw-frames-per-buffer+)))
            (let* ((inbuf-single-ar (pa:read-stream astream))
                   (inbuf-2d-ar (pa:separate-array-to-channels astream inbuf-single-ar))
                   ;;(read-avail (pa:get-stream-read-available astream))
                   ;;(thismax 0.0)
                   )

              (setf *last-fmcw-ins* inbuf-2d-ar)
              (setf *last-fmcw-outs* outbuf-2d-ar)

              #|(format t "-- r array dims: ~a, rr: ~a~%"
                      (array-dimensions r)
                      (array-dimensions rr))|#

              ;; -- r array dims: (2048), rr: (2 1024)

              ;; make outbuf
              (when (not outbuf-2d-ar) ;; make sure out ar same shape as in
                (setf outbuf-2d-ar
                      (make-array
                       (array-dimensions inbuf-2d-ar)
                       :element-type 'single-float
                       :initial-element 0.0))
                ;;(format t "-- made out-ar, dims: ~a~%" (array-dimensions out-ar))
                )

              ;; save bufs TODO: are these used anymore
              ;;(setf *last-inbuf* inbuf-2d-ar)
              ;;(when (not *first-inbuf*)
              ;;  (setf *first-inbuf* inbuf-2d-ar))

              (setf *last-fmcw-outs* outbuf-2d-ar)

              ;; TODO: carry context from wavegen thru to cb func:
              ;;   let #'rampgen send a list of trigger edges (offsets into outbuf)
              ;;   thru to callback fn a-la
              ;; (funcall handle-radar-data-fn inbuf-2d-ar trigger-edges-list)

              (when read-cb-fn
                (funcall read-cb-fn inbuf-2d-ar outbuf-2d-ar *last-fmcw-edges*))

              ;; calc max
              ;(when calc-max-p
              ;  ;; is this needed? TODO: use ar utils func
              ;  (loop for i from 0 below (first (array-dimensions inbuf-single-ar))
              ;        do
              ;           (when (> (aref inbuf-single-ar i) thismax)
              ;             (setf thismax (aref inbuf-single-ar i))))
              ;  ;;(format t "-- thismax: ~a~%" thismax)
              ;  (push thismax *maxes*))


              ;; TODO: carry args from wrapper
              (multiple-value-bind (ret-ar edges-list)
                  (cl-radar.wavegen:signals-fill-2ch-array
                   outbuf-2d-ar
                   :left-sig-next-fn #'cl-radar.wavegen:ramp-get-next
                   :right-ampl 0.2)
                (declare (ignore ret-ar))

                (setf *last-fmcw-edges* edges-list)) ;; TODO

              (pa:write-stream
               astream
               (pa:merge-channels-into-array astream outbuf-2d-ar))
              ;;(pa:write-stream astream out-ar)
              ;;(pa:write-stream astream (pa:read-stream astream))
              (incf n)))))
      (format t "-- ~a chunks of ~a~%" n (type-of *last-fmcw-ins*)))))

#|
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


#|
[jackc@nichor] ~/Projects/cl-radar :) $ ffmpeg -f avfoundation -list_devices true -i "" 2>&1 |tail -7
[AVFoundation indev @ 0x7fb807f04dc0] [0] FaceTime HD Camera (Built-in)
[AVFoundation indev @ 0x7fb807f04dc0] [1] Capture screen 0
[AVFoundation indev @ 0x7fb807f04dc0] AVFoundation audio devices:
[AVFoundation indev @ 0x7fb807f04dc0] [0] Jack’s AirPods
[AVFoundation indev @ 0x7fb807f04dc0] [1] Studio 24c
[AVFoundation indev @ 0x7fb807f04dc0] [2] MacBook Pro Microphone
: Input/output error

|#

(defparameter +wav-write-path+ "/tmp/out-writing.wav") ;;TODO:
(defparameter +wav-process-path+ "/tmp/out.wav")

@export
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
     (format nil "/usr/local/bin/ffmpeg -f avfoundation -ac 2 -i :1 -t 4 -y -hide_banner -nostats -nostdin ~a" +wav-write-path+)
     ;;(format nil "/usr/local/bin/ffmpeg -f avfoundation -ac 2 -i :0 -t 1 -y -hide_banner -nostats -nostdin ~a" +wav-write-path+)
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

@export
(defvar *last-wav* nil)
@export
(defvar *last-left-samps* nil)
@export
(defvar *last-right-samps* nil)

@export
(defvar *last-sample-rate* nil)
;; TODO: this prolly should import/ref fmcw::*last-sample-rate*
;; or not exist and use return vals as the gods intended


;; expects stereo structure (TODO:)
@export
(defun read-wav (&optional
                   (fpath
                    (format nil
                            "~a/1-fullchain_stereo.wav"
                            (asdf:system-relative-pathname 'cl-radar 'data)))
                   debug-p)
  (when debug-p
    (format t "-- read-wav: ~a~%" fpath))

  (let* ((wav-obj (setf *last-wav*
                        (wav:read-wav-file
                         fpath
                         :chunk-data-reader (wav:wrap-data-chunk-data-samples-reader))))
         (header-plist (first wav-obj))
         (metadata-plist (second wav-obj))
         (metadata-plist-inner (getf metadata-plist :chunk-data))
         (data-plist (or (fifth wav-obj) (third wav-obj))) ;; for wavs from audacity TODO:
         ;;(data-plist (fifth wav-obj)) ;; wavs from ffmpeg
         ;; TODO: nab the data plist automatically (not working for some reason)
         ;;(samples-ar (getf (third wav-obj) :chunk-data)) ;; if single chan?
         ;;(num-samples (first (array-dimensions samples-ar)))

         ;; TODO: might need to be fmcw::*sample-rate*
         (sample-rate (setf *last-sample-rate*
                            (getf metadata-plist-inner :sample-rate))))

    (when debug-p
      ;;(format t "-- loaded ~a samples- at ~a hz thats ~a seconds.~%"
      ;;        num-samples sample-rate (/ num-samples (float sample-rate)))
      (format t "----- 1: ~a~%" header-plist)
      (format t "----- 2: ~a~%" metadata-plist)
      (format t "----- 2 inner: ~a~%" metadata-plist-inner)
      ;;(format t "----- 3: ~a~%" (subseq data-plist 0 (- (length data-plist) 2)))
      )

    ;; TODO:
    (assert (string= "data" (getf data-plist :chunk-id)))
    (assert (= 2 (getf metadata-plist-inner :NUMBER-OF-CHANNELS)))
    (assert (= 16 (getf metadata-plist-inner :SIGNIFICANT-BITS-PER-SAMPLE)))
    (assert (= 1 (getf metadata-plist-inner :COMPRESSION-CODE)))
    (assert (= 4 (getf metadata-plist-inner :BLOCK-ALIGN)))

    (when debug-p
      (format t "-- oh, this is a stereo wav, deinterleaving... ~%"))
      ;;(assert (= 0 (mod num-samples 2)))

    (let* ((reported-samples-in-chunk (getf data-plist :chunk-data-size))
           (samples-ar (getf data-plist :chunk-data))
           (left-ar (make-array (/ reported-samples-in-chunk 2) :initial-element 0.0d0))
           (right-ar (make-array (/ reported-samples-in-chunk 2) :initial-element 0.0d0)))

      (when debug-p
        (format t "----- chunk reports size of ~a samples- half of that is ~a.~%"
                reported-samples-in-chunk (/ reported-samples-in-chunk 2))
        (format t "----- array we got in chunk has ~a elements.~%" (array-dimension samples-ar 0)))

      (loop for i from 0 below (/ (array-dimension samples-ar 0) 2)
            do
               (progn
                 (setf (aref left-ar i)
                       (aref samples-ar (* 2 i)))
                 (setf (aref right-ar i)
                       (aref samples-ar (1+ (* 2 i))))))

      (setf *last-left-samps* left-ar)
      (setf *last-right-samps* right-ar)

      (when debug-p
        (format t "-- read wav: removing dc bias on left chan!~%"))

      ;;(remove-dc-bias *last-left-samps*) ;; TODO: parameterize, both channels

      (when debug-p
        (format t "-- ok. now we have two arrays of length ~a samples or ~a seconds.~%"
                (array-dimension left-ar 0)
                (/ (array-dimension left-ar 0) (float sample-rate)))))))


;; TODO: this needs to go somewhere else

;; in-place modify (? doesnt make sense now with pad? or need confined i to i)
;; should go before zero pad
;; TODO:


;;TODO: also exists ish in math
@export
(defun remove-dc-bias (ar &optional rezero-end-len)
  (let* ((ar-avg (cl-radar.math:array-average ar))
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
