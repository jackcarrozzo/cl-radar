(in-package :cl-user)
(defpackage cl-radar-asd
  (:use :cl :asdf))
(in-package :cl-radar-asd)

(defsystem cl-radar
  :version "0.1"
  :author "JC"
  :license ""
  :depends-on (:cl-ppcre
               :cl-syntax-annot
               :cl-json
               ;;:ningle
               ;;:clack
               ;;:drakma
               :alexandria
               :simple-date-time
               ;;:datafly
               ;;:cl21
               :cl-portaudio
               :cl-wav
               :bordeaux-fft
               :vgplot
               :osicat
               :clunit
               :png
               :unit-test
	       :cl-portaudio)
  :components ((:module "src"
                        :components
                        ((:file "cl-radar")
                         (:file "audio" :depends-on ("util" "wavegen" "math"))
                         (:file "wavegen" :depends-on ("util"))
                         (:file "math" :depends-on ("util"))
                         (:file "image" :depends-on ("math"))
                         (:file "fmcw" :depends-on ("util" "math" "wavegen" "audio"))
                         (:file "websocket" :depends-on ("util"))
                         (:file "util")
                         (:file "t/main")
                         (:file "t/wavegen" :depends-on ("wavegen" "t/main"))
                         (:file "t/math" :depends-on ("math" "t/main"))
                         (:file "t/fmcw" :depends-on ("fmcw" "t/main"))
                        )))
  :description "A collection of handy functions for radar tasks"
  :in-order-to ((test-op (load-op cl-radar-test))))

#|(defsystem cl-radar-tests
  :name "Tests for cl-radar"
  :description "Tests for cl-radar"
  :author "Jack C <jack@crepinc.com>"
  :version "0.1"
  :components ((unit-test:unit-test-files "src/t/*.lisp"))
  :depends-on (:cl-radar))|#
