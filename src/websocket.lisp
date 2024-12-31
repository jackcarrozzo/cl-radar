(in-package :cl-user)
(defpackage cl-radar.websocket
  (:use :cl))
(in-package :cl-radar.websocket)
(cl-syntax:use-syntax :annot)

(defparameter +ws-port+ 5070)

(defvar *connections* (make-hash-table))

(defun handle-new-connection (con)
  (format t "-- ws server: new client connection: ~a~%" con)

  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun handle-close-connection (connection &optional code reason)
  (let ((message (format nil "---- ~a has left."
                         (gethash connection *connections*))))
    (declare (ignore message))
    (format t "-- ws server: client close: ~a code ~a, reason ~a.~%"
            (gethash connection *connections*) code reason)
    (remhash connection *connections*)

    ;; tell all connections
    ;;(loop :for con :being :the :hash-key :of *connections* :do
    ;;  (websocket-driver:send con message))
    ))

@export
(defun send-to-all-clients (msg &optional dont-autostart-p)
  (when (not dont-autostart-p)
    (run-if-not-already))

  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con msg)))

(defun ws-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda ()
                           (format t "-- ws server: new connection arrived: ~a~%" ws)
                           (handle-new-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (msg)
                           (format t "-- ws server: msg in: ~a~%" msg)
                           (send-to-all-clients (format nil "msg in and around: ~a" msg))))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           ;;(declare (ignore code reason))
                           (handle-close-connection ws code reason)))
    (lambda (responder)
      ;;(declare (ignore responder))
      (format t "-- ws server: responder ~a~%" responder)
      (websocket-driver:start-connection ws))))

(defvar *handler* nil)

@export
(defun run-if-not-already ()
  (when (not *handler*)
    (format t "-- ws run-if-not-already: werent yet running, doing it.~%")
    (run)))

@export
(defun run ()
  (setf *handler*
        (clack:clackup #'ws-server :port +ws-port+)))

@export
(defun stop (&optional (handler *handler*))
  (assert handler)

  (format t "-- ws server: port ~a stopping...~%"
          +ws-port+)
  (clack:stop handler)
  (format t "-- ws server: port ~a ended.~%" +ws-port+)
  (setf *handler* nil))
