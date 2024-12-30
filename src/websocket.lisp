(in-package :cl-user)
(defpackage cl-radar.websocket
  (:use :cl))
(in-package :cl-radar.websocket)
(cl-syntax:use-syntax :annot)

(defparameter +ws-port+ 5070)

(defvar *connections* (make-hash-table))

(defun handle-new-connection (con)
  (format t "new client connection: ~a~%" con)

  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

@export
(defun send-to-all-clients (msg)
  (loop :for con :being :the :hash-key :of *connections* :do
    (websocket-driver:send con msg)))

(defun ws-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda ()
                           (format t "new connection arrived: ~a~%" ws)
                           (handle-new-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (msg)
                           (format t "msg to all: ~a~%" msg)
                           (send-to-all-clients (format nil "msg: ~a" msg))))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           ;;(declare (ignore code reason))
                           (format t "client close: code ~a, reason ~a.~%" code reason)
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

(defvar *handler*)

@export
(defun run ()
  (setf *handler*
        (clack:clackup #'ws-server :port +ws-port+)))

@export
(defun stop ()
  (format t "-- stopping ws server on port ~a.~%"
          +ws-port+))
