(in-package :cl-user)
(defpackage cl-radar.util
  (:use :cl))
(in-package :cl-radar.util)
(cl-syntax:use-syntax :annot)

@export
(defun printing-assert (expected actual name)
  (if (= expected actual)
      t
      (progn
        (format t "--- Assertion failed: expected ~a ~s but got ~a.~%" expected name actual)
        nil)))

@export
(defun substringp (needle haystack &key (test 'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))




@export
(defun percent-diff (v1 v2)
  (let ((dv (- v2 v1)))
    (values
     (* 100.0
        (/ dv v1))
     dv)))

@export
(defun round-.1 (v)
  (multiple-value-bind (n d)
      (round v)
    (+ n
       (/
        (round (* d 10.0))
        10.0))))

@export
(defun unix-ts ()
  (- (simple-date-time:to-universal-time
      (simple-date-time:now ))
     simple-date-time::+posix-epoch+))

@export
(defun unix-ts-from-parts (yr mon day)
  (- (simple-date-time:to-universal-time
      (simple-date-time:make-date yr mon day))
     simple-date-time::+posix-epoch+))


@export
(defun write-js-list-data (fname the-lists &key (n -1))
  (with-open-file (out fname
                       :direction :output
                       :if-exists :supersede)
    (loop for lst in the-lists
       do
         (let ((vname (string-downcase (pop lst))))
           (format out "var ~a=~a;~%"
                   (if nil
                    (format nil "values~a" (incf n)))
                   (cl-json:encode-json-to-string lst))))))

@export
(defun write-string-to-file (fpath str)
  (with-open-file (out fpath
                       :direction :output
                       :if-exists :supersede)
    (format out str)))

@export
(defun write-string-to-js-file (fpath varname str)
  (write-string-to-file
   fpath
   (format nil "var ~a=~a;~%" varname str)))


(defun read-u2 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    u2))


@export
(defun std-dev (float-list)
  (let* ((n (length float-list)) mean)
    (cond
      ((zerop n) nil)
      ((= n 1) 0.0)
      (t
       (setf mean (/ (reduce #'+ float-list) n))
       (sqrt (/
              (reduce #'+
                        (mapcar #'(lambda (x)
                                    (expt (- x mean) 2))
                                    float-list))
              n))))))
