(in-package :cl-user)
(defpackage cl-radar.image
  (:use :cl))
(in-package :cl-radar.image)
(cl-syntax:use-syntax :annot)

;; take list of ar and plot them as one column each going rightward
;; closer distance / smaller fft bin at bottom of image (highest row)
@export
(defun write-slices-to-png (slices-list &optional (png-path "/Users/jackc/fullthing.png"))
  (let* ((width-px (length slices-list))
         (height-px (array-dimension (first slices-list) 0))
         (png (png:make-image height-px width-px 1))
         (maxval (reduce #'max (mapcar #'cl-radar.math:array-max slices-list)))
         (column 0))
    (format t "making png ~a high by ~a wide, maxval ~a.~%"
            height-px width-px maxval)

    (loop for slice-ar in slices-list
          do
             (progn
               (loop for row from 0 below (array-dimension slice-ar 0)
                     do
                        (progn
                          (setf (aref png row column 0)
                                (coerce
                                 (round
                                  (* 255
                                     (/ (aref slice-ar (- (- height-px 1) row))
                                        maxval)))
                                 '(unsigned-byte 8)))))
               (incf column)))

    (with-open-file
        (output png-path :element-type '(unsigned-byte 8)
                                :direction :output :if-exists :supersede)
      (png:encode png output))))
