(in-package :cl-user)
(defpackage cl-radar.image
  (:use :cl))
(in-package :cl-radar.image)
(cl-syntax:use-syntax :annot)

;; take list of ar and plot them as one column each going rightward
;; closer distance / smaller fft bin at bottom of image (highest row)
@export
(defun write-slices-to-png (slices-list &key (path "/Users/jackc/fullthing.png") (log-scale-p nil))
  (let* ((width-px (length slices-list))
         (height-px (array-dimension (first slices-list) 0))
         (png (png:make-image height-px width-px 1))
         (maxval (reduce #'max (mapcar #'cl-radar.math:array-max-min slices-list)))
         (column 0))
    (format t "making png ~a high by ~a wide, maxval ~a.~%"
            height-px width-px maxval)

    (loop for slice-ar in slices-list
          do
             (let ((sample-ar (if log-scale-p
                                  (cl-radar.math:dB-ulize-array slice-ar)
                                  slice-ar)))
               (loop for row from 0 below (array-dimension sample-ar 0)
                     do
                        (progn
                          ;; TODO: colors need to be fixed for log scale
                          (setf (aref png row column 0)
                                (coerce
                                 (round
                                  (* 255
                                     (/ (aref sample-ar (- (- height-px 1) row))
                                        maxval)))
                                 '(unsigned-byte 8)))))
               (incf column)))

    (with-open-file
        (output path :element-type '(unsigned-byte 8)
                                :direction :output :if-exists :supersede)
      (png:encode png output))))
