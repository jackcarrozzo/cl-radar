(in-package :cl-user)
(defpackage cl-radar.md
  (:use :cl
        :simple-date-time
        :cl-radar.util))
(in-package :cl-radar.md)
(cl-syntax:use-syntax :annot)

#|
@export
(defvar *md* nil)

@export
(defun clear-md ()
  (setf *md*
        (list
         :1min (make-hash-table :test 'equalp)
         :5min (make-hash-table :test 'equalp)
         :1day (make-hash-table :test 'equalp))))

;; - each is a hashtable of timestamps
;; - a blob is the ts-keyed set of syms and their props for that ts

@export
(defun write-js-model ()
  (let ((syms (mapcar #'car (mdfetch))))
    (write-js-list-data
     "~/Projects/graphthis/www/vdata.js"
     (mapcar #'(lambda (s)
                 (cons (string-downcase s)
                       (slurp-single-inst-field s
                                                :incl-sym-p nil)))
             syms))))

@export
(defun fetch-md-hashtable-by-int (&key (interval :1day))
  (getf *md* interval))

@export
(defun md-keys (&key (interval :1day))
  (sort
   (alexandria:hash-table-keys
    (fetch-md-hashtable-by-int
     :interval interval))
   #'string-lessp))

@export
(defun mapc-md-table (fn &key (interval :1day))
  (mapc fn
        (md-keys :interval interval))
  (values))

@export
(defun mapset-md-table
    (sym fn &key (default nil)(field :close) (interval :1day))

  (loop for ts in (md-keys :interval interval)
     do
       (set-sym-val-by-ts sym ts
                          (or
                           (apply fn (list ts))
                           default)
                          :field field
                          :interval interval))
  (values))

@export
(defun slurp-single-inst-field (sym &key (field :close) (incl-sym-p t)
                                      (interval :1day))
  ;; nab sym val by ts
  (let ((r
         (loop for ts in (md-keys)
            collecting
              (nab-sym-val-by-ts sym ts
                                 :field field
                                 :interval interval))))
    (if incl-sym-p
        (cons sym r)
        r)))



;; actual fetchers below


;; http://chartapi.finance.yahoo.com/instrument/1.0/VXX/chartdata;type=quote;range=10d/csv

@export
(defun fetch-md (sym &key (from-ts "-1month")(interval :1day))
  (add-sym-data-to-md sym
                      (fetch-daily-yhoo sym :from-time from-ts)
                      :interval interval))

@export ;; todo: this is retarded
(defun fetch-daily-yhoo (sym &key (from-time "-1month") (to-time "now"))
  (let* ((to-dt (if (string= to-time "now")
                   (simple-date-time:now)
                   (apply #'make-date
                          (mapcar #'parse-integer
                                  (cl21.core:split #\- to-time))))) ;; "2016-02-08"
        (from-dt (if (string= from-time "-1month")
                     (normalize (month+ (simple-date-time:now) -1)) ;; ---
                     (apply #'make-date
                            (mapcar #'parse-integer
                                    (cl21.core:split #\- from-time))))))

    (format t "--- fetching yhoo data for ~a from ~a to ~a...~%" sym
            (yyyy-mm-dd from-dt) (yyyy-mm-dd to-dt))

    (mapcar #'alexandria:hash-table-alist
            (cl-yahoo-finance:read-historical-data
             sym
             (list (month-of from-dt) (day-of from-dt) (year-of from-dt))
             (list (month-of to-dt) (day-of to-dt) (year-of to-dt))))))

;;; ---- accessors and gross shit

@export
(defun mdfetch (&optional ts sym field (interval :1day))
  (let ((ts-r (or ts (first (last (md-keys))))))
    (cond ((and field sym)
           (nab-sym-val-by-ts sym ts-r :field field :interval interval))
          (sym
           (fetch-sym-props-by-ts sym ts-r :interval interval))
          (t
           (fetch-md-blob-by-ts ts-r :interval interval)))))

@export
(defun fetch-md-blob-by-ts (ts &key (interval :1day))
  (gethash ts (fetch-md-hashtable-by-int :interval interval)))

;; add ts to hashtable as empty alist if not exists;
;; remove alist key and prop if exists, and set
@export
(defun add-or-update-ht-def (ts hashtable def-key def-val)
  ;;(format t "~a: ~a --> ~a~%" ts def-key def-val)

  (when (not (gethash ts hashtable))
    (setf (gethash ts hashtable) (list)))

  (setf (gethash ts hashtable)
        (acons def-key def-val
               (remove def-key (gethash ts hashtable)
                       :key #'car
                       :test #'string=))))

@export
(defun add-sym-data-to-md
    (sym data &key (field nil) (ts-key :date) (interval :1day))
  "pull a symbols hist data into our md store"

  ;;(format t "-->>> fetching ~a (field ~a) (ts-key ~a)~%"
  ;;        sym field ts-key)

  (mapc
   #'(lambda (l)
       (add-or-update-ht-def
        (rest (assoc ts-key l))
        (fetch-md-hashtable-by-int :interval interval)
        (instr-sym-name sym)
        (if field
            (getf l field)
            l)))
   data)
  (values))

@export
(defun fetch-sym-props-by-ts (sym ts &key (interval :1day))
  (rest
   (assoc sym
          (fetch-md-blob-by-ts ts :interval interval))))

@export
(defun nab-sym-val-by-ts (sym ts &key (field :close) (interval :1day))
  (rest (assoc field
               (fetch-sym-props-by-ts sym ts :interval interval))))

@export
(defun set-sym-val-by-ts (sym ts val &key (field :close) (interval :1day))
  (assert (not (stringp sym)))

  ;;(format t "--- @~a: ~a .~a--> ~a~%"
  ;;        ts sym field val)

  (let* ((orig-alist (fetch-sym-props-by-ts sym ts :interval interval))
         (without-field (remove field orig-alist :key #'car))
         (new-alist (acons field val without-field)))

    (add-or-update-ht-def ts
                          (fetch-md-hashtable-by-int :interval interval)
                          sym new-alist)))

@export
(defun md-as-alist (&key (interval :1day))
  (alexandria:hash-table-alist
   (fetch-md-hashtable-by-int interval)))
|#
