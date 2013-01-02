;; remembered-filenames.lispp -- convenience macros for remembering
;; prompted filenames
;;
;; DM/RAL 11/10
;; -------------------------------------------------------------

(in-package :um)

;; ----------------------------------------------------------

(defvar *remembered-filenames*
  (make-hash-table))

(defun remember-filename (key fname)
  (setf (gethash key *remembered-filenames*) fname))

(defun remembered-filename (key)
  (gethash key *remembered-filenames*))

(defun do-with-remembered-filename (key init prompter fn)
  (um:when-let (fname (or init (funcall prompter (remembered-filename key))))
    (remember-filename key fname)
    (funcall fn fname)))
  
(defmacro with-remembered-filename ((fname key &optional init) form &body body)
  `(do-with-remembered-filename ,key ,init
                                (lambda (,fname)
                                  (declare (ignorable ,fname))
                                  ,form)
                                (lambda (,fname)
                                  (declare (ignorable ,fname))
                                  ,@body)))

#+:LISPWORKS
(editor:setup-indent "with-remembered-filename" 2)

;; ----------------------------------------------------------

(defvar *last-timestamp* nil)
(defvar *timestamp-index* 0)

(defun filename-timestamp-string ()
  (let ((now (get-universal-time)))
    (unless (eql now *last-timestamp*)
      (setf *last-timestamp*  now
            *timestamp-index* 0))
    (multiple-value-bind (ss mm hh dd mon yr)
        (decode-universal-time now 0)
      (format nil "~A~{~{~2,'0d~}-~}~d"
              yr (list
                  (list mon dd)
                  (list hh mm ss))
              (incf *timestamp-index*)))))

(defun add-timestamp-to-filename (fname)
  (concatenate 'string
               (pathname-name fname)
               "-"
               (filename-timestamp-string)
               "."
               (pathname-type fname)))
