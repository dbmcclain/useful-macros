;; OSX-UUID-Generate-Allegro.lisp
;;
;; DM/SD  08/08
;; --------------------------------------------------

(defpackage :uuidgen
  (:use #:common-lisp)
  (:export
   #:generate))

(in-package :uuidgen)

;; (require :foreign)

(ff:def-foreign-type <c-uuid> (:array :unsigned-char 16))

(ff:def-foreign-call uuid_generate ((uuid (* <c-uuid>)))
  :returning :int)

(defun generate ()
"return a 16 element vector of unsigned 8-bit bytes
containing an OS X UUID generated from the /dev/urandom entropy store
if it is available and of high enough quality. Otherwise a time-based UUID
will be generated and returned."
  
  (ff:with-stack-fobject (uuid '<c-uuid>)
      (uuid_generate uuid)
      (let ((ans (make-array 16
                             :element-type '(unsigned-byte 8))))
        (dotimes (ix 16)
          (setf (aref ans ix) (ff:fslot-value-typed '<c-uuid> nil uuid ix)))
        ans)))
                             