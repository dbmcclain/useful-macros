;; OSX-UUID-Generate.lisp
;;
;; DM/SD  08/08
;; --------------------------------------------------

(defpackage :uuidgen
  (:use #:common-lisp)
  (:export
   #:generate))

(in-package :uuidgen)

(fli:define-foreign-function (_uuid_generate "uuid_generate" :source)
    ((out (:pointer (:unsigned :byte))))
  :documentation "FLI interface to OS X library")

(defun generate ()
"return a 16 element vector of unsigned 8-bit bytes
containing an OS X UUID generated from the /dev/urandom entropy store
if it is available and of high enough quality. Otherwise a time-based UUID
will be generated and returned."
  
  (fli:with-dynamic-foreign-objects ()
    (let ((uuid (fli:allocate-dynamic-foreign-object
                 :type '(:unsigned :byte)
                 :nelems  16)))
      (_uuid_generate uuid)
      (let ((ans (make-array 16
                             :element-type '(unsigned-byte 8))))
        (fli:replace-foreign-array ans uuid
                                   :start1 0
                                   :start2 0
                                   :end2   16)
        ans))))
                             