;; ctypes.lisp -- extended C typedefs for general use
;; An attempt to avoid the use of keyword symbol names for commonly used C typdefs
;; All FLI users should "use" this package to gain common access to these C typdefs.
;;
;; 12/01  DM/MCFA
;; ------------------------------------------------------------------

(defpackage "CTYPES"
  (:nicknames "CT")
  (:use "COMMON-LISP")
  (:export
   "UCHAR"
   "OUT-CSTRING"
   "IN-CSTRING"
   ))

(in-package "CTYPES")

(fli:define-c-typedef uchar        (:unsigned :char))
(fli:define-c-typedef out-cstring  (:pointer uchar))
(fli:define-c-typedef in-cstring   (:pointer uchar))

;; -- end of ctypes.lisp -- ;;
