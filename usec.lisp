;; usec.lisp -- timestamps to the nearest microsecond, when supported
;; DM/RAL  08/09
;; ----------------------------------------------------------------

(defpackage #:usec
  (:use #:common-lisp)
  (:export
   #:get-time-usec
   #:adjust-to-standard-universal-time-usec
   #:get-universal-time-usec
   ))

;; ----------------------------------------------------------------
(in-package :usec)
;; ----------------------------------------------------------------
;; ----------------------------------------------------------------
;; Timestamps to the nearest microsecond

;;--- MAC OS/X ---

#+(AND :LISPWORKS :MACOSX)
(PROGN
 (fli:define-foreign-function (_get-time-of-day "gettimeofday" :source)
    ((tsinfo :pointer)
     (tzinfo :pointer)
			       ))

 (defun get-time-usec ()
   ;; time since midnight Jan 1, 1970, measured in microseconds
   (fli:with-dynamic-foreign-objects ()
	(um:bind*
	 ((arr (fli:allocate-dynamic-foreign-object
		:type   '(:unsigned :long)
		:nelems 2
		:fill   0)))
	 
	 (_get-time-of-day arr fli:*null-pointer*)
	 (+ (* 1000000 (the integer (fli:dereference arr :index 0)))
	    (the integer (fli:dereference arr :index 1)))
	 )))

 (defun adjust-to-standard-universal-time-usec (tm)
   (declare (integer tm))
   #F
   (+ tm #.(* 1000000 (encode-universal-time 0 0 0 1 1 1970 0)))))

(defun get-universal-time-usec ()
  (adjust-to-standard-universal-time-usec (get-time-usec)))

;; ------- WIN/32 -----------------
#|
#+(AND :LISPWORKS :WIN32)
(fli:define-foreign-function (_getSystemTime "GetSystemTime" :source)
    ((lpSystemTime :pointer)))

#+(AND :LISPWORKS :WIN32)
(defun get-system-time ()
  ;; only provides information to 1ms resolution
  (fli:with-dynamic-foreign-objects ()
    (let ((buf (fli:allocate-dynamic-foreign-object
                :type :uint16 :nelems 8)))
      (_getSystemTime buf)
      (loop for ix from 0 below 8 collect
            (fli:dereference buf :index ix)))))
|#

#|
#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defvar *ut-delta*
  (locally
    #f
    (let ((utc (get-universal-time))
          (now (get-internal-real-time)))
      (declare (integer utc now))
      ;; only accurate to nearest second
      (- (* internal-time-units-per-second utc) now))))

#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defun adjust-to-standard-universal-time-usec (tm)
  (declare (integer tm))
  #F
  tm)

#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defvar *last-time* 0)
#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defvar *time-incr* 0)
#+(OR :ALLEGRO (AND :LISPWORKS :WIN32))
(defun get-time-usec ()
  (let ((this-time (get-internal-real-time)))
    (if (= this-time *last-time*)
        (incf *time-incr*)
      (setf *time-incr* 0
            *last-time* this-time))
    (+ *time-incr*
       (/ (* 1000000 (+ this-time *ut-delta*))
          internal-time-units-per-second))
    ))
|#

#+(OR :WIN32 :ALLEGRO)
(progn
  (defvar *last-time* 0)
  (defvar *time-incr* 0)

  (defun get-time-usec ()
    (let ((now (get-universal-time)))
      (if (= now *last-time*)
	  (incf *time-incr*)
	(setf *time-incr* 0
	      *last-time* now))
      (+ (* now 1000000) *time-incr*)))
  
  (defun adjust-to-standard-universal-time-usec (tm)
    tm))

