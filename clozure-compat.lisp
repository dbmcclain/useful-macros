;; clozure-compat.lisp -- compatibility layer for Clozure Lisp
;; DM/RAL  02/09
;; -------------------------------------------------------------

#+:CLOZURE
(defpackage :clos
  (:use #:COMMON-LISP)
  (:import-from #:CCL
		#:standard-slot-definition
		#:standard-direct-slot-definition
		#:standard-effective-slot-definition
		#:direct-slot-definition-class
		#:effective-slot-definition-class
		#:compute-effective-slot-definition
		#:slot-value-using-class
		#:class-slots
		#:slot-definition-name
		#:slot-boundp-using-class
		#:slot-makunbound-using-class
		#:validate-superclass
		#:method-specializers
		#:generic-function-methods
		#:compute-class-precedence-list
		#:finalize-inheritance
		)
  (:export
   #:standard-slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-class
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:compute-effective-slot-definition
   #:slot-value-using-class
   #:class-slots
   #:slot-definition-name
   #:slot-boundp-using-class
   #:slot-makunbound-using-class
   #:generic-function-methods
   #:validate-superclass
   #:method-specializers
   #:compute-class-precedence-list
   #:finalize-inheritance
   #:class-name
   ))
  

#+:CLOZURE
(defpackage :mp
  (:use #:COMMON-LISP)
  (:import-from #:CCL
		#:*current-process*
		#:make-process
		#:process-suspend
		#:process-resume
		#:process-suspend-count
		#:process-preset
		#:process-enable
		#:process-run-function
		#:process-interrupt
		#:process-reset
		#:process-kill
		#:process-abort
		#:*ticks-per-second*
		#:process-whostate
		#:process-allow-schedule
		#:process-wait
		#:process-wait-with-timeout
		#:without-interrupts
		#:make-lock
		#:with-lock-grabbed
		#:grab-lock
		#:release-lock
		#:try-lock
		#:make-read-write-lock
		#:with-read-lock
		#:with-write-lock
		#:make-semaphore
		#:signal-semaphore
		#:wait-on-semaphore
		#:timed-wait-on-semaphore
		#:process-input-wait
		#:process-output-wait
		#:with-terminal-input
		#:*request-terminal-input-via-break*
		#:join-process
		#:process-name)
  (:export
   #:*current-process*
   #:make-process
   #:process-suspend
   #:process-resume
   #:process-suspend-count
   #:process-preset
   #:process-enable
   #:process-run-function
   #:process-interrupt
   #:process-reset
   #:process-kill
   #:process-abort
   #:*ticks-per-second*
   #:process-whostate
   #:process-allow-schedule
   #:process-wait
   #:process-wait-with-timeout
   #:without-interrupts
   #:make-lock
   #:with-lock-grabbed
   #:grab-lock
   #:release-lock
   #:try-lock
   #:make-read-write-lock
   #:with-read-lock
   #:with-write-lock
   #:make-semaphore
   #:signal-semaphore
   #:wait-on-semaphore
   #:timed-wait-on-semaphore
   #:process-input-wait
   #:process-output-wait
   #:with-terminal-input
   #:*request-terminal-input-via-break*
   #:join-process
   #:process-name
   ))

#+:CLOZURE
(defpackage :stream
  (:use #:COMMON-LISP)
  (:import-from #:CCL
		#:fundamental-binary-output-stream
		#:fundamental-binary-input-stream
                #:stream-write-byte
                #:stream-read-byte)
  (:export
   #:fundamental-binary-output-stream
   #:stream-write-byte
   #:fundamental-binary-input-stream
   #:stream-read-byte
   ))

