;; useful_macros.lisp -- A collection of really handy macros
;;
;; DM/HMSC  11/97
;; -----------------------------------------------------------

(in-package "USEFUL-MACROS")

;; -----------------------------------------------------------

#+:lispworks
(progn
  (editor:setup-indent "nlet" 2)
  (editor:setup-indent "nlet-tail" 2)
  (editor:setup-indent "if-let" 2 2 4)
  (editor:setup-indent "when-let" 1)
  (editor:setup-indent "with-gensyms" 2 2 4)
  (editor:setup-indent "with-tail-pure-code" 2 2 4)
  (editor:setup-indent "aif" 2 2 4)
  (editor:setup-indent "aif*" 2 2 4)
  (editor:setup-indent "awhen" 2 2 4)
  (editor:setup-indent "alet" 2)
  (editor:setup-indent "alet-fsm" 2)
  (editor:setup-indent "arun-fsm" 2 2 4)
  (editor:setup-indent "with-slot-accessors" 2 2 4)
  (editor:setup-indent "define-bind*-handler" 2 2)
  (editor:setup-indent "defmacro!" 2)
  (editor:setup-indent "defpan" 2 2 4)
  ;; (editor:setup-indent "dlambda" 2 2 4)
  (editor:setup-indent "dlambda" 0 2 0 t)
  
  (editor:setup-indent "plambda" 2 2 4)
  (editor:setup-indent "ichain-before" 2 2 4)
  (editor:setup-indent "ichain-after" 2 2 4)
  (editor:setup-indent "ichain-intercept" 2 2 4)
  (editor:setup-indent "alet-hotpatch" 2 2 4)
  (editor:setup-indent "let-hotpatch" 2 2 4)
  (editor:setup-indent "sublet" 2 2 4)
  (editor:setup-indent "sublet*" 2 2 4)
  (editor:setup-indent "pandoriclet" 2 2 4)
  (editor:setup-indent "with-pandoric" 2 2 4)
  (editor:setup-indent "defpan" 2)
  (editor:setup-indent "dis" 2 2 4)
  (editor:setup-indent "with-fast-stack" 2 2 4)
  (editor:setup-indent "with-conses-counted" 2 2 4)
  (editor:setup-indent "with-cons-pool" 2 2 4)
  (editor:setup-indent "with-dynamic-cons-pool" 2 2 4)
  (editor:setup-indent "nif" 2 2 4)
  (editor:setup-indent "fast-progn" 2 2 4)
  (editor:setup-indent "safe-progn" 2 2 4)

  (editor:setup-indent "curried-lambda" 1)
  )

;; ------------------------------------------------------

;; -----------------------------------------------------------
;; tools needed to support Doug Hoyte's DEFMACRO!

(eval-when (:compile-toplevel :load-toplevel :execute)

 (defmacro perform (name bindings &body body)
    (let ((args (mapcar 'first bindings))
          (vals (mapcar 'second bindings)))
      `(labels ((,name ,args ,@body))
         (,name ,@vals))
      ))
  
  (defmacro nlet (name bindings &body body)
    ;; NLET = Named LET
    `(perform ,name ,bindings ,@body))
  
  (defun flatten (x)
    (lol:flatten x))

  ;; in ML these are referred to as sections
  ;; these actually correspond to the Dylan operators
  ;; secr ::= rcurry, secl ::= curry
  (defun curry (fn &rest pref-args)
    (lambda (&rest suf-args)
      (apply fn (append pref-args suf-args))))

  (defun rcurry (fn &rest suf-args)
    (lambda (&rest pref-args)
      (apply fn (nconc pref-args suf-args))))
  
  (defun collect-if (predicate proseq &rest rest)
    (apply 'remove-if-not predicate proseq rest))
  
  ;; --------------------------------------------
  ;; Bang-symbols
  (defun bang-symbol-p (prefix s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s) prefix
                  :start1 0
                  :end1   2)))

  (defun get-bang-symbols (prefix body)
    (remove-duplicates
     (collect-if (curry 'bang-symbol-p prefix) (flatten body))))
      
  (defun bang-symbol-name (s)
    (subseq (symbol-name s) 2))
  
  ;; --------------------------------------------

  (defun raw-mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) 
        (princ a s))
      ))
  
  (defun mkstr (&rest args)
    (with-standard-io-syntax
      (apply 'raw-mkstr args)))
  
  (defun correct-for-symbol-character-case (str)
    ;; a portable way to make symbol strings
    ;; Modern Mode vs ANSI
    (if (eql #\a (char (string :a) 0))
        (string-downcase (string str))
      (string-upcase (string str))))

  (defun intern-symbol (str &rest package)
    (apply 'intern (correct-for-symbol-character-case str) package))
  
  (defun symb (&rest args)
    (values (intern-symbol (apply 'mkstr args))))
  
  ;; --------------------------------------------
  ;; Reader macro for #` for producing parameterized BQ lists
  ;; Produces a function that can be applied to arguments
  
  (defun |reader-for-#`| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    (let ((a-args (loop for i from 1 to numarg
                        collect (symb 'a i))))
      `(lambda ,a-args
         (declare (ignorable ,@a-args))
         ,(funcall
           (get-macro-character #\`) stream nil))))
    
  (set-dispatch-macro-character
   #\# #\` '|reader-for-#`|)
  
  ) ;; end of eval-when #1
  
#| ;; example -- a1 is first parameter
(mapcar #`(,a1 (intern ,(bang-symbol-name a1))) syms)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; --------------------------------------------
  ;; A-Bang symbols -- create anaphoric symbol names
  ;; in package of macro expansion

  (defmacro defmacro/a! (name args &body body)
    (let ((syms (get-bang-symbols #.(symbol-name :A!) body)))
      (if syms
          `(defmacro ,name ,args
             (let ,(mapcar #`(,a1 (intern ,(bang-symbol-name a1))) syms)
               ,@body))
        `(defmacro ,name ,args ,@body))))

  ;; --------------------------------------------
  ;; G-Bang symbols -- auto generated gensyms

  (defmacro defmacro/g! (name args &body body)
    (let ((syms (get-bang-symbols #.(symbol-name :G!) body)))
      (if syms
          `(defmacro/a! ,name ,args
             (let ,(mapcar #`(,a1 (gensym ,(bang-symbol-name a1))) syms)
               ,@body))
        `(defmacro/a! ,name ,args ,@body))))

  
  ;; --------------------------------------------
  ;; O-Bang symbols -- once-only eval gensyms

  (defun o!-symbol-to-g!-symbol (s)
    (symb #.(symbol-name :G!)
          (bang-symbol-name s)))
    
  (defmacro defmacro! (name args &body body)
    (let* ((os (get-bang-symbols #.(symbol-name :O!) args))
           (gs (mapcar 'o!-symbol-to-g!-symbol os)))
      ;; o-bang symbols can interfere with find-source
      (if os
          `(defmacro/g! ,name ,args
             `(let ,(mapcar 'list (list ,@gs) (list ,@os))
                ,(progn
                   ,@body)))
        `(defmacro/g! ,name ,args ,@body)) ))

  ;; ---------------------------------------
  ;; This part from Doug Hoyte using Edi's ppcre

  (defun segment-reader (stream ch n)
    (if (> n 0)
        (let ((chars))
          (do ((curr (read-char stream)
                     (read-char stream)))
              ((char= ch curr))
            (push curr chars))
          (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))
  
  ;; ---------------------------------------

  #+cl-ppcre
  (defmacro! match-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:scan
         ,(car ,g!args)
         ,',g!str)))

  #+cl-ppcre
  (defmacro! subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:regex-replace-all
         ,(car ,g!args)
         ,',g!str
         ,(cadr ,g!args))))

  ;; Reader macro for #~ for pattern matching/substitution
  ;; Produces a function that can be applied to strings
  
  #+cl-ppcre
  (defun |reader-for-#~| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
       ((char= mode-char #\m)
        (match-mode-ppcre-lambda-form
         (segment-reader stream
                         (read-char stream)
                         1)))
       ((char= mode-char #\s)
        (subst-mode-ppcre-lambda-form
         (segment-reader stream
                         (read-char stream)
                         2)))
       (t (error "Unknown #~~ mode character")))))

  #+cl-ppcre
  (set-dispatch-macro-character #\# #\~ '|reader-for-#~|)

  #| ;; example
  ;; pattern matching
  (#~m/^[+-]?[0-9][0-9_,]*(\.[0-9_,]*([eEdD][+-]?[0-9]+)?)?/ s)
  ;; pattern substitution
  (#~s/[0-9]/N/ s)
  |#

  ) ;; end of eval-when #2


;; --------------------------------------------------------------
;; Allow extended number syntax:
;;   - embedded underscore separators 123_445.789_443
;;   - allow 1+2j or 1-2j or just 2j, where j in [jJiI]
;;   - allow dates in yyyy/mm/dd format
;;   - allow sexigisimal time in |hh:mm:ss.ss| format (bars needed because of #\:)
;;   - allow hyphenated numbers as in telephone numbers, SSN's, and UUID's

(defun remove-separators (s)
  (delete #\, (delete #\_ s)))
  
(defun match-number (s)
  (multiple-value-bind (start end)
      (#~m/^[+-]?[0-9][0-9_,]*(\.[0-9_,]*([eEdD][+-]?[0-9]+)?)?/ s)
    (when start
      (values (read-from-string (remove-separators (subseq s start end)))
              (subseq s end))
      )))

(defun match-complex-ij (s)
  (#~m/^[iIjJ]$/ s))

(defun convert-real-or-complex (s)
  (multiple-value-bind (val srest)
      (match-number s)
    (when val
      (cond ((= 0 (length srest)) val)
            ((match-complex-ij srest) (complex 0 val))
            ((multiple-value-bind (ival sresti)
                 (match-number srest)
               (and ival
                    (match-complex-ij sresti)
                    (complex val ival))))
            (t nil)))
    ))

(defun convert-sexigisimal (s)
  ;; hh:mm:ss.ss, or hh:mm
  (multiple-value-bind (start end gstart gend)
      (#~m/^([+-])?([0-9]+):([0-9]{1,2})(:[0-9]{1,2}(\.[0-9_,]*)?)?$/ s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((sign   (aref gstart 0))
           (hstart (aref gstart 1))
           (hend   (aref gend   1))
           (mstart (aref gstart 2))
           (mend   (aref gend   2))
           (sstart (aref gstart 3))
           (send   (aref gend   3))
           (sfrac  (aref gstart 4)))
        (ignore-errors
          (let* ((hh (read-from-string (subseq s hstart hend)))
                 (mm (read-from-string (subseq s mstart mend)))
                 (ss (if sstart
                         (read-from-string (remove-separators
                                            (subseq s (1+ sstart) send)))
                       0))
                 (val  (+ (* 60 (+ (* 60 hh) mm))
                          (if sfrac
                              (float ss 1d0)
                            ss))))
            (if (and sign
                     (char= (char s sign) #\-))
                (- val)
              val)
            ))))))

(defun convert-utc-date (s)
  ;; yyyy/mm/dd
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2}) UTC$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart (aref gstart 0))
           (yend   (aref gend   0))
           (mstart (aref gstart 1))
           (mend   (aref gend   1))
           (dstart (aref gstart 2))
           (dend   (aref gend   2)))
        (ignore-errors
          (let* ((yyyy (read-from-string (subseq s ystart yend)))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (dd   (read-from-string (subseq s dstart dend))))
            (encode-universal-time 0 0 0 dd mm yyyy 0)) ;; makes UTC date
          )))))
  
(defun convert-date (s)
  ;; yyyy/mm/dd
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2})$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart (aref gstart 0))
           (yend   (aref gend   0))
           (mstart (aref gstart 1))
           (mend   (aref gend   1))
           (dstart (aref gstart 2))
           (dend   (aref gend   2)))
        (ignore-errors
          (let* ((yyyy (read-from-string (subseq s ystart yend)))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (dd   (read-from-string (subseq s dstart dend))))
            (encode-universal-time 0 0 0 dd mm yyyy))
          )))))
  
(defun convert-american-short-date (s)
  ;; mm/dd/yy 
  (multiple-value-bind (start end gstart gend)
      (#~m%^([0-9]{1,2})/([0-9]{1,2})/([0-9]{1,2})$% s)
    (declare (ignore end))
    (when start
      (symbol-macrolet
          ((ystart (aref gstart 2))
           (yend   (aref gend   2))
           (mstart (aref gstart 0))
           (mend   (aref gend   0))
           (dstart (aref gstart 1))
           (dend   (aref gend   1)))
        (ignore-errors
          (let* ((yyyy (+ 2000 (read-from-string (subseq s ystart yend))))
                 (mm   (read-from-string (subseq s mstart mend)))
                 (dd   (read-from-string (subseq s dstart dend))))
            (encode-universal-time 0 0 0 dd mm yyyy)
            ))))))

(defun convert-hyphenated-number (s)
  ;; xxxx-xx-xxxx  as in telephone numbers, SSN's, and UUID's
  (if (#~m/^[0-9]+(\-[0-9]+)*$/ s)
      (read-from-string (delete #\- s))))
    
(defun convert-other-base-number (s)
  ;; #xNNNN_NNNN_NNN
  (when (or (#~m/^0[xXoObB]/ s)
            (#~m/^[0-9]+[rR]/ s))
    (ignore-errors
      (read-from-string (format nil "#~A" (remove-separators s))))))
    
(defun read-extended-number-syntax (s)
  (cond ((convert-real-or-complex s))
        ((convert-sexigisimal s))
        ((convert-date s))
        ((convert-american-short-date s))
        ((convert-utc-date s))
        ((convert-hyphenated-number s))
        ((convert-other-base-number s))
        ))

;; Reader macro for #N
;; Parses a variety of numbers

(defun |reader-for-#N| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((v (read stream t nil t)))
    (if (symbolp v)
        (or (read-extended-number-syntax (symbol-name v))
            v)
      v)))

(set-dispatch-macro-character
 #\# #\n '|reader-for-#N|)

#| ;; example
#n1_000
#n|12:45|
#n2009/08/15
#n1+2j
|#
;; --------------------------------------
;; Reader macro for #f
;; sets compiler optimization levels
;; #0f .. #3f   #f defaults to #3f for fastest code
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun |reader-for-#F| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (setq numarg (or numarg 3))
    (unless (<= numarg 3)
      (error "Bad value for #f: ~a" numarg))
    `(declare (optimize (speed ,numarg)
                        (safety ,(- 3 numarg))
                        (float ,(- 3 numarg)))))
  
  (set-dispatch-macro-character #\# #\f '|reader-for-#F|)

  ) ;; end of eval-when

(defmacro fast-progn (&rest body)
  `(locally #f ,@body))

(defmacro safe-progn (&rest body)
  `(locally #0f ,@body))

;; --------------------------------------

(defmacro! alet (letargs &rest body)
  `(let ((,a!this) ,@letargs)
     (setq ,a!this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply ,a!this params))))
  
(defmacro! alet-fsm (&rest states)
  `(macrolet ((,a!state (s)
                `(setq ,',a!this #',s)))
     (labels (,@states) #',(caar states))))
  
(defmacro! arun-fsm (bindings feeder &rest clauses)
  `(block ,g!block
     (let ((,g!machine
            (alet ,bindings
                (macrolet ((,a!finish (val)
                             `(return-from ,',g!block ,val)))
                  (alet-fsm ,@clauses)))))
       (tagbody
        ,g!again
        (funcall ,g!machine ,feeder)
        (go ,g!again))
       )))

;; ----------------------------------------------------------------------
;; Nestable suggestion from Daniel Herring
;; rewritten (DM/RAL) using our state-machine macro
;; Nesting fails on patterns like #"#"# where one expects #

(defun |reader-for-#"| (stream sub-char numarg)
   (declare (ignore sub-char numarg))
   (arun-fsm ((chars (make-rubber-vector ;; initial bindings
                        :element-type 'character))
              (depth 1))
             (read-char stream)  ;; feeder clause
             (normal (curr)      ;; state machine clauses - initial first
                     (cond ((char= curr #\#)
                            (state read-sharp)
                            (keep #\#))
                           
                           ((char= curr #\")
                            (state read-quote))
                           
                           (t (keep curr)) ))
             
             (read-sharp (curr)
                         (cond ((char= curr #\")
                                (state normal)
                                (keep #\")
                                (incf depth))
                               
                               #|
                               ((char= curr #\#)
                                (keep #\#))
                               |#

                               (t
                                (keep curr)
                                (state normal)) ))
             
             (read-quote (curr)
                         (cond ((char= curr #\#)
                                (state normal)
                                (decf depth)
                                (when (zerop depth)
                                  (finish (final-string)))
                                (keep #\")
                                (keep #\#))
                               
                               ((char= curr #\")
                                (keep #\"))
                               
                               (t (state normal)
                                  (keep #\")
                                  (keep curr)) ))

             ;; not a state, but becomes a labels clause that can be used
             (keep (ch) (vector-push-extend ch chars))
             (final-string ()
                           (prog1
                               (coerce (subseq chars 0) 'string)
                             (setf (fill-pointer chars) 0)))
             ))

(set-dispatch-macro-character
 #\# #\" '|reader-for-#"|)
  
;; --------------------------------------------
;; Reader macro for #>
;; like the Bourne shell > to-lists for surrounding strings
;;
;; This version is from Martin Dirichs

(defun |reader-for-#>| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars) ;; collect the end tag
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let ((pattern (nreverse chars))
          output)
      ;; collect chars until a sequence of them
      ;; matches the end-tag
      (labels ((match (pos chars)
                 (if (null chars)
                     pos
                   (if (char= (nth pos pattern) (car chars))
                       (match (1+ pos) (cdr chars))
                     (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
        (do (curr
             (pos 0))
            ((= pos (length pattern)))
          (setf curr (read-char stream)
                pos (match pos (list curr)))
          (push curr output))
        (coerce
         (nreverse
          (nthcdr (length pattern) output))
         'string)))))

(set-dispatch-macro-character
 #\# #\> '|reader-for-#>|)

#| ;; example
#>.end
This is a test
of the #> reader macro
.end
|#
;; --------------------------------------------

;; --------------------------------------------
;; Reader macro for #$
;; Takes a list and applies the car as a function to the cdr as parameters
;;
;; This version is from Martin Dirichs

(defvar $-reader-macros (make-hash-table :test 'equalp))

(defun |reader-for-#$| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((inp (read stream t nil t)))
    (if (and (consp inp)
             (symbolp (car inp)))
        (let ((fn (get-$-dispatch-reader (car inp))))
          (if fn
              (apply fn (cdr inp))
            (error "No $-Reader Macro for ~A" (car inp))))
      (error "badly formed #$ input: ~A" inp))))

(defun set-$-dispatch-reader (key fn)
  (unless (symbolp key)
    (error "$-dispatch names must be symbols"))
  (setf (gethash (string key) $-reader-macros) fn))

(defun get-$-dispatch-reader (key)
  (gethash (string key) $-reader-macros))

(set-dispatch-macro-character
 #\# #\$ '|reader-for-#$|)


#|
(set-$-dispatch-reader :test (lambda (&rest data)
                               (match data
                                 ((x) :when (numberp x) (/ x))
                                 (_   (list 'quote data)))))

#$(:test 15)
#$(:test :this)
|#
;; ----------------------------------------------------------
;; Reader for #/
;; Takes a function name and applies to stream following the second '/'

(defvar /-reader-macros (make-hash-table :test 'equalp))

(defun |reader-for-#/| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((key    (first (segment-reader stream #\/ 1)))
         (reader (get-/-dispatch-reader key)))
    (if reader
        (funcall reader stream)
      (error "No /-Reader Macro for ~A" key))))

    
(defun set-/-dispatch-reader (key fn)
  (setf (gethash (string key) /-reader-macros) fn))

(defun get-/-dispatch-reader (key)
  (gethash (string key) /-reader-macros))

(set-dispatch-macro-character
 #\# #\/ '|reader-for-#/|)

#|
(set-/-dispatch-reader "test"
                       (lambda (stream)
                         (let ((data (read stream t nil t)))
                           (match data
                             ((x) :when (numberp x) (/ x))
                             (_   (list 'quote data))))))

#/test/1.2
#/test/this
|#
;; ---------------------------------------------------
;; Symbol Aliases #?name
;; #?name looks up name in per-package alist and returns cdr symbol

(defvar *symbol-aliases-table* (make-hash-table)) ;; one alist per package

(defun aliases (&optional (package *package*))
  (gethash (find-package package) *symbol-aliases-table*))

(defsetf aliases (&optional (package *package*)) (alist)
  `(setf (gethash (find-package ,package) *symbol-aliases-table*) ,alist))

(defun lookup-alias (keysym &optional (package *package*))
  (let* ((keysym-name (symbol-name keysym))
         (package     (find-package package)))
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern keysym-name package)))
    (or (cdr (assoc keysym (aliases package)))
        (error "No alias named ~A" keysym))))
  
(defun alias (keysym sym &optional (package *package*))
  (unless keysym
    (error "Can't alias NIL"))
  (let* ((package (find-package package))
         (alist   (aliases package)))
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern (symbol-name keysym) package)))
    (if sym
        (let ((pair (assoc keysym alist)))
          (if pair
              (setf (cdr pair) sym)
            (setf (aliases package) (acons keysym sym alist))) )
      (setf (aliases package) (delete keysym alist :key 'first))) ))

(defun unalias (keysym &optional (package *package*))
  (alias keysym nil package))

(defun |reader-for-#?| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((key (read stream t nil t)))
    (lookup-alias key)) ) ;; note: can't alias NIL

(set-dispatch-macro-character
 #\# #\? '|reader-for-#?|)

#| ;; example
(alias 'this 'that)
(quote #?this)
(aliases)
(unalias 'this)
|#

;; ---------------------------------------------------
#|
;; Package Aliases #!name
;; #!name looks up name in per-package alist and returns cdr symbol

(defvar *symbol-aliases-table* (make-hash-table)) ;; one alist per package

(defun aliases (&optional (package *package*))
  (gethash (find-package package) *symbol-aliases-table*))

(defsetf aliases (&optional (package *package*)) (alist)
  `(setf (gethash (find-package ,package) *symbol-aliases-table*) ,alist))

(defun lookup-alias (keysym &optional (package *package*))
  (let* ((keysym-name (symbol-name keysym))
         (package     (find-package package)))
    #|
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern keysym-name package)))
    |#
    (or (cdr (assoc keysym (aliases package)))
        (error "No alias named ~A" keysym))))
  
(defun alias (keysym sym &optional (package *package*))
  (unless keysym
    (error "Can't alias NIL"))
  (let* ((package (find-package package))
         (alist   (aliases package)))
    #|
    (unless (eq package (symbol-package keysym))
      (setf keysym (intern (symbol-name keysym) package)))
    |#
    (if sym
        (let ((pair (assoc keysym alist)))
          (if pair
              (setf (cdr pair) sym)
            (setf (aliases package) (acons keysym sym alist))) )
      (setf (aliases package) (delete keysym alist :key 'first))) ))

(defun unalias (keysym &optional (package *package*))
  (alias keysym nil package))

(defun |reader-for-#?| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let* ((key (read stream t nil t)))
    (lookup-alias key)) ) ;; note: can't alias NIL

(set-dispatch-macro-character
 #\# #\? '|reader-for-#?|)

#| ;; example
(alias 'this 'that)
(quote #?this)
(aliases)
(unalias 'this)
|#
|#
;; ------------------------------------------------------------

(defun read-chars-till-delim (stream delims &rest first-char)
  (let ((chars (copy-list first-char)))
    (do ((ch (read-char stream)
             (read-char stream)))
        ((find ch delims))
      (push ch chars))
    (coerce (nreverse chars) 'string)))
             
;; -------------------------------------------------------

(defmacro! defaliasfn (new-name old-name)
  `(defun ,new-name (&rest ,g!args)
     (apply ',old-name ,g!args)))

(defmacro! defcapture (new-name old-name)
  `(unless (fboundp ',new-name)
     (when (fboundp ',old-name)
       (setf (symbol-function ',new-name) (symbol-function ',old-name)))))
     
;; -------------------------------------------------------

