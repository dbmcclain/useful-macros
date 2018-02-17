;; useful_macros.lisp -- A collection of really handy macros
;;
;; DM/HMSC  11/97
;; -----------------------------------------------------------

(in-package "USEFUL-MACROS")

;; -----------------------------------------------------------

(defun wholepart (x)
  (truncate x))

(defun fracpart (x)
  (- x (wholepart x)))

;; ----------------------------------------

(defmacro with (bindings &body body)
  `(let* ,bindings ,@body))

(defmacro letp (bindings &body body)
  `(let ,bindings ,@body))

#+:LISPWORKS
(editor:setup-indent "with" 1)
#+:LISPWORKS
(editor:setup-indent "letp" 1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun collect-decls (forms)
    (nlet iter ((forms forms)
                (decls nil))
      (let ((form (car forms)))
        (if (or (stringp form)
                (and (consp form)
                     (eq (car form) 'declare)))
            (iter (cdr forms) (cons form decls))
          (values forms (nreverse decls))
          ))))
  
  (defun symbol-gensym (s)
    (gensym (format nil "~A-" (symbol-name s))))
  
  ) ;; end of eval-when
  
(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (mapcar (lambda (arg)
                      (declare (ignore arg))
                      (gensym))
                    letargs))
        (gsx (mapcar (lambda (arg)
                       (declare (ignore arg))
                       (gensym))
                     letargs)))
    (multiple-value-bind (body decls)
        (collect-decls body)
      `(macrolet
           ((,n ,gs
              `(progn
                 (psetq
                  ,@(apply 'nconc
                           (mapcar
                            'list
                            ',gsx
                            (list ,@gs))))
                 (go ,',g!n))))
         (block ,g!b
           (let ,(mapcar #2`(,a1 ,(cadr a2)) gsx letargs)
             (tagbody
              ,g!n
              (let ,(mapcar #2`(,(car a2) ,a1) gsx letargs)
                ,@decls
                (return-from
                    ,g!b (progn ,@body)))))) ))))

  
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #`(,a1 (symbol-gensym ',a1)) syms)
     ,@body))

;; -------------------------------------------------------------------
;; WITH-TAIL-PURE-CODE -- Compiled code using this macro can run recursively
;; all day long without ever blowing the stack.
;; Tail calls are effectively made into direct jumps.
;;
;; N.B. DM/RAL 02/07 -- tests with LWM 5.01 indicate that this may be
;; unnecessary. Code must be compiled for tail optimization. Interpreted code
;; has problems no matter what.
(defmacro with-tail-pure-code (&body body)
  `(locally
     (declare (optimize (debug 1) (safety 1)))
     ,@body))

;; ----------------------------------------------------------------------

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (let ((,g!tail (cdr ,g!args)))
       (case (car ,g!args)
         ,@(mapcar
            (lambda (d)
              `(,(if (eq t (car d))
                     t
                   (list (car d))
                   ;; (car d)
                   )
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                            g!args
                          g!tail)) ))
            ds)))))

(defmacro dcase (args &rest clauses)
  `(apply (um:dlambda
              ,@clauses)
          ,args))

#+:LISPWORKS
(editor:setup-indent "dcase" 1)

;; ----------------------------------------------------------------------
#|
(um:defmacro! make-state-machine ((initial-state inp-var
                                                 &key on-reset)
                                  &rest state-bindings)
  `(let ((,g!state ,initial-state)
         ,g!state-stack)
     (macrolet ((push-state (state)
                  `(push ,state ,',g!state-stack))
                (pop-state ()
                  `(pop ,',g!state-stack)))
       (dlambda
        (:reset ()
         (setf ,g!state       ,initial-state
               ,g!state-stack nil)
         ,on-reset)

        (t (,inp-var)
           (cond ,@(mapcar
                    (lambda (binding)
                      (destructuring-bind (from-state &rest sub-clauses)
                          binding
                        (let (t-seen)
                          `((eq ,g!state ,from-state)
                            (cond ,@(mapcar
                                     (lambda (subclause)
                                       (destructuring-bind (test to-state &rest clause)
                                           subclause
                                         (setf t-seen (or t-seen
                                                          (eq test 't)
                                                          (eq test 'otherwise)))
                                         `(,test
                                           (setf ,g!state ,to-state)
                                           ,@clause)))
                                     sub-clauses)
                                  ,@(unless t-seen
                                      `((t (error "State machine error in state: ~A"
                                                  ,g!state)))) ))
                          )))
                    state-bindings)
                 (t (error "State machine error")) ))) )))

  #+:LISPWORKS
(editor:setup-indent "make-state-machine" 2 4)

;; ----------------------------------------------------------------------

(um:defmacro! run-state-machine ((initial-state (inp-var step-expr)) &rest state-bindings)
  `(let ((,g!machine (make-state-machine (,initial-state ,inp-var) ,@state-bindings)))
     (block ,g!block
       (tagbody
        ,g!top
        (multiple-value-bind (,g!ans ,g!done)
            (funcall ,g!machine ,step-expr)
          (if ,g!done
              (return-from ,g!block ,g!ans)
            (go ,g!top))
          )))))

#+:LISPWORKS
(editor:setup-indent "run-state-machine" 2 4)
|#
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------

(defun make-rubber-vector (&key (length 16) element-type)
  (make-array length
              :fill-pointer 0
              :adjustable t
              :element-type element-type))

;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------

(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env ,a!this))
     (setq ,a!this
       (lambda (&rest ,g!temp-args)
         ,@body
         (apply ,g!indir-env
                ,g!temp-args)))))

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env ,a!this))
     (setq ,a!this
       (lambda (&rest ,g!temp-args)
         (prog1
           (apply ,g!indir-env
                  ,g!temp-args)
           ,@body)))))

(defmacro! ichain-intercept (&rest body)
  `(let ((,g!indir-env ,a!this))
     (setq ,a!this
       (lambda (&rest ,g!temp-args)
         (block ,g!intercept
           (macrolet ((,a!intercept (v)
                       `(return-from
                          ,',g!intercept
                          ,v)))
             (prog1
               (apply ,g!indir-env
                      ,g!temp-args)
               ,@body)))))))

(defmacro! alet-hotpatch (letargs &rest body)
  `(let ((,a!this) ,@letargs)
     (setq ,a!this ,@(last body))
     ,@(butlast body)
     (dlambda
       (:hotpatch (closure)
         (setq ,a!this closure))
       (t (&rest args)
         (apply ,a!this args)))))

(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
       (:hotpatch (closure)
         (setq ,g!this closure))
       (t (&rest args)
         (apply ,g!this args)))))

(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))

;; ----------------------------------------------------------------------

(defun tree-leaves%% (tree test result)
  (if tree
    (if (listp tree) 
      (cons
        (tree-leaves%% (car tree) test result)
        (tree-leaves%% (cdr tree) test result))
      (if (funcall test tree)
        (funcall result tree)
        tree))))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
     ,tree
     (lambda (x)
       (declare (ignorable x))
       ,test)
     (lambda (x)
       (declare (ignorable x))
       ,result)))

(defmacro sublet (bindings% &rest body)
  (let ((bindings (let-binding-transform
                    bindings%)))
    (setq bindings
      (mapcar
        (lambda (x)
          (cons (gensym (symbol-name (car x))) x))
        bindings))
    `(let (,@(mapcar 'list
                     (mapcar 'car bindings)
                     (mapcar 'caddr bindings)))
       ,@(tree-leaves
           body
           #1=(member x bindings :key 'cadr)
           (caar #1#)))))

(defmacro sublet* (bindings &rest body)
  `(sublet ,bindings
     ,@(mapcar 'macroexpand-1 body)))

(defmacro! pandoriclet (letargs &rest body)
  (let ((letargs (cons
                   '(a!this)
                   (let-binding-transform
                     letargs))))
    `(let (,@letargs)
       (setq ,a!this ,@(last body))
       ,@(butlast body)
       (dlambda
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
           (apply ,a!this args))))))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1)) letargs)
     (t (error
          "Unknown pandoric get: ~a"
          sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                   (setq ,(car a1) val))
               letargs)
     (t (error
          "Unknown pandoric set: ~a"
          sym))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro! with-pandoric (syms o!box &rest body)
  `(symbol-macrolet
     (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                syms))
     ,@body))

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))

(defmacro! pandoric-recode (vars box new)
  `(with-pandoric (,a!this ,@vars) ,box
     (setq ,a!this ,new)))

(defmacro! plambda (largs pargs &rest body)
  (let ((pargs (mapcar 'list pargs)))
    `(let (,a!this ,a!self)
       (setq
         ,a!this (lambda ,largs ,@body)
         ,a!self (dlambda
                (:pandoric-get (sym)
                  ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)
                  ,(pandoriclet-set pargs))
                (t (&rest args)
                  (apply ,a!this args)))))))

(defmacro! defpan (name args &rest body)
  `(defun ,name (,a!self)
     ,(if args
        `(with-pandoric ,args ,a!self
           ,@body)
        `(progn ,@body))))

(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
           (plambda () ,vars t)))
     (eval `(with-pandoric
              ,',vars pandoric-eval-tunnel
              ,,expr))))

;; ----------------------------------------------------------------------

(defmacro dis (args &rest body)
  `(disassemble
     (compile nil
       (lambda ,(mapcar (lambda (a)
                          (if (consp a)
                            (cadr a)
                            a))
                        args)
         (declare
           ,@(mapcar
               #`(type ,(car a1) ,(cadr a1))
               (remove-if-not 'consp args)))
         ,@body))))

;; ----------------------------------------------------------------------

(defmacro! pointer-& (obj)
  `(lambda (&optional (,g!set ',g!temp))
     (if (eq ,g!set ',g!temp)
       ,obj
       (setf ,obj ,g!set))))

(defun pointer-* (addr)
  (funcall addr))

(defsetf pointer-* (addr) (val)
  `(funcall ,addr ,val))

(defsetf pointer-& (addr) (val)
  `(setf (pointer-* ,addr) ,val))

;; ----------------------------------------------------------------------

(defmacro! with-fast-stack
           ((sym &key (type 'fixnum) (size 1000)
                      (safe-zone 100))
            &rest body)
  `(let ((,g!index ,safe-zone)
         (,g!mem (make-array ,(+ size (* 2 safe-zone))
                             :element-type ',type)))
     (declare (type (simple-array ,type) ,g!mem)
              (type fixnum ,g!index))
     (macrolet
       ((,(symb 'fast-push- sym) (val)
            `(locally #f
               (setf (aref ,',g!mem ,',g!index) ,val)
               (incf ,',g!index)))
         (,(symb 'fast-pop- sym) ()
            `(locally #f
               (decf ,',g!index)
               (aref ,',g!mem ,',g!index)))
         (,(symb 'check-stack- sym) ()
            `(progn
               (if (<= ,',g!index ,,safe-zone)
                 (error "Stack underflow: ~a"
                        ',',sym))
               (if (<= ,,(- size safe-zone)
                       ,',g!index)
                 (error "Stack overflow: ~a"
                        ',',sym)))))
         ,@body)))

;; ----------------------------------------------------------------------

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil)) ;; For gc
      (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defvar number-of-conses 0)

(declaim (inline counting-cons))

(defun counting-cons (a b)
  (incf number-of-conses)
  (cons a b))

(defmacro! with-conses-counted (&rest body)
  `(let ((,g!orig number-of-conses))
     ,@body
     (- number-of-conses ,g!orig)))

(defmacro counting-push (obj stack)
  `(setq ,stack (counting-cons ,obj ,stack)))

(defmacro with-cons-pool (&rest body)
  `(let ((cons-pool)
         (cons-pool-count 0)
         (cons-pool-limit 100))
     (declare (ignorable cons-pool
                         cons-pool-count
                         cons-pool-limit))
     ,@body))

(defmacro! cons-pool-cons (o!car o!cdr)
  `(if (= cons-pool-count 0)
     (counting-cons ,g!car ,g!cdr)
     (let ((,g!cell cons-pool))
       (decf cons-pool-count)
       (setf cons-pool (cdr cons-pool))
       (setf (car ,g!cell) ,g!car
             (cdr ,g!cell) ,g!cdr)
       ,g!cell)))

(defmacro! cons-pool-free (o!cell)
  `(when (<= cons-pool-count
             (- cons-pool-limit 1))
     (incf cons-pool-count)
     (setf (car ,g!cell) nil)
     (push ,g!cell cons-pool)))

(defmacro make-cons-pool-stack ()
  `(let (stack)
     (dlambda
       (:push (elem)
         (setf stack
               (cons-pool-cons elem stack)))
       (:pop ()
         (if (null stack)
           (error "Tried to pop an empty stack"))
         (let ((cell stack)
               (elem (car stack)))
           (setf stack (cdr stack))
           (cons-pool-free cell)
           elem)))))

(with-cons-pool
  (defun make-shared-cons-pool-stack ()
    (make-cons-pool-stack)))

(defmacro with-dynamic-cons-pools (&rest body)
  `(locally (declare (special cons-pool
                              cons-pool-count
                              cons-pool-limit))
     ,@body))

(defmacro fill-cons-pool ()
  `(let (tp)
     (loop for i from cons-pool-count 
                 to cons-pool-limit
           do (push
                (cons-pool-cons nil nil)
                tp))
     (loop while tp
           do (cons-pool-free (pop tp)))))

;; ----------------------------------------------------------------------

(defmacro! nif (o!expr pos zero neg)
  `(cond ((plusp ,g!expr) ,pos)
         ((zerop ,g!expr) ,zero)
         (t ,neg)))

#|
(nif x '+ 0 '-)
(defmacro! square (o!x)
  `(* ,g!x ,g!x))
(square expr)
|#

;; ----------------------------------------------------------------------

(declaim (inline last1 single append1 conc1 mklist))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defun single (arg)
  (and (consp arg)
       (null (cdr (the cons arg)))))

(defun last1 (lst)
  (car (the cons (last lst))))

;; ----------------------------------------------------
;; Special macro versions of functional composition operators
;; All of these expect a parenthesized list of named dummy args
;; immediately following the EXPANDED-xxx macro name, and prior to
;; the actual arguments
;;
(defmacro expanded-combine ((&rest args) op f1 f2)
  `(lambda ,args
     (funcall ,op (funcall ,f1 ,@args) (funcall ,f2 ,@args))))

(defmacro expanded-compose ((&rest args) &rest fns)
  (cond ((null fns) `'identity)
        ((single fns) (car fns))
        ((single (rest fns))
         `(lambda ,args
            (funcall ,(first fns) (funcall ,(second fns) ,@args))
            ))
        (t  (let ((fn1 (last1 fns))
                  (fns (butlast fns)))
              `(lambda ,args
                 (foldr 'funcall (list ,@fns) (funcall ,fn1 ,@args)))
              ))
        ))

(defmacro expanded-curry ((&rest suf-args) f &rest pref-args)
  `(lambda ,suf-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defmacro expanded-rcurry ((&rest pref-args) f &rest suf-args)
  `(lambda ,pref-args
     (funcall ,f ,@pref-args ,@suf-args)))

(defmacro curried-lambda ((&rest args) &body body)
  (if (rest args)
      `(lambda (,(first args))
         (curried-lambda ,(rest args)
                         ,@body))
    `(lambda ,args
       ,@body)))

#|
;; try them out...
(expanded-compose (tree) 'first 'second 'fifth)
(expanded-compose (tree) 'first 'second)
(expanded-combine (x) '+ 'second 'third)
(expanded-rcurry (seq) 'subseq start end)
(expanded-curry  (val) '* 3)

(curried-lambda (a b c) (list a b c))
|#
;; -------------------------------------------------------------

(defmacro! allf (o!val &rest places)
  `(setf ,@(mapcan (rcurry 'list #|place|# g!val) places)))

(defmacro nilf (&rest places)
  `(allf nil ,@places))

#|
(nilf this that thother)
|#

(defmacro tf (&rest places)
  `(allf t ,@places))


;; ------------------------------------
(define-modify-macro addf (&rest args)
  +)

(define-modify-macro subf (&rest args)
  -)

(define-modify-macro mulf (&rest args)
  *)

(define-modify-macro divf (&rest args)
  /)

;; ------------------------------------
(defmacro deletef (place item &rest args)
  `(setf ,place (delete ,item ,place ,@args)))

(defmacro deletef-if (place pred &rest args)
  `(setf ,place (delete-if ,pred ,place ,@args)))

(defmacro removef (place item &rest args)
  `(setf ,place (remove ,item ,place ,@args)))

(defmacro removef-if (place pred &rest args)
  `(setf ,place (remove-if ,pred ,place ,@args)))

(defmacro aconsf (place key val)
  `(setf ,place (acons ,key ,val ,place)))

(defmacro conc1f (place obj)
  `(setf ,place (nconc ,place (list ,obj))))

;; ------------------------------------

(defmacro self-init (place &body body)
  `(or ,place
       (setf ,place
             (progn
               ,@body))))

#+:LISPWORKS
(editor:setup-indent "self-init" 1)

(defmacro! ensure-assoc ((key place &rest args) &body body)
  `(let ((,g!key ,key))
     (or (assoc ,g!key ,place ,@args)
         (car (aconsf ,place ,g!key (progn ,@body))) )))
       
;; ------------------------------------

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro foreach (fn &rest seqs)
  `(map nil ,fn ,@seqs))

(defmacro if-let ((var val) t-clause &optional f-clause)
  `(let ((,var ,val))
     (if ,var
         ,t-clause
       ,f-clause)))

(defmacro when-let ((var val) &body body)
  `(let ((,var ,val))
     (when ,var
       ,@body)))

;; ------------------------------------------------------------
;; if-let*
;;
;;  (if-let* ((s1 e1)
;;            (s2 e2)
;;             ...  )
;;      (... true clause ...)
;;    (... false clause ....))
;;
;; => true-clause is only executed if *all* of the expressions in the let clauses are true
;;    false-clause is executed if *any* of the expressions is false
;;    bindings of sequential symbols in the let clauses are like let*, so that later clauses
;;    may refer to symbols representing the tests of eariler clauses.
;;    let clauses are evaluated in the order stated, and short-circuit evaluation is performed
;;    so that a false let clause aborts the evaluation of all later let clauses.

(defmacro when-let* (let-clauses true-clause)
  (labels ((generate-when-let* (lets)
             (if lets
                 `(when-let ,(first lets)
                      ,(generate-when-let* (rest lets)))
               true-clause)))
    (generate-when-let* let-clauses)))

#|
(when-let* ((a ea) (b eb)) tc)
|#

(defmacro! if-let* (let-clauses true-clause &optional false-clause)
  (if false-clause
      (labels ((generate-if-let* (lets)
                 (if lets
                     `(if-let ,(first lets)
                          ,(generate-if-let* (rest lets))
                        (go ,g!false))
                   `(return-from ,g!block ,true-clause)) ))
        `(block ,g!block
           (tagbody
            ,(generate-if-let* let-clauses)
            ,g!false
            (return-from ,g!block ,false-clause))))
    `(when-let* ,let-clauses ,true-clause) ))

#|
(if-let* ((a ea) (b eb)) tc fc)
(if-let* ((a ea) (b eb)) tc)
|#

 
;; -------------------------------------------------------
;; Define our own collector objects that
;; perform rapid nconc list accumulation
;;
;; DM/RAL 02/07 -- added a Lock for MP safety

(defclass <collector> ()
  ((hd   :accessor collector-hd)
   (tl   :accessor collector-tl)
   (lock :accessor collector-lock)))

(defmacro with-locked-collector ((c &rest lock-args) &body body)
  `(bt:with-lock-held ((collector-lock ,c) ,@lock-args)
     ,@body))

(defun collector-discard-contents (c)
  (with-locked-collector (c)
    (let ((v (list nil)))
      (setf (collector-hd c) v
            (collector-tl c) v)
      )))

(defmethod initialize-instance ((c <collector>) &key &allow-other-keys)
  (setf (collector-lock c) (bt:make-lock "Collector Lock"))
  (collector-discard-contents c))

(defun collector-contents (c &key (discard t))
  ;; make readout destructive to the collector object
  ;; so that we can't accidentally hand off a list that
  ;; is still undergoing active mutation
  ;;
  ;; If user doesn't want to discard contents,
  ;; then hand him a copy of the contents as they exist at this moment.
  (with-locked-collector (c)
    (let ((lst (cdr (the cons (collector-hd c)))))
      (if discard
          (progn
            (collector-discard-contents c)
            lst)
        (copy-seq lst))
      )))
    
(defun collector-ncontents (c)
  (with-locked-collector (c)
    (length (cdr (the cons (collector-hd c))))
    ))

(defun collector-empty-p (c)
  (zerop (collector-ncontents c)))

(defun collector-append-item (c item)
  (with-locked-collector (c)
    (setf (collector-tl c)
          (cdr (the cons (rplacd (the cons (collector-tl c)) (list item))))
          )))

(defun collector-push-item (c item)
  (with-locked-collector (c)
    (setf (collector-hd c)
          (cons nil (the cons (rplaca (the cons (collector-hd c)) item)))
          )))

(defun collector-pop (c)
  (with-locked-collector (c)
    (let* ((lst (collector-contents c))
           (v   (car lst)))
      (unless (endp lst)
        (setf (collector-hd c) lst))
      v)))

(defun make-collector ()
  (make-instance '<collector>))
  

;; ---------------------------------------------------------------------
#+:lispworks
(defun constituent (c)
  (and (graphic-char-p c)
       (not (lispworks:whitespace-char-p c))))

#-:lispworks
(progn
  (defvar *whitespace-chars*
      (list #\space #\tab #\newline #\return #\backspace #\Page))
  
  (defun whitespace-char-p (c)
    (member c *whitespace-chars*))
  
  (defun constituent (c)
    (and (graphic-char-p c)
         (not (whitespace-char-p c)))))

#|
;; show whitespace chars
(dolist (item
         (loop for ix from 0 below 256
               for c = (code-char ix)
               when (lw:whitespace-char-p c)
               collect (list ix c)))
  (format t "~%0x~2,'0x ~S" (first item) (second item)))

;; show graphics chars
(dolist (item
         (loop for ix from 0 below 256
               for c = (code-char ix)
               when (graphic-char-p c)
               collect (list ix c)))
  (format t "~%0x~2,'0x ~S" (first item) (second item)))
|#

(defun tokens (str &key (test 'constituent) test-not (start 0) end key)
  (let ((test (if test-not
                  (complement test-not)
                test)))
    (loop for p1 = (position-if test str :start start :end end :key key)
          while p1
          do (setf start (position-if-not test str :start p1 :end end :key key))
          collect (subseq str p1 (or start end))
          while start
          )))

(defun tokens-if (test str &rest args)
  (apply 'tokens str :test test args))

(defun tokens-if-not (test-not str &rest args)
  (apply 'tokens str :test-not test-not args))

(defun split-string (str &key delims (start 0) end key)
  (if delims
      (tokens-if-not (rcurry 'find #|c|# delims) str
                     :start start :end end :key key)
    (tokens str :start start :end end :key key)
    ))


(defun paste-strings (delim &rest args)
  (with-output-to-string (s)
    (when args
      (princ (car args) s)
      (dolist (arg (cdr args))
        (princ delim s)
        (princ arg s)
        ))
    ))
      
;; ----------------------------------------------------------------

(defmacro fn (args &body body)
  `(lambda ,args ,@body))

(defmacro if* (test tclause &rest fclauses)
  `(if ,test ,tclause
       ,(if (< (length fclauses) 2)
            (first fclauses)
          `(if* ,(first fclauses)
                ,(second fclauses)
                ,@(cddr fclauses))
          )))

;; Graham's alambda
(defmacro! alambda (parms &body body)
  `(labels ((,a!self ,parms ,@body))
     #',a!self))

(defmacro! aif (test tclause &optional fclause)
  `(let ((,a!it ,test))
     (if ,a!it ,tclause ,fclause)))

(defmacro! aif* (test tclause &rest fclauses)
  `(let ((,a!it ,test))
     (if ,a!it ,tclause
       ,(if (< (length fclauses) 2)
            (first fclauses)
          `(aif* ,(first fclauses)
                 ,(second fclauses)
                 ,@(cddr fclauses))
          ))))

(defmacro! awhen (test &rest clauses)
  `(let ((,a!it ,test))
     (when ,a!it ,@clauses)))

;; ------------------------------------------------------------------
(defmethod longer ((x list) (y list))
  (do ((lx x (cdr lx))
       (ly y (cdr ly)))
      ((or (null lx)
           (null ly)) lx)))

(defmethod longer (x y)
  (> (length x) (length y)))

(defun filter (fn seq)
  (remove-if (complement fn) seq))

#|
(defun group (seq n)
  (when (zerop n) 
    (error "zero length"))
  (if seq
      (let ((c (make-collector)))
        (nlet-tail rec ((seq seq))
          (cond ((zerop (length seq))
                 (collector-contents c))
                
                ((>= (length seq) n)
                 (collector-append-item c (subseq seq 0 n))
                 (rec (subseq seq n)))
                
                (t 
                 (collector-append-item c seq)
                 (collector-contents c)))
          ))
    ))
|#
(defun group (seq n)
  (when (zerop n) 
    (error "zero length"))
  (when seq
    (let ((c (make-coll)))
      (nlet-tail rec ((seq seq))
        (cond ((zerop (length seq))
               (funcall c :get-items))
              
              ((>= (length seq) n)
               (funcall c :add (subseq seq 0 n))
               (rec (subseq seq n)))
              
              (t 
               (funcall c :add seq)
               (funcall c :get-items))
              )))))

#|
(defun prune (test tree)
  (nlet rec ((tree tree)
             (c    (make-collector)))
    (cond ((null tree) (collector-contents c))
          ((consp (car tree))
           (collector-append-item c
                                  (rec (car tree) (make-collector)))
           (rec (cdr tree) c))
          (t
           (unless (funcall test (car tree))
             (collector-append-item c (car tree)))
           (rec (cdr tree) c))
          )))
|#
(defun prune (test tree)
  (nlet rec ((tree tree)
             (c    (make-coll)))
    (cond ((null tree) (funcall c :get-items))
          ((consp (car tree))
           (funcall c :add (rec (car tree) (make-coll)))
           (rec (cdr tree) c))
          (t
           (unless (funcall test (car tree))
             (funcall c :add (car tree)))
           (rec (cdr tree) c))
          )))

(defun find2 (fn lst)
  (do* ((l lst (cdr l))
        (val (funcall fn (car l))
             (funcall fn (car l))))
       (val (values (car l) val))))

(defun before (x y lst &key (test 'eql))
  (with-tail-pure-code
    (and lst
         (let ((first (car lst)))
           (cond ((funcall test y first) nil)
                 ((funcall test x first) lst)
                 (t (before x y (cdr lst) :test test))
                 )))))

(defun after (x y lst &key (test 'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test 'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn seq)
  (let ((pos (aif (position-if fn seq)
                  it
                  (length seq))))
    (values (subseq seq 0 pos)
            (subseq seq pos))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
           (max  (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setf wins obj
                  max  score))))
      (values wins max))))

(defun best (fn lst)
  (and lst
       (let ((wins (car lst)))
         (dolist (obj (cdr lst))
           (if (funcall fn obj wins)
               (setf wins obj)))
         wins)))

#|
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((c   (make-collector))
          (max (funcall fn (car lst))))
      (collector-append-item c (car lst))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setf max score)
                 (collector-discard-contents c)
                 (collector-append-item c obj))

                ((= score max)
                 (collector-append-item c obj)))
          ))
      (values (collector-contents c) max))))
|#
(defun mostn (fn lst)
  (when lst
    (let ((c   (make-coll))
          (max (funcall fn (car lst))))
      (funcall c :add (car lst))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setf max score)
                 (funcall c :discard)
                 (funcall c :add obj))

                ((= score max)
                 (funcall c :add obj)) )
          ))
      (values (funcall c :get-items) max))))

(defun drop (n seq)
  (subseq seq n))

(defun take (n seq)
  (subseq seq 0 n))

(defun split (n seq)
  (let ((hd (take n seq))
        (tl (drop n seq)))
    (values hd tl)))

(defun zip (&rest lsts)
  (apply 'mapcar (lambda (&rest args)
                    (apply 'list args))
         lsts))

(defun interleave (&rest lsts)
  (flatten (apply 'zip lsts)))

;; -----------------------------------------------------
;; Mapping

#|
(defun mapa-b (fn a b &optional (step 1))
  (let ((c (make-collector)))
    (do ((i a (+ i step)))
        ((> i b) (collector-contents c))
      (collector-append-item c (funcall fn i))
      )))
|#
(defun mapa-b (fn a b &optional (step 1))
  (loop for i from a below b by step
        collect (funcall fn i)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 (1+ n)))

#|
(defun map-> (fn start until-fn succ-fn)
  (let ((c (make-collector)))
  (do ((i start (funcall succ-fn i)))
      ((funcall until-fn i) (collector-contents c))
    (collector-append-item c (funcall fn i))
    )))
|#
(defun map-> (fn start until-fn succ-fn)
  (let ((c (make-coll)))
  (do ((i start (funcall succ-fn i)))
      ((funcall until-fn i) (funcall c :get-items))
    (funcall c :add (funcall fn i))
    )))

(defun mappend (fn &rest lsts)
  (apply 'append (apply 'mapcar fn lsts)))

#|
(defun mapcars (fn &rest lsts)
  (let ((c (make-collector)))
    (dolist (lst lsts)
      (dolist (obj lst)
        (collector-append-item c (funcall fn obj))))
    (collector-contents c)))
|#
(defun mapcars (fn &rest lsts)
  (let ((c (make-coll)))
    (dolist (lst lsts)
      (dolist (obj lst)
        (funcall c :add (funcall fn obj))))
    (funcall c :get-items)))

(defun rmapcar (fn &rest args)
  (if (some 'atom args)
      (apply fn args)
    (apply 'mapcar
           (lambda (&rest args)
             (apply 'rmapcar fn args))
           args)))

;; -------------------------------------------------------
;;
(defun readlist (&rest args)
  (values (read-from-string
           (mkstr "(" (apply 'read-line args) ")"))))

(defun prompt (&rest args)
  (apply 'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply 'prompt args)))
     (if (funcall quit in)
         (return)
       (format *query-io* "~A~%" (funcall fn in))))))

(defun reread (&rest args)
  (values (read-from-string (apply 'mkstr args))))

(defun explode (sym)
  (map 'list (compose 'intern-symbol 'string) (symbol-name sym)))

;; ------------------------------------------------------------------
#+:lispworks
(defun pickfile (message &rest rest)
  (apply 'capi:prompt-for-file message rest))

(defun get-time-string (&key (time (get-universal-time))
                             short-form
                             utc)
  (multiple-value-bind (secs mins hrs day mon year dow dst-p tz)
      (if utc
          (decode-universal-time time 0)
        (decode-universal-time time))
    (let ((wkday (aref #("Monday" "Tuesday" "Wednesday"
                                  "Thursday" "Friday" "Saturday"
                                  "Sunday")
                       dow))
          (mth (aref #("January" "February" "March"
                                 "April" "May" "June" "July"
                                 "August" "September" "October"
                                 "November" "December")
                     (1- mon)))
          (zone (if (zerop tz)
						"UTC"
						(aref (if dst-p
								#("EDT" "CDT" "MDT" "PDT")
	                        #("EST" "CST" "MST" "PST"))
    	                  (- tz 5)))))
      (format nil "~A ~A ~A ~A  ~2,'0D:~2,'0D:~2,'0D ~A"
              (if short-form
                  (subseq wkday 0 3)
                wkday)
              (if short-form
                  (subseq mth 0 3)
                mth)
              day year hrs mins secs zone))
    ))

;; ----------------------------------------------------
#|
(defun map-from-below (fn from below &optional (by 1))
  (let ((c (make-collector)))
    (do ((ix from (+ ix by)))
        ((>= ix below) (collector-contents c))
      (collector-append-item c (funcall fn ix))
      )))
|#

(defun map-from-below (fn from below &optional (by 1))
  (loop for ix from from below below by by
        collect (funcall fn ix)))

(defun map-from-to (fn from to &optional (by 1))
  (map-from-below fn from (1+ to) by))

(defun map-from-for (fn from len &optional (by 1))
  (map-from-below fn from (+ from len) by))

;; ---------------------------------------------------------------------
#|
(defun collect-where (lst sels &key (test 'identity) (key 'identity))
  (let ((c (make-collector)))
    (do ((l lst  (cdr l))
         (s sels (cdr s)))
        ((or (endp l)
             (endp s)) (collector-contents c))
      (if (funcall test (funcall key (car s)))
          (collector-append-item c (car l)))
      )))
|#
(defun collect-where (lst sels &key (test 'identity) (key 'identity))
  (let ((c (make-coll)))
    (do ((l lst  (cdr l))
         (s sels (cdr s)))
        ((or (endp l)
             (endp s)) (funcall c :get-items))
      (if (funcall test (funcall key (car s)))
          (funcall c :add (car l)))
      )))

(defun collect-where-not (lst sels &key (test 'identity) (key 'identity))
  (collect-where lst sels :test (complement test) :key key))

;; ---------------------------------------------------------------------
(defmethod where (predicate (proseq null) &key key)
  (declare (ignore predicate key))
  nil)

#|
(defmethod where (predicate (proseq cons) &key (key 'identity))
  (let ((c (make-collector)))
    (do ((l proseq (cdr l))
         (ix 0     (1+ ix)))
        ((endp l) (collector-contents c))
      (if (funcall predicate (funcall key (car l)))
          (collector-append-item c ix))
      )))
|#
(defmethod where (predicate (proseq cons) &key (key 'identity))
  (let ((c (make-coll)))
    (do ((l proseq (cdr l))
         (ix 0     (1+ ix)))
        ((endp l) (funcall c :get-items))
      (if (funcall predicate (funcall key (car l)))
          (funcall c :add ix))
      )))

#|
(defmethod where (predicate (proseq vector) &key (key 'identity))
  (let ((c (make-collector)))
    (dotimes (ix (length proseq) (collector-contents c))
      (if (funcall predicate (funcall key (aref proseq ix)))
          (collector-append-item c ix))
      )))
|#
(defmethod where (predicate (proseq vector) &key (key 'identity))
  (let ((c (make-coll)))
    (dotimes (ix (length proseq) (funcall c :get-items))
      (if (funcall predicate (funcall key (aref proseq ix)))
          (funcall c :add ix))
      )))

(defmethod where (predicate (proseq array) &key (key 'identity))
  (where predicate (make-array (array-total-size proseq)
                               :displaced-to proseq
                               :element-type (array-element-type proseq))
         :key key))


(defun where-not (predicate &rest rest)
  (apply 'where (complement predicate) rest))


(defmethod subselect (proseq (where null))
  (declare (ignore proseq))
  nil)

(defmethod subselect (proseq (where cons))
  (subselect proseq (coerce where 'vector)))

(defmethod subselect ((proseq list) (where vector))
  (subselect (coerce proseq 'vector) where))

(defmethod subselect ((proseq vector) (where vector))
  (let* ((len  (length where))
         (rslt (if (plusp len)
                   (make-array len
                               :element-type (array-element-type proseq))
                 nil)))
    (when rslt
      (dotimes (ix len rslt)
        (setf (aref rslt ix) (aref proseq (aref where ix)))))
    ))

(defmethod subselect ((proseq array) (where vector))
  (subselect (make-array (array-total-size proseq)
                         :displaced-to proseq
                         :element-type (array-element-type proseq))
             where))


(defun indgen (n)
  (let ((rslt (make-array n :element-type 'integer)))
    (dotimes (ix n rslt)
      (setf (aref rslt ix) ix))))

#| ;; already defined above
(defun collect-if (predicate proseq &rest rest)
  (apply 'remove-if-not predicate proseq rest))
|#

(defun collect-if-not (predicate proseq &rest rest)
  (apply 'remove-if predicate proseq rest))


(defun keep-if (predicate proseq &rest rest)
  (apply 'delete-if-not predicate proseq rest))

(defun keep-if-not (predicate proseq &rest rest)
  (apply 'delete-if predicate proseq rest))

;; ---------------------------------------------------------------------
;; Macros to take the pain out of passing strings to DLL's
;;
#+:lispworks
(progn
  (defconstant *null-string*
    (fli:make-pointer :address 0))
  
  (defun _with-cstring (str fn)
    (if str
	(fli:with-foreign-string (cstr nel nb) (mkstr str)
				 (declare (ignore nel nb))
				 (funcall fn cstr))
	(fli:with-coerced-pointer (p :type 'ct:uchar) *null-string*
				  (funcall fn p))))
  
  (defmacro with-cstring (binding &body body)
    `(_with-cstring ,(second binding)
		    (lambda (,(first binding))
                      ,@body)))
  
  (defmacro with-cstrings (bindings &body body)
    (if (null bindings)
	`(progn ,@body)
	`(with-cstring ,(first bindings)
	   (with-cstrings ,(cdr bindings) ,@body))))
  
  (defun actual-args (arglist)
    ;; Some args are default :constant args without a name
    ;; These should not be specified when calling the FLI function.
    ;; Just return the list of actual arguments.
    (remove-if (lambda (arg)
                 (and (consp arg)
                      (eql :constant (first arg))))
	       arglist))
  
  (defun arg-name (arg)
    ;; some args are simple identifiers that default to type :int
    ;; and others are lists that follow the name with a C-type.
    ;; Just return the name of the arg.
    (if (consp arg)
	(first arg)
	arg))
  
  (defun arg-names (arglist)
    ;; Return the list of actual argument names.
    (mapcar 'arg-name arglist))
  
  
  (defmacro ez-define-foreign-function-receiving-c-string (name entry-name args
							   &rest rest)
    (with-gensyms (rslt dll-name)
      (let ((dll-argnames (arg-names (actual-args args))))
	`(progn
	   
	   (fli:define-foreign-function (,dll-name ,entry-name)
	       ,args
	     ,@rest)
	   
	   (defun ,name ,dll-argnames
	     (let ((,rslt (,dll-name ,@dll-argnames)))
	       (unless (fli:null-pointer-p ,rslt)
		 (fli:convert-from-foreign-string ,rslt)))))
	)))
  
  (defmacro ez-define-foreign-function-sending-c-strings (name entry-name args
							  &rest rest)
    (labels
	((is-cstring-arg (arg)
	   (and (consp arg)
		(equal (rest arg) '(ct:out-cstring)))))
      
      (let* ((actuals          (actual-args args))
             (proto-list       (mapcar (lambda (arg)
                                         (if (is-cstring-arg arg)
                                             (list (gensym)
                                                   (arg-name arg))
                                           (arg-name arg)))
                                       actuals))
	     (actual-argnames  (arg-names actuals))
	     (cstring-bindings (remove-if-not 'consp proto-list))
	     (call-argnames    (arg-names proto-list))
	     (dll-name         (gensym)))
	
	`(progn
	   
	   (fli:define-foreign-function (,dll-name ,entry-name)
	       ,args ,@rest)
	   
	   (defun ,name ,actual-argnames
	     (with-cstrings ,cstring-bindings
	       ,(if (eql 'ct:in-cstring (getf rest :result-type))
		    (let ((rslt (gensym)))
		      `(let ((,rslt (,dll-name ,@call-argnames)))
			 (unless (fli:null-pointer-p ,rslt)
			   (fli:convert-from-foreign-string ,rslt))))
		    `(,dll-name ,@call-argnames))
	       )))
	)))
  
  (defmacro ez-define-foreign-function ((name entry-name) args &rest rest)
    (cond ((find '(ct:out-cstring) args :key 'cdr :test 'equal)
	   `(ez-define-foreign-function-sending-c-strings ,name ,entry-name
							  ,args
							  ,@rest))
	  ((eql 'ct:in-cstring (getf rest :result-type))
	   `(ez-define-foreign-function-receiving-c-string ,name ,entry-name
							   ,args 
							   ,@rest))
	  (t
	   `(fli:define-foreign-function (,name ,entry-name)
		,args
	      ,@rest))))
  )

;; ------------------------------------------------------------------
;;

(defmacro def-enum (&rest enums)
  "Generate a list of defconstant's beginning with zero.
If one if the elements of the list is a pair
then generate that and all following constants
beginning with the value of the second element.
This is C++ style enumerations."
  (let ((cur-ix -1)
        (base   0))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcar
          (lambda (enum)
            (if (consp enum)
                `(defconstant ,(first enum)
                   ,(progn
                      (setf cur-ix 0)
                      (setf base (second enum))))
              `(defconstant ,enum
                 (+ ,base ,(incf cur-ix)))
              ))
          enums))
    ))

;; ------------------------------------------------------------------
;; !!! Extra !!!

#|
(defmacro compose (&rest fns)
  (case (length fns)
    (0 'identity)
    (1 (car fns))
    (otherwise
     (let ((args (gensym))
           (revfns (reverse fns)))
       (labels ((revlist (a b)
                         `(funcall ,b ,a)))
         `(lambda (&rest ,args)
            ,(reduce #'revlist revfns
                     :start 1
                     :initial-value
                     `(apply ,(first revfns) ,args)))
         )))
    ))
|#
#|
(defmacro compose (&rest fns)
  (cond ((null fns) `'identity)
        ((endp (cdr fns)) (car fns))
        (t (let ((args (gensym)))
             `(lambda (&rest ,args)
                ,(second
                  (foldr (lambda (f ans)
                           `(funcall (,(first ans) ,f ,(second ans))))
                         fns `(apply ,args))))
             ))))
|#

#|
(defmacro compose (&rest fns)
  (cond ((null fns) `'identity)
        ((single fns) (car fns))
        (t (let ((args (gensym)))
             `(lambda (&rest ,args)
                ,(foldr (lambda (f ans)
                          `(funcall ,f ,ans))
                        (butlast fns) `(apply ,(last1 fns) ,args)))
             ))
        ))
|#

(defun foldl (fn init seq)
  ;; fn should be a function of (accum item)
  (reduce fn seq
          :initial-value init))

(defun foldr (fn seq init)
  ;; fn should be a function of (item accum)
  (reduce fn seq :from-end t :initial-value init))

(defun compose (&rest fns)
  (cond ((null fns)   'identity)
        ((single fns) (car fns))
        ((single (rest fns))
         (destructuring-bind (fn1 fn2) fns
           (lambda (&rest args)
             (funcall fn1 (apply fn2 args)))
           ))
        (t (let ((fn1 (last1 fns))
                 (fns (butlast fns)))
             (lambda (&rest args)
               (foldr 'funcall fns (apply fn1 args)))
             ))
        ))

#|
;; in ML these are referred to as sections
;; these actually correspond to the Dylan operators
;; secr ::= rcurry, secl ::= curry
(defun rcurry (f &rest suf-args)
  (lambda (&rest pref-args)
    (apply f (append pref-args suf-args))))

(defun curry (f &rest pref-args)
  (lambda (&rest suf-args)
    (apply f (append pref-args suf-args))))
|#

#|
;; ML versions
(defun secr (f &rest suf-args)
  (lambda (&rest pref-args)
    (apply f (append pref-args suf-args))))

(defun secl (f &rest pref-args)
  (lambda (&rest suf-args)
    (apply f (append pref-args suf-args))))

;; in ML currying and uncurrying happens with functions of 2 args
(defun curry (f)
  (lambda (a)
    (lambda (b)
      (funcall f a b))))

(defun uncurry (f)
  (lambda (a b)
    (funcall (funcall f a) b)))
|#
#|
(defmacro rcurry (f &rest args)
  (let ((x (gensym)))
    `(lambda (,x)
       (funcall ,f ,x ,@args))
    ))

(defmacro curry (f &rest args)
  (let ((x (gensym)))
    `(lambda (,x)
       (funcall ,f ,@args ,x))
    ))
|#

(defun combine (op f1 f2)
  ;; operationally combine two functions f1 and f2
  ;; under binary operation op
  (lambda (x)
    (funcall op (funcall f1 x) (funcall f2 x))))


#| |#
(defun make-coll ()
  (let ((coll nil)
        (tail nil))
    (dlambda
     (:add (item)
      (if tail
          (let ((newcons (cons item (cdr tail))))
            (setf (cdr tail) newcons
                  tail       newcons))
        (setf coll (list item)
              tail coll
              (cdr tail) coll))
      (values))

     (:push (item)
      (if coll
          (let ((newcons (cons item coll)))
            (setf coll newcons
                  (cdr tail) coll))
        (setf coll (list item)
              tail coll
              (cdr tail) coll))
      (values))

     (:discard ()
      (nilf coll tail))

     (:empty-p ()
      (null coll))

     (:get-items ()
      (let ((ans coll))
        (when coll
          (setf (cdr tail) nil)
          (nilf coll tail))
        ans))

     (:pop ()
      (when coll
        (let ((ans (car coll)))
          (if (eq coll tail)
              (nilf coll tail)
            (setf (cdr tail) (cdr coll)
                  coll       (cdr coll)))
          ans)))
     
     (:n-items ()
      (if coll
          (progn
            (setf (cdr tail) nil)
            (prog1
                (length coll)
              (setf (cdr tail) coll)))
        0))
     )))
#| |#     
     
              
;; ---------------------------------------------------
;;
(defun eqlcond-clause (var val &rest body)
  (cond ((consp val)
         (if (eql 'QUOTE (first val))
             `((eql ,var ,val) ,@body)
           `((or ,@(mapcar (lambda (v)
                             `(eql ,var ,v))
                           val))
             ,@body)
           ))
        
        ((eq val :otherwise) `(t ,@body) )
        
        (t  `((eql ,var ,val) ,@body) )))

(defmacro! eqlcond (o!var &rest clauses)
  `(cond ,@(mapcar
            (lambda (clause)
              (apply 'eqlcond-clause g!var clause))
            clauses)))

;; -------------------------------------------------------
;; Safe FLI interfaces with coercion of caller args to
;; types required by FLI interface
;;
#+:LISPWORKS
(defun coerce-fli-arg (arg)
  (destructuring-bind (name type &rest rest) arg
    (declare (ignore rest))
    (unless (eq name :constant)
      (case type
        ((:short :long :int) 
         `(,name (coerce ,name 'fixnum)))
        ((:float :single-float)
         `(,name (coerce ,name 'single-float)))
        ((:double :double-float)     
         `(,name (coerce ,name 'double-float)))
        (otherwise
         `(,name ,name))
        ))))

#+:LISPWORKS
(defmacro def-safe-fli-function ((name &rest args)
                                 user-args &rest other-args)
  (let ((cname       (intern-symbol (format nil "_~A" name)))
        (caller-args (delete :constant 
                             (mapcar 'first user-args)))
        (coercions   (delete nil 
                             (mapcar 'um:coerce-fli-arg user-args))))
    `(progn
       (fli:define-foreign-function (,cname ,@args)
           ,user-args
         ,@other-args)
       (defun ,name ,caller-args
         (let ,coercions
           (,cname ,@caller-args))))
    ))

#|
;; Example:
(def-safe-fli-function (diddly)
                       ((a :float)
                        (b :int))
                       :result-type :long)

   ==> (by macro-expansion)

(PROGN
  (FLI:DEFINE-FOREIGN-FUNCTION (_DIDDLY) ((A :FLOAT) (B :INT)) :RESULT-TYPE :LONG)
  (DEFUN DIDDLY (A B)
    (LET ((A (COERCE A 'SINGLE-FLOAT)) (B (COERCE B 'FIXNUM))) (_DIDDLY A B))))
|#

;; -----------------------------------------
;; Lazy evaluation and once-functions
;;
#|
(defclass lazy-item ()
  ((thunk  :reader   lazy-item-thunk  :initarg :thunk)
   (val    :accessor lazy-item-val)
   ))

(defun make-lazy-item (thunk)
  (make-instance 'lazy-item :thunk thunk))

(defmacro lazy (&body body)
  `(make-lazy-item (lambda () ,@body)))

(defmethod force (val)
  val)

(defmethod force ((item lazy-item))
  (values-list
   (if (slot-boundp item 'val)
       (lazy-item-val item)
     (setf (lazy-item-val item)
           (multiple-value-list
            (funcall (lazy-item-thunk item))) ))))
|#
;; ---------------------------------------------------

(defvar *unique* #())

(defun make-once-only (fn)
  (let ((ans *unique*))
    (lambda ()
      (if (eq ans *unique*)
          (setf ans (funcall fn))
        ans))))

(defmacro lazy (&body body)
  `(make-once-only (lambda () ,@body)))

(defmethod force (val)
  val)

(defmethod force ((fn function))
  (funcall fn))

;; ------------------

(defmacro deferred (&body body)
  `(lambda ()
     ,@body))

;; ------------------

#+:LISPWORKS
(editor:setup-indent "lazy"  2 2)
#+:LISPWORKS
(editor:setup-indent "deferred" 2 2)

#|
;; --------------------------------------------
;; So...
;; We can define LET through the use of a lambda function:
;;
;;  (LET ((x <expr>)) <body>)
;;
;; same as
;;
;;  (funcall (lambda (x)
;;               <body>)
;;     <expr>)
;;
;; ----------------------------------
;; Idiom:  (if x x y)  => (or x y)  ;; good one to call (either x y)
;;         (when x y)  => (and x y)
;;         (unless x y) => (and (not x) y)

(defmacro either (a b)
  `(or ,a ,b))

(defmacro both (a b)
  `(and ,a ,b))

(defmacro any (&rest args)
  `(or ,@args))

(defmacro all (&rest args)
  `(and ,@args))
|#

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun do-nothing (&rest ignore)
  ignore)

;; -------------------------------------------------------
;; finite lists of values spanning a range
;;
(defun make-range (from to &key (by (if (>= to from) 1 -1)))
  (labels
      ((iota (tst)
         (loop for ix from 0
               for v = (+ from (* ix by))
               until (funcall tst v)
               collect v)))

    (cond ((zerop by)
           (and (= from to) (list from)))
          ((and (>= to from) (plusp by))
           (iota (rcurry '>= #|ix|# to)))
          ((and (< to from) (minusp by))
           (iota (rcurry '< #|ix|# to)))
          (t nil))))

(defun range (a b &optional c)
  (if c
      (make-range a c :by (- b a))
    (make-range a b)))

;; ------------------------------------------------------------
;; once-thereafter objects

(defstruct once-thereafter
  first-time
  thereafter)

(defmethod get-value ((v once-thereafter))
  (shiftf (once-thereafter-first-time v) (once-thereafter-thereafter v)))


;; ------------------------------------------------------------
#|
;; Possibly useful idiom...
;;
;;  To make a binary function into a function that can be mapped against
;;  multiple sequences, possibly more than 2,
;;  as in (defun a+b (a b) (+ a b)) in (map 'vector 'a+b '(1 2 3) '(4 5 6) '(7 8 9))
;;  which should have the effect of (vector (reduce 'a+b '(1 4 7))
;;                                          (reduce 'a+b '(2 5 8))
;;                                          (reduce 'a+b '(3 6 9))) 
;;
;;   (um:compose (um:curry 'reduce #'<your-binary-function-here>) 'list)
|#

(defun largest-abs-value (a b)
  ;; return the argument having the largest absolute value
  (if (> (abs a) (abs b))
      a
    b))

(defun make-list-reducer (fn)
  (compose (curry 'reduce fn) 'list))

;; -----------------------------------------------------
;; Post incr and decr
 
(defmacro! post-incf (place &optional (incr 1))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar 'list vars forms)
            (,g!old-val ,access)
            (,(car var) (+ ,g!old-val ,incr)))
       ,set
       ,g!old-val)
    ))

#|
(post-incf (aref arr ix))
|#

(defmacro! post-decf (place &optional (decr 1))
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar 'list vars forms)
            (,g!old-val ,access)
            (,(car var) (- ,g!old-val ,decr)))
       ,set
       ,g!old-val)
    ))


(defmacro! with-slot-values (slots o!instance &body body)
  `(let (mapcar (lambda (pair)
                  `(,(first pair) (,(second pair) ,g!instance)))
                ,slots)
     ,@body))

(defun firsts-of (lst)
  (mapcar 'first lst))

(defun slice (seq start &optional (nel 1))
  (let* ((len (length seq))
         (actual-start (mod (if (minusp start)
                                (+ len start)
                              start)
                            len))
         (slen (min nel (- len actual-start))))
    (if (< slen nel)
	(concatenate (cond
		       ((listp   seq) 'list)
		       ((stringp seq) 'string)
		       ((vectorp seq) 'vector))
		     (subseq seq actual-start (+ actual-start slen))
			 (slice seq 0 (- nel slen)))
	(subseq seq actual-start (+ actual-start nel))
	)))

(defun left-part (seq nel)
  (slice seq 0 nel))

(defun right-part (seq nel)
  (slice seq (- nel) nel))

;; ---------------------------------------------------------------
;; Array mover

(defun move (src src-from dst dst-from nel)
  (replace dst src
           :start1 dst-from
           :start2 src-from
           :end1   (+ dst-from nel)))

;; ----------------------------------------------------------------

#-:LISPWORKS
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (labels ((iter (n nsh)
             (declare (fixnum n nsh))
             (logior n (ash n nsh))))
    (declare (inline iter))
    (1+ (iter 
         (iter
          (iter
           (iter
            (iter (1- n) -1)
            -2)
           -4)
          -8)
         -16))))

#+:LISPWORKS
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (declare (optimize (float 0)))
  (labels ((iter (n nsh)
             (declare (sys:int32 n))
             (declare (fixnum nsh))
             (sys:int32-logior n (sys:int32>> n nsh))))
    (declare (inline iter))
    (the fixnum
         (sys:int32-to-integer
          (sys:int32-1+
           (iter 
            (iter
             (iter
              (iter
               (iter (sys:int32-1- n) 1)
               2)
              4)
             8)
            16)))
         )))

(defun ceiling-log2 (n)
  (declare (fixnum n))
  (integer-length (1- n)))

#|
(defun ceiling-pwr2 (n)
  (declare (fixnum n))
  (the fixnum
       (ash 1
            (the fixnum
                 (integer-length
                  (the fixnum
                       (1- n)))))))

(defun ceiling-log2 (n)
  (declare (fixnum n))
  (logcount (1- (ceiling-pwr2 n))))

(defun floor-pwr2 (n)
  (declare (fixnum n))
  (let ((c (ceiling-pwr2 n)))
    (if (= n c)
        n
      (ash c -1))))

(defun floor-log2 (n)
  (declare (fixnum n))
  (logcount (1- (floor-pwr2 n))))
|#

(defun floor-log2 (n)
  (declare (fixnum n))
  (1- (integer-length n)))

(defun floor-pwr2 (n)
  (declare (fixnum n))
  (ash 1 (floor-log2 n)))

(defun align-pwr2 (val pwr2)
  (declare (type fixnum pwr2)
           (type integer val))
  (let ((pwr2m1 (1- pwr2)))
    (logandc2 (+ val pwr2m1) pwr2m1)))

;; -----------------------------------------------------------------------

(defun format-error (err)
  "Routine to get the readable error message from a condition object."
  (with-output-to-string (s)
    (let ((*print-escape* nil))
      (print-object err s))))

;; ----------------------------------------------
;; convenience macro

(defmacro with-slot-accessors ((varname struct-name) slot-names &body body)
  `(with-accessors ,(mapcar (lambda (slot-name)
                              `(,(if (consp slot-name)
                                     (first slot-name)
                                   slot-name)
                                ,(intern-symbol (um:mkstr struct-name #\-
                                                          (if (consp slot-name)
                                                              (second slot-name)
                                                            slot-name)))
                                ))
                            slot-names)
       ,varname
     ,@body))


;; --------------------------------------------
;; BIND*

(defmacro bind* (bindings &body body)
  (nlet iter ((bindings bindings))
    
    (labels ((invalid-syntax (bindings)
               (error "Invalid BIND* syntax: ~S" bindings))
             
             (more (bindings)
               (let ((decls (loop while (and (cdr bindings)
                                             (consp (cadr bindings))
                                             (member (caadr bindings) '(:DECLARE DECLARE)))
                                  do (pop bindings)
                                  collect
                                  `(declare ,@(cdr (first bindings)))
                                  )))
                 `(,@decls
                   ,@(if (cdr bindings)
                         `(,(iter (rest bindings)))
                       body))
                 )))

      (cond ((endp bindings) `(progn ,@body))
          
            ((symbolp (first bindings))
             `(let (,(first bindings))
                ,@(more bindings)))
          
            ((consp (first bindings))
             (let ((binding (first bindings)))
               
               (cond ((symbolp (first binding))

                      (cond ((bind*-handler (first binding))
                             ;; something we handle
                             (funcall (bind*-handler (first binding))
                                      binding
                                      bindings
                                      #'more))


                            ;; regular LET binding
                            (t (destructuring-bind (name val) (first bindings)
                                 `(let ((,name ,val))
                                    ,@(more bindings))))
                            ))
                   
                     ((consp (first binding))
                      ;; destructuring binding
                      (destructuring-bind (lst val) (first bindings)
                        `(destructuring-bind ,lst ,val
                           ,@(more bindings))))
                   
                     (t
                      (invalid-syntax bindings))
                     )))
          
            (t (invalid-syntax bindings))
            ))))

(defun bind*-handler (symbol)
  (get symbol 'bind*-handler))

(defmacro! define-bind*-handler (symbol (binding-name more-bindings-name) &body body)
  `(setf (get ,symbol 'bind*-handler)
         (lambda (,binding-name ,g!bindings ,g!more)
           (symbol-macrolet ((,more-bindings-name (funcall ,g!more ,g!bindings)))
             ,@body))))

(define-bind*-handler :ACCESSORS (binding more-bindings)
  (destructuring-bind (a-bindings obj) (rest binding)
    `(with-accessors ,(mapcar (lambda (a-binding)
                                (if (consp a-binding)
                                    a-binding
                                  (list a-binding a-binding)))
                              a-bindings)
         ,obj
       ,@more-bindings)))

(define-bind*-handler :STRUCT-ACCESSORS (binding more-bindings)
  (destructuring-bind (struct-type slot-bindings obj) (rest binding)
    `(with-slot-accessors (,obj ,struct-type) ,slot-bindings
       ,@more-bindings)))

(define-bind*-handler :SLOTS (binding more-bindings)
  (destructuring-bind (slot-bindings obj) (rest binding)
    `(with-slots ,slot-bindings ,obj
       ,@more-bindings)))
                      
(define-bind*-handler :SYMBOL-MACRO (binding more-bindings)
  (destructuring-bind (name form) (rest binding)
    `(symbol-macrolet ((,name ,form))
       ,@more-bindings)))

(define-bind*-handler :VALUES (binding more-bindings)
  (destructuring-bind (names form) (rest binding)
    `(multiple-value-bind ,names ,form
       ,@more-bindings)))
                        
#|
(bind* ((a 1)
        (b 2)
        (:values (x y z) doit)
        ((a c &key (d 5) &rest xs) doit2))
  body)
(bind* ((a 1)
        (b 2)
        (:values (x y z) (values 15 22 34 55))
        ((d e &rest xs &key (g 99) &allow-other-keys) '(101 102 :g 13 :h 88)))
  (list a b x y z d e xs g))
|#

;; --------------------------------------------------------------

(defun binsearch (low-index hi-index compare-fn)
  (declare (type fixnum low-index hi-index))
  ;; General utility binary search routine.
  ;; low-index = starting index of table, high-index is 1 beyond table's last index.
  ;; compare-fn is a user provided comparison routine of one argument, the index,
  ;; and it should return <0, =0, >0 for each index.
  ;; returns: found, ixu
  ;;
  ;; When found is true, ixu is its index location
  ;; When found is false, ixu is where it would have to be inserted for key < key[ixu]
  ;; each index. Routine stops when comparison yields 0, or when the table is exhausted.
  ;; Comparison values of <0 indicate that the index is too high,
  ;; >0 indicates it is too low.
  ;;
  (nlet-tail srch ((ixl (1- low-index))
                     (ixu hi-index))
    (declare (type fixnum ixl ixu))
    (cond ((> (- ixu ixl) 1)
           (let* ((ixm (truncate (+ ixu ixl) 2))
                  (c (funcall compare-fn ixm)))
             (declare (type fixnum ixm c))
             (cond ((= c 0) (values t ixm)) ;; found it!
                   
                   ((< c 0) (srch ixl ixm))
                   
                   (t       (srch ixm ixu))
                   )))

          (t (values nil ixu))
          )))

;; --------------------------------------

(defun hhmmss.ss (val)
  "(hhmmss.ss val) -- convert time from hhmmss.ss to sec"
  (let* ((h    (truncate val #n1_00_00))
         (mmss (- val (* h #n1_00_00)))
         (m    (truncate mmss #n1_00))
         (s    (- mmss (* #n1_00 m))))
    (values (+ s (* 60 (+ m (* 60 h))))
            h m s)))

(defun hms (val)
  "(hms val) -- convert time from hhmmss.ss to sec"
  (hhmmss.ss val))

(defun ddmmyyyy (val)
  "(ddmmyyyy val) -- convert date from ddmmyyyy to universal time"
  (let* ((d    (truncate val #n1_00_0000))
         (mmyy (- val (* d #n1_00_0000)))
         (m    (truncate mmyy #n1_0000))
         (y    (- mmyy (* m #n1_0000))))
    (values (encode-universal-time 0 0 0 d m y)
            d m y)))

(defun dmy (val)
  "(dmy val) -- convert date from ddmmyy to universal time"
  (let* ((d    (truncate val #n1_00_00))
         (mmyy (- val (* d #n1_00_00)))
         (m    (truncate mmyy #n1_00))
         (y    (+ 2000 (- mmyy (* m #n1_00)))))
    (values (encode-universal-time 0 0 0 d m y)
            d m y)))

  
(defun yyyymmdd (val)
  "(yyyymmdd val) -- convert date from yyyymmdd to universal time"
  (let* ((y     (truncate val #n1_00_00))
         (mmdd  (- val (* y #n1_00_00)))
         (m     (truncate mmdd #n1_00))
         (d     (- mmdd (* m #n1_00))))
    (values (encode-universal-time 0 0 0 d m y)
            y m d)))
    
(defun ymd (val)
  "(ymd val) -- convert date from yymmdd to universal time"
  (yyyymmdd (+ val (* 2000 #n1_00_00))))
    
;; --------------------------------------

(defun nn-to-hz (nn)
  (* 440 (expt 2 (/ (- nn 69) 12))))

(defun hz-to-nn (hz)
  (+ 69 (* 12 (log (/ hz 440) 2))))

;; --------------------------------------

(defmethod slot-names ((class structure-class))
  #+:LISPWORKS (structure:structure-class-slot-names class)
  #+:CLOZURE   (mapcar 'ccl:slot-definition-name
                       (ccl:class-slots class))
  #+:ALLEGRO   (mapcar 'clos:slot-definition-name
                       (clos:class-slots class)))

(defmethod slot-names ((class standard-class))
  #+:LISPWORKS (mapcar 'clos:slot-definition-name
                       (clos:class-slots class))
  #+:CLOZURE   (mapcar 'ccl:slot-definition-name
                       (ccl:class-slots class))
  #+:ALLEGRO   (mapcar 'clos:slot-definition-name
                       (clos:class-slots class)))

(defmethod slot-names (class)
  (error "Can't get slot names for class: ~A" class))
    
;; --------------------------------------

(defun make-struct-copy (type struct)
  ;; assumes we have a standard copy function for the struct
  (let ((copier (intern (mkstr :COPY- (symbol-name type))
                        (symbol-package type))))
    (funcall copier struct)))

(defun copy-struct (obj &rest bindings)
  ;; assumes we have a standard copy function for the struct
  (let* ((type        (type-of obj))
         (cpy         (make-struct-copy type obj)))
    (when bindings
      (let* ((class       (find-class type))
             (slots       (slot-names class))
             (pairs       (group bindings 2))
             (setf-pairs  (mapcar (lambda (pair)
                                    (destructuring-bind (name val) pair
                                      (let ((sname (find name slots
                                                         :test 'string-equal)))
                                        (if sname
                                            (list sname val)
                                          (error "no slot named: ~A for type: ~A"
                                                 name type)))))
                                  pairs)))
        (mapcar (lambda (pair)
                  (destructuring-bind (name val) pair
                    (setf (slot-value cpy name) val)))
                setf-pairs)
        ))
    cpy))

;; -----------------------------------------------------------------

(defun separate-decls-and-body (body)
  (do ((decls (and (stringp (car body)) (list (car body))) )
       (lst   (if (stringp (car body)) (cdr body) body)  (cdr lst)))
      ((not (and (consp (car lst))
                 (eql 'declare (caar lst))))
       (values (nreverse decls) lst))
    (push (car lst) decls)))

;; -----------------------------------------------------------------

(defmacro! defwrapper (name args &body body)
  ;; defines a macro that encapsulates its &body arg into a lambda and
  ;; calls a do-it routine with the lambda function. The do-it routine
  ;; contains the bulk of the the wrapper code. And the macro
  ;; generated wrapped code contains only a function call to the doit
  ;; routine.
  (let ((fn-name   (intern (symbol-name g!fn-name)))
        (body-name (intern (symbol-name g!body))))
    `(progn
       (defun ,fn-name (,@args ,g!fn)
         ,@(subst `(funcall ,g!fn ,@args) '&body body))
       (defmacro ,name ((,@args) &body ,body-name)
         `(,',fn-name ,,@args (lambda (,,@args) ,@,body-name))) )))

#|
  ;; example
(defwrapper with-wrapped-thingy (a b c)
  (let ((ans (startup-fn a)))
    (unwind-protect
        &body ;; <-- this is where a funcall will be placed
      (shutdown ans b c))))
==>
(PROGN
  (DEFUN FN-NAME7520 (A B C #1=#:FN7519)
    (LET ((ANS (STARTUP-FN A))) (UNWIND-PROTECT (FUNCALL #1# . #2=(A B C)) (SHUTDOWN ANS B C))))
  (DEFMACRO WITH-WRAPPED-THINGY (#2# &BODY BODY7521)
    `(FN-NAME7520 ,A ,B ,C (LAMBDA (,A ,B ,C) ,@BODY7521))))

(with-wrapped-thingy (x y z)
    (doit x y z))
==>
(#:FN-NAME32008 X Y Z (LAMBDA (X Y Z) (DOIT X Y Z)))
|#

;; -- end of usefull_macros.lisp -- ;;
