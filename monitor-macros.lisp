
(in-package :useful-macros)

;; --------------------------------------------

(defun separate-declares-and-documentation (body-list &optional decls doc)
  "Used to help define macros that define declarations that can have a
documentation string and any number of declares. Those items must be placed
in the correct spot after the opening of the declaring form (e.g., DEFUN, DEFMETHOD).
And so these elements must be stripped off the incoming macro argument representing
the &body of the defining form. See the example of DEFINE-MONITOR which follows."
  (if (null body-list)

      (if decls
          (values doc (nreverse decls) nil)
        
        (if doc
            (values nil nil (list doc))

          (values nil nil nil)))
    
    (if (and (null doc)
             (stringp (first body-list)))
        (separate-declares-and-documentation (rest body-list) decls (first body-list))
      
      (if (and (consp (first body-list))
               (eq 'declare (first (first body-list))))
          (separate-declares-and-documentation (rest body-list)
                                               (push (first body-list) decls)
                                               doc)
        
        (values doc (nreverse decls) body-list))
      )))

;; ----------------------------------------------
;; A MONITOR is a group of functions whose entry is controlled
;; by a lock, and which may affect globally held values that are
;; shared between processes.
;;
;; Guarded functions can only be entered by one process at a time.

(defmacro with-monitor (name bindings clauses &key pre-lock)
  (labels ((gen-body (def-hdr body)
             (um:bind*
                 ((:values (doc decls body-list) (separate-declares-and-documentation body)))
               
               `(,@def-hdr
                 ,@(if doc `(,doc))
                 ,@decls
                 ,@(if pre-lock `(,pre-lock))
                 (bt:with-lock-held
                    (,name)
                    ,@body-list))
                 )))
    
    `(let ,bindings
       ,@(mapcar (lambda (clause)
                   (match clause
                     
                     ((deftype name meth-comb args &rest body) :when (and meth-comb
                                                                          (symbolp meth-comb))
                      (gen-body `(,deftype ,name ,meth-comb ,args) body))
                     
                     ((deftype name args &rest body)
                      (gen-body `(,deftype ,name ,args) body))
                     ))
                 clauses))
    ))

(defmacro define-monitor (name bindings clauses &key pre-lock)
  `(#+:LISPWORKS dspec:def
		 #+:LISPWORKS (define-monitor ,name)
		 #+:ALLEGRO progn
		 #+:CLOZURE progn
		 #+:SBCL    progn
     (progn
       (defvar ,name
          (bt:make-lock))
       (with-monitor ,name ,bindings ,clauses :pre-lock ,pre-lock))))


(defmacro let-monitor (bindings clauses &key pre-lock)
  (let ((glock (gensym)))
    `(let ((,glock  (bt:make-lock)))
       (with-monitor ,glock ,bindings ,clauses :pre-lock ,pre-lock))))

;; ----------------------------------------------
#|
(defclass lock-mixin ()
  ((lock-mixin-lock :reader lock-mixin-lock
		    :initform
		     (bt:make-lock)
		    )))

(defmacro let-locking (clauses &key pre-lock)
  (labels ((gen-body (def-hdr lockable-arg body)
             (multiple-value-bind (doc decls body-list)
                 (separate-declares-and-documentation body)
               `(,@def-hdr
                 ,@(if doc `(,doc))
                 ,@decls
                 ,@(if pre-lock `(,pre-lock))
                  (bt:with-lock-held
                  ((lock-mixin-lock ,(if (consp lockable-arg) ;; as from a qualified method arg
                                         (first lockable-arg)
                                       lockable-arg)))
                  ,@body-list))
               )))
    
    `(progn
       ,@(mapcar (lambda (clause)
                   (match clause
                     
                     ((deftype name meth-comb args . body) :when (keywordp meth-comb)
                      (gen-body `(,deftype ,name ,meth-comb ,args) (first args) body))
                     
                     ((deftype name args . body)
                      (gen-body `(,deftype ,name ,args) (first args) body))
                     ))
                 clauses))
    ))
|#
;; ----------------------------------------------

#+:LISPWORKS
(progn
  (editor:setup-indent "define-monitor" 2 2 4)
  (editor:setup-indent "let-monitor" 2 2 4))

