;; cache.lisp -- Make cached objects and functions
;;
;; DM/HMSC  04/09
;; -----------------------------------------------------------

(in-package "USEFUL-MACROS")

;; -----------------------------------------------------------

(defun cache (fn)
  (let ((cache (make-array 2))
        (ix    0))
    (um:dlambda
        (:clear ()
         (setf (aref cache 0) nil
               (aref cache 1) nil
               ix             0))

        (:set (val &rest args)
         (let ((v1 (aref cache ix)))
           (if (equalp args (car v1))
               (setf (cdr v1) (list val))
             (setf ix (- 1 ix)
                   (aref cache ix) (cons args (list val))))))
      
        (t (&rest args)
           (values-list
            (let ((v1 (aref cache ix)))
              (if (equalp args (car v1))
                  (cdr v1)
                (let* ((ixp  (- 1 ix))
                       (v2   (aref cache ixp)))
                  (setf ix ixp)
                  (if (equalp args (car v2))
                      (cdr v2)
                    (let ((ans (multiple-value-list (apply fn args))))
                      (setf (aref cache ixp) (cons args ans))
                      ans)))
                ))))
      )))

(defun cacheize (fn-name)
  (let ((fn (symbol-function fn-name)))
    (unless (get fn-name 'cacheized)
      (setf (get fn-name 'cacheized)  fn
            (symbol-function fn-name) (cache fn))
      )))

(defun un-cacheize (fn-name)
  (let ((fn (get fn-name 'cacheized)))
    (when fn
      (setf (symbol-function fn-name) fn)
      (remprop fn-name 'cacheized))
    ))
            
;; -----------------------------------------------------------

(defclass 2-way-cache ()
  ((cache   :accessor 2-way-cache      :initform (make-array 2))
   (ix      :accessor 2-way-cache-ix   :initform 0)
   (test    :accessor 2-way-cache-test :initarg :test :initform 'eql)
   ))

(defun cache-oper (obj key found-fn not-found-fn)
  (let* ((ix      (2-way-cache-ix obj))
         (cache   (2-way-cache obj))
         (v1      (aref cache ix))
         (test-fn (2-way-cache-test obj)))
    (declare (fixnum ix)
             ((array t (2)) cache))
    (if (and v1
             (funcall test-fn key (car (the cons v1))))
        (funcall found-fn v1)
      (let* ((ixp (logxor ix 1))
             (v2  (aref cache ixp)))
        (declare (fixnum ixp))
        (if (and v2
                 (funcall test-fn key (car (the cons v2))))
            (progn
              (setf (2-way-cache-ix obj) ixp)
              (funcall found-fn v2))
          (funcall not-found-fn cache ixp))
        ))))

(defmethod check-cache ((obj 2-way-cache) key)
  (cache-oper obj key
              'cdr
              'false))

(defmethod update-cache ((obj 2-way-cache) key val)
  (cache-oper obj key
              (lambda (v)
                (declare (cons v))
                (setf (cdr v) val))
              (lambda (cache ix)
                (declare (fixnum ix)
                         ((array t (2)) cache))
                (setf (2-way-cache-ix obj)  ix
                      (aref cache ix)       (cons key val)))
              ))

(defmethod clear-cache ((obj 2-way-cache))
  (let ((cache (2-way-cache obj)))
    (setf (2-way-cache-ix obj) 0)
    (fill cache nil)))

;; ---------------------------------------------------

(defclass 2-way-n-level-cache ()
  ((cache-lines :accessor cache-lines)
   (test-fn     :accessor cache-test-fn :initarg :test  :initform 'eql)
   (row-fn      :accessor cache-row-fn  :initarg :rowfn :initform 'sxhash)
  ))

(defmethod initialize-instance :after ((obj 2-way-n-level-cache)
                                       &key (nlines 16) &allow-other-keys)
  (setf (cache-lines obj) (make-array (list nlines 3)))
  (clear-cache obj))

(defun n-level-cache-oper (obj key found-fn not-found-fn)
  (let* ((test-fn (cache-test-fn obj))
         (cache   (cache-lines obj))
         (row     (rem (funcall (cache-row-fn obj) key)
                       (array-dimension cache 0)))
         (ix      (aref cache row 0))
         (v1      (aref cache row ix)))
    (if (funcall test-fn key (car v1))
        (funcall found-fn v1)
      (let* ((ixp (logxor 3 ix))
             (v2  (aref cache row ixp)))
        (if (funcall test-fn key (car v2))
            (progn
              (setf (aref cache row 0) ixp)
              (funcall found-fn v2))
          (funcall not-found-fn cache row ixp))
        ))))
      
(defmethod check-cache ((obj 2-way-n-level-cache) key)
  (n-level-cache-oper obj key
                      (lambda (v)
                        (values-list (cdr v)))
                      (constantly nil)))

(defmethod update-cache ((obj 2-way-n-level-cache) key val)
  (n-level-cache-oper obj key
                      (lambda (v)
                        (setf (cdr v) (list val)))
                      (lambda (cache row ix)
                        (setf (aref cache row 0)  ix
                              (aref cache row ix) (cons key (list val))))
                      ))

(defmethod clear-cache ((obj 2-way-n-level-cache))
  (dotimes (row (array-dimension (cache-lines obj) 0))
    (clear-cache-row obj row)))

(defmethod clear-cache-row ((obj 2-way-n-level-cache) row)
  (let ((cache (cache-lines obj)))
    (setf (aref cache row 0) 1
          (aref cache row 1) nil
          (aref cache row 2) nil)
    ))


         
;; ----------------------------------------------------------
