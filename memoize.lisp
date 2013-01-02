;; memoize.lisp -- Make memoized functions
;;
;; DM/HMSC  04/09
;; -----------------------------------------------------------

(in-package "USEFUL-MACROS")

;; -----------------------------------------------------------

(defun memo (fn &key name (key 'first) (test 'eql))
  ;; for ad-hoc memoization of arbitrary functions
  (let ((cache (make-hash-table :test test)))
    (setf (get name 'memo) cache)
    (lambda (&rest args)
      (let ((k (funcall key args)))
        (multiple-value-bind (val found) (gethash k cache)
          (values-list
           (if found
               val
             (setf (gethash k cache) (multiple-value-list (apply fn args)))))
          )))))

(defun memoize (fn-name &key (key 'first) (test 'eql))
  (let ((fn (symbol-function fn-name)))
    (unless (get fn-name 'memoized)
      (setf (get fn-name 'memoized)   fn
            (symbol-function fn-name) (memo fn :name fn-name :key key :test test))
      )))

(defun un-memoize (fn-name)
  (let ((fn (get fn-name 'memoized)))
    (when fn
      (setf (symbol-function fn-name) fn)
      (remprop fn-name 'memoized)
      (remprop fn-name 'memo))
    ))

(defun clear-memoize (fn-name)
  (let ((tbl (get fn-name 'memo)))
    (when tbl
      (clrhash tbl))))

(defmacro defun-memo (fn args &body body)
  `(memoize
    (defun ,fn ,args . ,body)))

#|
(defun fib (n)
  #F
  (declare (fixnum n))
  (if (< n 2)
      1
    (+ (fib (the fixnum (- n 1)))
       (fib (the fixnum (- n 2))))))

(time (fib 35))
(memoize 'fib)
(time (loop repeat #N1_000_000 do (fib 35)))
|#

;; ----------------------------------------------------------

#|
(defun fact (n)
  #f
  (declare (fixnum n))
  (format t "~&compute fact(~D)" n)
  (if (<= n 1)
      1
    (* n (fact (1- n)))))
(memoize 'fact)
|#
