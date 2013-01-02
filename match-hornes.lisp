
(in-package :useful-macros)

#|
match($a,$a).
match($a,$b) :- gensym($a),gensym($b),unify($a,$b).
match([$ha | $ta],[$hb | $tb]) :- match($ha,$hb),match($ta,$tb).
|#
(defun gsym? (x)
  (and (symbolp x)
       (null (symbol-package x))))

(defvar *env* nil)
(defvar alt #(alt))

(defun compare-tree (a b)
  ;; (format t "~&(compare-tree ~A ~A)" a b)
  (let ((*env* nil))
    (nlet outer ((a a)
                 (b b))
      (labels ((compare (a b)
                 (cond
                  ((and (consp a)
                        (eq alt (car a)))
                   (let ((env-save *env*))
                     `(,alt ,@(nlet iter ((alst (cdr a)))
                                (setf *env* env-save)
                                (if (endp alst)
                                    `(,b)
                                  (let ((ans (outer (car alst) b)))
                                    (if (and (consp ans)
                                             (eq alt (car ans)))
                                        `(,(car alst) ,@(iter (cdr alst)))
                                      `(,ans ,@(cdr alst))) )))) ))
                 
                  ((and (consp a)
                        (consp b))
                   (nlet iter ((a a)
                               (b b))
                     (if (and (cdr a) (cdr b))
                         (let ((hd (outer (car a) (car b))))
                           (if (and (consp hd)
                                    (eq alt (car hd)))
                               `(,alt ,a ,b))
                           `(,hd ,@(iter (cdr a) (cdr b))))
                      `(,(outer (car a) (car b))) )))

                  (t 
                   (if (eql-tree a b)
                       a
                     `(,alt ,a ,b)))
                  )))
        
        (if (gsym? b)
            (if-let (pair (assoc b *env*))
                (compare a (cdr pair))
              (if (gsym? a)
                  (progn
                    (aconsf *env* b a)
                    a)
                (compare a b)))
          (compare a b))
        ))))

(defun gen-tree (x)
  (cond
   ((and (consp x)
         (eq alt (car x)))
    (mapcar #'gen-tree (cdr x)))

   ((and (consp x)
         (consp (cdr x))
         (eq alt (cadr x)))
    (cons (car x) (reduce #'append (gen-tree (cdr x))
                          :from-end t)))
   
   ((consp x)
    (cons (gen-tree (car x))
          (gen-tree (cdr x))))

   (t x)
   ))
#|

(compare-tree 'a 'b)
(compare-tree 'a 'a)
(compare-tree '(a b c) '(a d c))
(reduce #'compare-tree '((a b c) (a d c) (a b d)))
(gen-tree (reduce #'compare-tree '((a b c) (a d c) (a b d))))
(flatten (gen-tree (compare-tree (compare-tree '(a b c) '(a d c)) '(a b d))))


(match msg
        ( (:a :b :c) (list a b c))
        ( (:a :d :c) (list a d c))
        ( (:a :b :d) (list a b d)))
(match msg
        ( (:a a :b :c) (list a b c))
        ( (:a b :d :c) (list a d c))
        ( (:a c :b :d) (list a b d)))
(match msg
        ( (:a :b :c) (list a b c))
        ( (:a :b :d) (list a b d))
        ( (:a :d :c) (list a d c)))
|#
