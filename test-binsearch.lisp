
(defun meas-binsearch (low-index hi-index compare-fn)
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
  ;; Comparison values of <0 indicate that the index is too high, >0 indicates it is too low.
  ;;
  (um:perform search ((ixl (1- low-index))
                      (ixu hi-index)
                      (iter 1))
    (declare (type fixnum ixl ixu))
    (cond ((> (- ixu ixl) 1)
           (let* ((ixm (truncate (+ ixu ixl) 2))
                  (c (funcall compare-fn ixm)))
             (declare (type fixnum ixm c))
             (cond ((= c 0) (values iter t ixm)) ;; found it!
                   
                   ((< c 0) (search ixl ixm (1+ iter)))
                   
                   (t       (search ixm ixu (1+ iter)))
                   )))

          (t (values iter nil ixu))
          )))

(defun tst (n)
  (format t "~&Table length: ~D~%" n)
  (let ((tbl (make-array n :initial-contents (loop for ix from 0 below n collect ix)))
        (arr (make-array 20 :initial-element 0))
        (niter 1000000))
    (loop for ix from 1 to niter do
          (let ((key (lw:mt-random (* 1 n))))
            (incf (aref arr (meas-binsearch 0 n (lambda (jx) (-  key (aref tbl jx))))))))
    (plt:plot 'plt1
              (map 'vector (let ((sum 0))
                             (lambda (v)
                               (incf sum (/ v niter))))
                   (subseq arr 0 (+ 2 (position-if (complement 'zerop) arr
                                                  :from-end t))))
              :title  (format nil "Binary Search for Table Size = ~D" n)
              :xtitle "Number of Iterations"
              :ytitle "Cumulative Fraction"
              :clear t
              :thick 2
              :line-type :stepped)
    (plt:plot 'plt2
              (map 'vector (lambda (v)
                             (/ v niter))
                   (subseq arr 0 (+ 2 (position-if (complement 'zerop) arr
                                                  :from-end t))))
              :title  (format nil "Binary Search for Table Size = ~D" n)
              :xtitle "Number of Iterations"
              :ytitle "Fraction"
              :clear t
              :thick 2
              :line-type :stepped)))
