(defun fib-recursive (n)
  (if (< n 2)
      n
      (+ (fib-recursive (- n 1)) (fib-recursive (- n 2)))))

(defun fib (n)
    (labels (
        (fib-iteration (last current n)
            (if (= n 2)
                current
                (fib-iteration current (+ last current) (- n 1)))))
        (cond
            ((< n 0) (error "negative input"))
            ((< n 2) n)
            (t (fib-iteration 1 1 n)))))

(defun generate (from to)
    (labels (
        (_generate (from to manipulator ls)
            (if (= from to)
                (cons to ls)
                (_generate from (funcall manipulator to) manipulator (cons to ls)))))
        (if (< from to)
            (_generate from to (lambda (x) (- x 1)) '())
            (_generate from to (lambda (x) (+ x 1)) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapcar #'fib (generate 0 100))
;; will crash
;; (mapcar #'fib-recursive (generate 0 100))