(defun move-recursive (amount target destination spare)
    (cond
        ((= amount 1) (list (list target destination)))
        ((> amount 1)
            (append
                (move-recursive (- amount 1) target spare destination)
                (list (list target destination)
                (move-recursive (- amount 1) spare destination target))
            ))
        ((< amount 0) (error "negative number"))
        (t '())))

(defun hanoi-recursive (amount)
    (move-recursive amount #\A #\B #\C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct snapshot
    amount
    target
    destination
    spare)

(defun move (amount target destination spare)
    (cond
        ((= amount 1) (list (list target destination)))
        ((> amount 1)
            (list
                (make-snapshot
                    :amount (- amount 1)
                    :target target
                    :destination spare
                    :spare destination)
                (list target destination)
                (make-snapshot
                    :amount (- amount 1)
                    :target spare
                    :destination destination
                    :spare target)))
        ((< amount 0) (error "negative number"))
        (t '())))

(defun snapshot-run (snapshot)
    (move
        (snapshot-amount snapshot)
        (snapshot-target snapshot)
        (snapshot-destination snapshot)
        (snapshot-spare snapshot)))

(defun eval-move (list result n)
    (cond
        ((= n 0) (list result list))
        ((null list) (list result list))
        ((typep (car list) 'snapshot)
            (eval-move (append (snapshot-run (car list)) (cdr list)) result n))
        (t  (eval-move (cdr list) (cons (car list) result) (- n 1)))))

(defun hanoi-take (amount n)
    (eval-move
        (move amount #\A #\B #\C)
        '()
        n))

(defun hanoi (amount) (hanoi-take amount 1))

;;; print 100 instructions for moving a hanoi tower iteratively until
;;; the instructions end.
(defun hanoi-print (hanoi-result)
    (labels (
        (hanoi-print_ (rest)
            (if (null rest)
                Nil
                (let ((result (eval-move rest '() 100)))
                    (print (first result))
                    (hanoi-print_ (second result))
                    Nil))))
    (print (first hanoi-result))
    (hanoi-print_ (second hanoi-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (hanoi-recursive num)
;; (hanoi 100 100)
(hanoi-print (hanoi 100))