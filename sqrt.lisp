(defun square (x) (* x x))
(defun average (x y) (/ (+ x y) 2))

;;; simple implementation that just improves the guess until the guess is close
;;; enough to the actual value
(defun simple-sqrt (x)
    (labels (
        (close-enough? (guess)
            (< (abs (- (square guess) x)) 0.0001))
        (improve (guess)
            (average guess (/ x guess)))
        (try (guess)
            (if (close-enough? guess)
                guess
                (try (improve guess))))
        )
    (try 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fixed-point (f initial-guess)
    (labels (
        (close-enough? (a b)
            (< (abs (- a b)) 0.0001))
        (try (old new)
            (if (close-enough? old new)
                    new
                    (try new (funcall f new)))))
    (try initial-guess (funcall f initial-guess))))

;;; supposeingly the function y = x / y should also work, but if we apply this
;;; function iteratively, the values would just oscillate.
;;; Therefore, if we average out the new value (x / y) and the old value y,
;;; we are essentially damping the values in each iteration
;;; until we get a close enough value.
(defun fixed-sqrt (x)
    (fixed-point (lambda (y) (average (/ x y) y)) 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; f(x) = (f(x + delta) - f(x)) / delta
(defun derivative (f)
    (let ((delta 0.00001))
        (lambda (x)
            (/ (- (funcall f (+ x delta)) (funcall f x)) delta))))

(defun newton (f guess)
    (fixed-point (lambda (y) (- y (/ (funcall f y) (funcall (derivative f) y))))
                 guess))

;;; Newton's method allow one to find the fixed point x of any function f
;;; such that f(x) = 0. This method systematically creates a damping function
;;; for any function
(defun newton-sqrt (x)
    (newton (lambda (y) (- x (square y)))
            1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simple-sqrt 13)
(fixed-sqrt 13)
(newton-sqrt 13)
(sqrt 13)