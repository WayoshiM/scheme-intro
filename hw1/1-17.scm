(define (fast-mult a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))

  (cond ((or (= b 0) (= a 0)) 0)
	((= b 1) a)
	((= b -1) (- a))
	((even? b) (double (fast-mult a (halve b))))
	(else (+ a (fast-mult a (- b 1))))))
