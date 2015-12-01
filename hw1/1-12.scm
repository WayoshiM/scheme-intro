(define (p n e)
  (define n-1 (- n 1))
  (if (or (= e 0) (= e n))
      1
      (+ (p n-1 (- e 1)) (p n-1 e))))		   

