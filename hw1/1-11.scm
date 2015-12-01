(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))))

(define (f-iter target-n)
  (define f-n target-n)   ;; if n < 3, have input ready to be returned
  (define f-n-1 2)
  (define f-n-2 1)
  (define f-n-3 0)
  
  (define (f-test-and-increment n)
    (cond ((> n target-n) f-n)
	  (else	(set! f-n (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3)))
		(set! f-n-3 f-n-2)    ;; update variables
		(set! f-n-2 f-n-1)    ;; if another calculation
		(set! f-n-1 f-n)      ;; is needed
		(f-test-and-increment (+ 1 n)))))
  
  (f-test-and-increment 3))

(define (test-1-11 n up-to-n)
  (cond ((< n up-to-n) (display "for n = ")
        (display n)
        (display ", recur: ")
        (display (f-recur n))
        (display ", iter: ")
        (display (f-iter n))
        (newline)
        (test-1-11 (+ 1 n) up-to-n))))

(test-1-11 0 20)

