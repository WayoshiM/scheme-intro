;;;  hw7/test.scm
;;;
;;;  Script to test student's s450.
;;;  Run this from student project directory; invoke as
;;;      scheme < test.scm
;;;
;;;  For best results, run this inside a transcript.  That is,
;;;
;;;      script aaa
;;;      scheme < test.scm
;;;      exit
;;;
;;;  Then run the file aaa through filter_scheme.
;;;
;;;  NOTE: this was a difficult assignment.  Many of these tests may
;;;  have to be run individually in order to return meaningful
;;;  results.

(begin
   (define divider "
--------------------------------------------------------
")

(load "s450_after.scm")

(install-primitive-procedure 'xdisplay (lambda (x)
					 (display x)
					 (newline)))

(install-primitive-procedure '*plus* +)
(install-primitive-procedure '*minus* -)
(install-primitive-procedure '*times* *)
(install-primitive-procedure '*over* /)
(install-primitive-procedure '*equals* =)

(display* divider "test delayed [20]:" #\newline)
) ; begin

(s450)
(begin
  (define (foo x (delayed y))
    (if (*equals* x 3)
	1
	y))
  (foo 3 (*over* 1 0))
  )
(exit)


(display* divider "test delayed with cons-stream, etc [10]:" #\newline)
(s450)
(begin
  (define (integers-from n)
    (cons-stream n (integers-from (*plus* n 1))))
  (define integers (integers-from 1))
  (define (delayed-interleave s (delayed t))
    (cons-stream (stream-car s)
		 (delayed-interleave t (stream-cdr s))))
  (define (display-nth s n)
    (cond ((*equals* n 1) (stream-car s))
	  (else (display-nth (stream-cdr s) (*minus* n 1)))))
  (define alt (delayed-interleave integers alt))
  (display-nth alt 20)
  )
(exit)



(display* divider "test dynamic [20]:" #\newline)
(s450)
(begin
  (define f (lambda(x)(lambda(y)(cons (g x) y))))
  (define g (lambda((dynamic z))(cons z 4)))
  (define h (f 2))
  (define x 1)
  (h 5)
  )
(exit)


(display* divider "test dynamic with internal define and set! [10]:" #\newline)
(s450)
(begin
  (define x 1)
  (define (foo1)
    (define a 2)
    (define (foo2)
      (define x a)
      (print))
    (foo2))
  (define (print)
    (p x))
  (define (p (dynamic x))
    (xdisplay x))
  (foo1)
  )
(exit)


(display* divider "test reference [20]:" #\newline)
(s450)
(begin
  (define f (lambda (x (reference y))
	      (xdisplay (cons x y))
	      (set! x 10)
	      (set! y 20)
	      (cons x y)))
  (define a 1)
  (define b 2)
  (f a b)
  )
a
b
(f a 2)
(exit)


(display* divider "test reference (2) [10]:" #\newline)
(s450)
(begin
  (define u 3)
  (define x 10)
  (define t 2)
  (define g (lambda ((reference x))(f x x) t))
  (define f (lambda ((reference x)(reference y))
	      (set! x 5) y))
  (f u u)
  )
(f x x)
(g t)
(exit)

(display* divider "test error return to s450 prompt (1) [5]:" #\newline)
(s450)
(1)
(exit)


(display* divider "test error return to s450 prompt (+ 3 zzz) [5]:" #\newline)
(s450)
(*plus* 3 zzz)
