(load "regsim.scm")

(define gcd-mac
  (make-machine
   '(t a b)
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-mac 'a 91)
(set-register-contents! gcd-mac 'b 140)
(set-breakpoint gcd-mac 'test-b 4)

(define nonsense-mac
  (make-machine
   '(a b)
   '()
   '(start
     (goto (label here1))
   here1
     (assign a (const 3))
     (goto (label there))
   here1
     (assign b (const 4))
     (goto (label there))
   there)))

(set-breakpoint nonsense-mac 'here1 1)
