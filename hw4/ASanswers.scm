;; 1.1. altering make-account into make-account-lambda.

(define make-account-lambda
         (lambda (balance)
           (define withdraw
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds")))
           (define deposit
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
           (lambda (m)  ;; lambda last expression, function will be returned
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request --> MAKE-ACCOUNT"
                                m))))
           )
         )

;; 1.2. altering make-account-lambda into make-account-inline.

(define make-account-inline
         (lambda (balance)
           (lambda (m)  ;; lambda ONLY expression
             (cond ((eq? m 'withdraw)
                    (lambda (amount)
                      (if (>= balance amount)
                          (begin (set! balance (- balance amount))
                                 balance)
                          "Insufficient funds")))
                   ((eq? m 'deposit)
                    (lambda (amount)
                      (set! balance (+ balance amount))
                      balance))
                   (else (error "Unknown request --> MAKE-ACCOUNT"
                                m))))
             )
           )
         )

;; 1.3. factoring make-account-inline into make-account-inline-factored.

(define make-account-inline-factored
         (lambda (balance)
           (lambda (m)  ;; lambda ONLY expression
             (if (or (eq? m 'withdraw) (eq? m 'deposit))
                 (lambda (amount)
                   (cond ((eq? m 'withdraw)
                          (if (>= balance amount)
                              (begin (set! balance (- balance amount))
                                     balance)
                              "Insufficient funds"))
                         (else (set! balance (+ balance amount))
                               balance)))
                 (error "Unknown request --> MAKE-ACCOUNT"))
             )
           )
         )

;; 3. make-monitored. mf is an unnamed lambda expression within the context of
;; a count starting at 0.

(define (make-monitored f)
  (let ((count 0))
    (lambda (m) ;; function mf
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0) count)
            (else (set! count (+ count 1)) (f m)))
      )
    )
  )

;; 4. a more generic make-monitored that can take any function by using the
;; primitive function apply.

(define (better-make-monitored f)
  (let ((count 0))
    (lambda args ;; function mf with 0 or more arguments in list args
      (cond ((and (not (null? args)) (eq? (car args) 'how-many-calls?))
             count)
            ((and (not (null? args)) (eq? (car args) 'reset-count))
             (set! count 0) count)
            (else (set! count (+ count 1)) (apply f args)))
      )
    )
  )

;; 5. implement password features into a version of make-account.

(define (make-pw-account balance password)
  (let ((acc (make-account-inline balance)))
    (lambda (pw m)
      (let ((pw-equal? (eq? pw password)))
        (cond ((eq? m 'check-pw) pw-equal?)
              (pw-equal? (acc m))
              (else (error "Incorrect password")))
        )
      )
    )
  )

;; 6. implement joint account features: a new account name and password maps to
;; the same original account. The orginal account must be a make-pw-account
;; procedure.

(define (make-joint original pw-old pw-new)
  (if (original pw-old 'check-pw)
      (lambda (pw m)
        (if (eq? pw pw-new)
            (original pw-old m) ;; wrap a make-pw-account w/o any set!
            (error "Incorrect password")))
      (error "Incorrect password, no joint account created")
      )
  )

;; 7. Show that the order of evaluation of a procedure can change its result if
;; set! is used, by testing a function f such that (+ (f 0) (f 1)) varies in
;; return value from (+ (f 1) (f 0)).

(define f
  (let ((number 1))
    (lambda (n)
      (set! number (if (= n 0) 0 number))
      ;; 0 will "lock" number to 0 whether (f 0) comes first (0+0) or second
      ;; (1+0)
      number)
    )
  )

;; an explicit lambda expression, assigned to variable f (as opposed to (define
;; (f n ) ... ), is needed since the let syntax is needed to save a number
;; between calls and create the inconsistency.


