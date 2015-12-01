;; make a unit based on its representation in source: (base-unit
;; power). (returns function to get these two things)

(define (make-unit repre)
  (lambda (x)
    (cond ((equal? x 'base) (car repre))
          ((equal? x 'power) (cadr repre))
          (else (error "Unit only has base and power")))))

;; make a quantity based on its representation in source: (factor (unit-1)
;; (unit-2)...) (returns a function to get these or one of the units)

(define (make-quantity repre)
  (lambda (x . y)   ;; only first element of list y will be used
    (cond ((equal? x 'factor) (car repre))
          ((equal? x 'unit-list) (cdr repre))
          (else (error "Quantity only has factor and unit-list")))))

;; raise a quantity to a power (returns new quantity with these proper values).

(define (raise-quantity quantity power)
  (make-quantity
   (cons (expt (quantity 'factor) power)
         ;; keep base (car x) while changing power by multiplying it
         (map (lambda (x) (list (car x) (* (cadr x) power)))
              (quantity 'unit-list)))))

;; get a quantity from a line in the file

(define (get-quantity name)
  (let ((entry (assoc name source)))
    (make-quantity (if entry
                       (cadr entry) ;; quantity mapped to this name
                       (list 1 (list name 1)))))) ;; 1 meter, 1 sec, 1 kg, etc.

;; check two SETS of lists (for unit lists) for equality. The sub-lists must be
;; in order though (always (base-unit power)). Error checking is not too
;; rigorous.

(define (set-equal? set1 set2)
  (define (set1-subset-of-set2? s1 s2)
    (cond ((null? s1) #t)
          ((member (car s1) s2) (set1-subset-of-set2? (cdr s1) s2))
          (else #f)))
  
  ;; set1 subset of set2 and lengths are equal --> set1 = set2 
  (and (set1-subset-of-set2? set1 set2) (equal? (length set1) (length set2))))
  
;; Transform a unit-list to an equivalent quantity whose unit-list is only in
;; base units (for this project, units not defined in source: m, sec, kg).

(define (normalize derived-unit-list)
  (define (to-raw-elementary-quantities unit-list)
    (cond ((null? unit-list) '())
          (else (let ((this-unit (make-unit (car unit-list))))
                  (let ((this-power (this-unit 'power))
                        (base-quantity (get-quantity (this-unit 'base))))
                    (cons
                     (if (equal? this-power 1)
                         base-quantity
                         (raise-quantity base-quantity this-power))
                     (to-raw-elementary-quantities (cdr unit-list))))))))

  ;; product of all the factors of old quantities
  (define (new-factor quantity-list)
    (if (null? quantity-list)
        1
        (* ((car quantity-list) 'factor) (new-factor (cdr quantity-list)))))

  ;; append all base units together while combining like base units with their
  ;; powers.
  (define (trimmed-base-unit-list quantities)
    (define (collect-powers-iter m-power sec-power kg-power unit-list)
      (if (null? unit-list)
          (list (list 'm m-power) (list 'sec sec-power) (list 'kg kg-power))
          (let ((unit (make-unit (car unit-list))))
            (let ((base (unit 'base)) (power (unit 'power)))
              (collect-powers-iter
               (if (equal? base 'm) (+ m-power power) m-power)
               (if (equal? base 'sec) (+ sec-power power) sec-power)
               (if (equal? base 'kg) (+ kg-power power) kg-power)
               (cdr unit-list))))))
    
    (define (all-unit-lists-in-one-list quantity-list)
      (if (null? quantity-list)
          '()
          (append
           ((car quantity-list) 'unit-list)
           (all-unit-lists-in-one-list (cdr quantity-list)))))
    
    (collect-powers-iter 0 0 0 (all-unit-lists-in-one-list quantities)))

  (let ((raw-quantities (to-raw-elementary-quantities derived-unit-list)))
    (make-quantity
     (cons
      (new-factor raw-quantities)
      (trimmed-base-unit-list raw-quantities)))))

;; Convert a quantity to an equivalent quantity q in a different unit-list V.

(define (convert q V)
  (let ((quantity (make-quantity q)))
    (let ((a (quantity 'factor)) (U (quantity 'unit-list)))
      (let ((U-base (normalize U)) (V-base (normalize V)))
        (if (set-equal? (U-base 'unit-list) (V-base 'unit-list))
            (cons (* a (/ (U-base 'factor) (V-base 'factor))) V)
            (error "Units are not compatible"))))))

;; Given: read a file and create a list of each line.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

(define source (with-input-from-file "units.dat" read-file))

