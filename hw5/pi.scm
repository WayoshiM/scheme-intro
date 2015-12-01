;; Header file for pi.scm

;;; file: pi_header.scm
;;;
;;; This should be included (textually) at the top of pi.scm.  All
;;; these definitions are from the textbook.

;;; cons-stream is already defined (by a macro, as a special form) in
;;; UMB Scheme

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; display-line: same as display but also with newline.

(define (display-line string) (display string) (newline))


;; 1. print the first n elements of a stream, or the whole stream if n >=
;; a finite length for the stream.

(define (display-n stream n)
  (if (or (= n 0) (stream-null? stream))
      (newline)
      (begin (display-line (stream-car stream))
             (display-n (stream-cdr stream) (- n 1))
             )
      )
  )

;; 2. generalize stream-map for any n number of streams, applying proc to n
;; arguments, each of the first elements of each stream, each of the second
;; elements, etc., and returning a new stream of the results. (Stream analog of
;; Scheme's standard map)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))
       )
      )
  )

;; Completion above involved adding in stream-null?, cons-stream, stream-car,
;; and stream-cdr (replacing their list counterparts).

;; 3. Generate a stream of integers not divisible by 2, 3 or 5 based off of the
;; simple stream of integers.

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define notdiv-235
  (stream-filter
   (lambda (x) 
     (and
      (not (zero? (remainder x 2)))  ;; half of integers will fail right here
      (not (zero? (remainder x 3)))
      (not (zero? (remainder x 5)))
      )
     )
   integers
   )
  )

;; 4. implementation of mult-stream

(define (mult-stream m in-strm)
  (define (action a a-list strm)
    (let ((pow (expt 10 (- (length a-list) 1))))
      (let ((remain (remainder a (if (> pow 1) pow 1))))
        (cond ((stream-null? strm) (list->stream a-list))
              ;; infinite will be most common but check for finite anyways
              ((and (not (null? a-list)) (< (+ m remain) pow))  ;; produce
               (cons-stream
                (car a-list)
                (action remain (cdr a-list) strm)))
              (else  ;; consume
               (let ((new-a (+ (* a 10) (* m (stream-car strm)))))
                 (let ((new-a-list (string->list (number->string new-a))))
                   (action new-a
                           (add-prepended-0s
                            new-a-list
                            (- (length a-list) (length new-a-list)))
                           (stream-cdr strm))
                   )
                 )
               )
              )
        )
      )
    )
  
  ;; make new-a-list longer than a-list if necessary.
  (define (add-prepended-0s list length-diff) 
    (if (< length-diff 0)
        list
        (cons 0 (add-prepended-0s list (- length-diff 1))))
    )
  
  (action 0 '() in-strm)
  )

;; necessary helper function in the finite stream case

(define (list->stream l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (list->stream (cdr l)))
      )
  )

;; (define multtest (mult-stream 87 (list->stream '(9 8 7 4 3 6 9 1 7))))

;; 5. generate pi with an infinite stream.

(define (two-by-two-matrix a b c d)
  (lambda (m)
    (cond ((eq? m 'a) a)
          ((eq? m 'b) b)
          ((eq? m 'c) c)
          ((eq? m 'd) d)
          ((eq? m 'row1) (cons a b))
          ((eq? m 'row2) (cons c d))
          ((eq? m 'col1) (cons a c))
          ((eq? m 'col2) (cons b d))
          (else (error "Unknown element of 2x2 matrix"))
          )
    )
  )

(define (compose matrix1 matrix2)
  (define (calc-element row col)
    (+ (* (car row) (car col)) (* (cdr row) (cdr col))))
  
  (two-by-two-matrix
   (calc-element (matrix1 'row1) (matrix2 'col1))
   (calc-element (matrix1 'row1) (matrix2 'col2))
   (calc-element (matrix1 'row2) (matrix2 'col1))
   (calc-element (matrix1 'row2) (matrix2 'col2))
   )
  )

(define (add-matrices matrix1 matrix2)
  (two-by-two-matrix
   (+ (matrix1 'a) (matrix2 'a))
   (+ (matrix1 'b) (matrix2 'b))
   (+ (matrix1 'c) (matrix2 'c))
   (+ (matrix1 'd) (matrix2 'd))
   )
  )

(define f-matrix-of-1 (two-by-two-matrix 1 6 0 3))
;; f(x) - f(x-1) in matrix form
(define f-matrix-difference (two-by-two-matrix 1 4 0 2))

;; infinite stream of one element for stream-map below
(define f-matrix-difference-stream
  (cons-stream f-matrix-difference f-matrix-difference-stream))

;; matrices for f for k >= 1
(define f-matrix-stream
  (cons-stream
   f-matrix-of-1
   (stream-map add-matrices f-matrix-difference-stream f-matrix-stream))) 

;; apply a linear fractional transformation ((ax+b)/(cx+d)) to x. For this
;; application only the floor of the value is needed, so the efficiency of
;; integer values is used with the quotient procedure.

(define (apply-lft-floor matrix x)
  (quotient (+ (* (matrix 'a) x) (matrix 'b))
            (+ (* (matrix 'c) x) (matrix 'd))))

(define (pi)
  (define (action a strm)
    (let ((floor-3 (apply-lft-floor a 3)) (floor-4 (apply-lft-floor a 4)))
      (if (= floor-3 floor-4)
          (cons-stream  ;; produce
           floor-3  ;; or floor-4, equal
           (action
            (compose
             (two-by-two-matrix 10 (* -10 floor-3) 0 1)  ;; or floor-4, equal
             a)
            strm)
           )
          (action (compose a (stream-car strm)) (stream-cdr strm))  ;; consume
          )
      )
    )
  
  ;; start with f for k = 1, pass k >=2 along
  (action (stream-car f-matrix-stream) (stream-cdr f-matrix-stream))
  )

