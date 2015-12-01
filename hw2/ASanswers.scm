; is-list: predicate which returns #t only if obj is a list.

(define (is-list? obj) (if (pair? obj) (is-list? (cdr obj)) (null? obj)))

; This follows directly from the R5RS definition of a list: it is either the
; empty list (base case) or a pair whose cdr is a list (recursive call).


; Ex 2.18: take a list as input and reverse it as a new list as output

(define (my-reverse z)
  (define (append-from-end sub-list k)
    (if (< k 0)
	'()  ; Done iterating through list / default if empty list or non-list
	(cons (list-ref sub-list k) (append-from-end sub-list (- k 1)))))
  
  ; length returns 0 if not a list, so no errors
  (append-from-end z (- (length z) 1)))


; Identical filter from Lecture 3: A new list keeping elements from an inputted
; list based on a predicate tested on each element. I actually did a version of
; this function before the class went over filter, using append with one
; elementinstead of a recursive cons. After finishing Lecture 3 I cleaned it
; up. filter describes a couple of the remaining exercises perfectly.

(define (filter z pred)
  (if (null? z)
      '()    ; Done iterating, end new list
      (if (pred (car z)) ; keeping element depends on pred
	  (cons (car z) (filter (cdr z) pred))
	  (filter (cdr z) pred))))


; Ex 2.20: A list whose evenness or oddness matches the first parameter. This
; is essentially a filter operation on the variable-length list y depending on
; x. x is always in the list up front, so cons takes care of this right away.

(define (same-parity x . y)
  (define parityOfX (even? x))
  (cons x (filter y (lambda (z) (equal? parityOfX (even? z))))))


; Ex 2.21: Complete implementations of square-list. square-list example:
; (square-list (list 1 2 3 4)) --> (1 4 9 16)

; parameters of cons filled in
(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items))) ))

; parameters of map filled in
(define (square-list2 items) (map (lambda (x) (* x x)) items))


; Ex 2.23: For-each : apply a function to each element of a list.

(define (my-for-each proc seq)
  (cond ((not (null? seq))   ; cond needed here for two expressions
         (proc (car seq)) (my-for-each proc (cdr seq))) ))


; Ex 2.54: equals predicate for two lists with eqv?.

(define (my-equal? list1 list2)
  (cond ((null? list1) (null? list2))  ; end of both lists or list2 is longer
	((null? list2) #f)  ; if list2 is done but not list1, definitely false
	((eqv? (car list1) (car list2)) (my-equal? (cdr list1) (cdr list2)))
	(else #f)) )  ; eqv? test failed on a pair of elements, false


; every? implementation: checks every element of a seq meets pred's
; condition. It is possible to use filter with (not (pred)) as filter's pred
; and check for the empty list (or equivalently, keep pred and use equal? or
; my-equal? on the returned list and seq). But since every? can return #f as
; soon as a test fails, the following will be on average twice as fast (still
; O(n) though).

(define (every? pred seq)
  (cond ((null? seq) #t)   ; all elements have passed, #t
        ((pred (car seq)) (every? pred (cdr seq)))  ; element passes, onto next
        (else #f) ))   ; element failed, done

; Two lists which meet pred's condition should still meet the condition when
; appended as one list, as the elements in the new list contain the union of
; the two lists' elements, which all are given to meet the condition. If one
; element of either list fails every?, it will be present in the appended list
; and also make it fail every?, therefore every? of an appended list must be
; an and operation on every? of each of the two lists, and only that.

; Since the empty list is a list, it must be considered as well: the union of a
; list and the empty list is the first list itself. The first list solely
; decides the result of every? on the appended list, therefore the empty list
; should return #t and not conflict with this judgment. An empty list as input
; for seq immediately returns true in the above without any call to pred.


; Ex 2.59: union of two sets (unordered representation). Can't use shortcut of
; copying one of the sets if the two sets are equal since, as a list
; implementation, (equal? '(3 4) '(4 3)) will return #f.

(define (unordered-union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())  ; both empty
        ((null? set1) (cons (car set2) (cdr set2)))  ; copy of set2
        ((null? set2) (cons (car set1) (cdr set1)))  ; copy of set1
        ((not (member (car set1) set2)) ; add element to union
         (cons (car set1) (unordered-union-set (cdr set1) set2)) )
        (else (unordered-union-set (cdr set1) set2)) )) ; continue w/o adding

; After the conds taking care of empty lists, the 4th cond checks sequentially
; if a member of set1 is NOT in set2 (since member returns #f if it's not
; found, and not will only return #t if #f and only #f is the input, member
; is OK despite not being a full predicate). That element is added to the union
; set if so, otherwise not, and iteration continues with the rest of
; set1. Eventually set1 is done (empty list) and set2 is copied to be appended
; to the other elements. All of set2 unioned with the unique elements of set1
; result in the total union of set1 and set2 (and also vice versa - set1 and
; set2 could have reversed roles in this procedure and the same list would have
; resulted with a different order, which is considered abstractly an identical
; set for unordered implementation).


; Ex 2.63: union implementation given the sets are ordered.

(define (ordered-union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())  ; both empty
        ((null? set1) (cons (car set2) (cdr set2)))  ; copy of set2
        ((null? set2) (cons (car set1) (cdr set1)))  ; copy of set1
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((< x1 x2)
                      (cons x1 (ordered-union-set (cdr set1) set2)) )
                      ((< x2 x1)
                      (cons x2 (ordered-union-set set1 (cdr set2))) )
                      (else ; equal: also add element
                       (cons x1 (ordered-union-set
                                 (cdr set1) (cdr set2))) ) ))) ))

; The same initial conds apply here. Then, the first elements of both sets are
; tested. The lesser element is put as the next element of the list with the
; rest of the set and the other set passed on to continue adding. Example:
; elements from x1 will go on the union until something from x2 is greater than
; the next x1, and vice versa, etc., sorting out the correct order by itself
; based on the sub-conds. On equals (else), one and only one copy of the value
; is added to the list, while the rest of both lists are passed on (since we're
; done with the value in both lists). If the lists are of different size,
; eventually one of the null? conds will be true, which will leave the rest of
; the other list to append to the end. If the lists are the same size, the
; first cond will apply, finishing the list off with the empty list.


; remove-val - return a new list with value removed. Inversing this condition,
; this is just a filter with all elements not equal to the value kept.

(define (remove-val value oldList)
  (filter oldList (lambda (z) (not (equal? z value)))))

