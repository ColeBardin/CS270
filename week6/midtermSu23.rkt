;type your name after the colon: Cole Bardin

#lang racket
(require rackunit)
(require rackunit/text-ui)

#| QUESTION #10 [worth 10pts]:  deepPairs  
a deepPair is defined recursively as an integer, or a two element list
in which both members are deepPairs.
For example, 2 is a deepPair, (2 3) is a deepPair, and so is ( (2 3) ( (4 5) (6 7) ) )
Here is a predicate that detects deepPairs
|#

; Input = L is anything
; Output = (deepPair? L) returns #t iff L is a deepPair.
(define (deepPair? L)
  (or (integer? L) (and (list? L) (not (null? L)) (not (null? (rest L))) (null? (rest (rest L)))
                        (deepPair? (first L)) (deepPair? (second L)))))

; Input = L is a deepPair
; Output = (reducePairs L) is a deepPair in which every integer (no matter how deeply nested in L)
;          gets reduced by one.
; Example: (reducePairs '((5 3) ((2 6) (0 8))) would return the deepPair '((4 2) (1 5)) (-1 7))
; Requirement: there is no need to use any helper functions or length or reverse or any functions not
; learned in class. The most efficient solution uses just a single if with a base case and recursive case.
(define/contract (reducePairs L)
  (-> deepPair? deepPair?)
   (cond
     [(list? L) (list (reducePairs (first L)) (reducePairs (second L)))] ; If L is a list, reduce its first and second elements
     [else (- L 1)] ; If it is not a list, its a number, so reduce it
  )
) ; replace this line with your implementation

;end

; Some Unit Tests for Question10
(display "Question 10 Tests\n")
(define-test-suite testRed
  (test-equal? "" (reducePairs 8) 7)
  (test-equal? "" (reducePairs '((2 3) (4 5))) '((1 2) (3 4)))
  (test-equal? "" (reducePairs '(((2 3)(4 5))(6 7))) '(((1 2) (3 4)) (5 6)))
  (test-equal? "" (reducePairs '((2 3)((4 5)(6 7)))) '((1 2) ((3 4) (5 6))))
  (test-equal? "" (reducePairs '(((0 1)(2 3))((4 5)((6 7)(8 9))))) '(((-1 0) (1 2)) ((3 4) ((5 6) (7 8)))))
  (test-equal? "" (reducePairs '((((0 1)(2 3))(4 5))((6 7)(8 9)))) '((((-1 0) (1 2)) (3 4)) ((5 6) (7 8)))))
(define q10res (- 6 (run-tests testRed)))


#| QUESTION 11 [worth 10 pts]:  translating dubNums
Implement a function according to the following specifications
Input contract: N is the dubnum representation of the number n
Output contract: (dubToInt N) returns n
Example:  (dubToInt '(DP1 (D (DP1 z)))) should output 5
Note: no helper functions are necessary.
|#
(define (dubToInt N)
  (cond
    [(equal? N '(DP1 z)) 1] ; reached end of nested lists
    [(equal? (first N) 'DP1) (+ 1 (* 2 (dubToInt (second N))))] ; DP1 case: compute 1 + 2*(op of N)
    [else (* 2 (dubToInt (second N)))] ; DP case: compute 2*(op of N)
  )
) ;<delete this line with the null and replace it with the lines of code for your answer>

; some sample unit tests:
(display "Question 11 Tests\n")
(define-test-suite testDubs
  (test-equal? "" (dubToInt '(DP1 (D (DP1 z)))) 5)
  (test-equal? "" (dubToInt '(DP1 (DP1 (DP1 (DP1 z))))) 15)
  (test-equal? "" (dubToInt '(D (D (DP1 (D (DP1 z)))))) 20)
  (test-equal? "" (dubToInt '(D (DP1 (D (DP1 (DP1 z)))))) 26)
  (test-equal? "" (dubToInt '(D (D (D (D (D (DP1 z))))))) 32)
  (test-equal? "" (dubToInt '(D (D (DP1 (D (D (DP1 (DP1 z)))))))) 100))
(define q11res (- 6 (run-tests testDubs)))
(display "\nQ10: ")(display q10res )(display "/6 tests passed\n")
(display "Q11: ")(display q11res )(display "/6 tests passed\n")