;type your name after the colon: Cole Bardin

#lang racket
(require rackunit)
(require rackunit/text-ui)

#|
CS 270
Homework 5
Professor B. Char, M. Boady,  J. Johnson, G. Long, and S. Earth

Tests given are not designed to be comprehensive.
They will give you an idea if your code is right, but they do not test all possible cases.
Think about your design.  When grading, we may add additional tests for your functions.

- make sure the entire file is accepted by DrRacket. If you don't finish some problems, comment out any
unfinished code so that it doesn't throw errors.

Submit this HW8 racketfile into Gradescope

Important Rules:
0) All function names are given, do not change the names or signatures of any functions.
1) If the instructions state something must be recursive, you will receive a zero if it is not recursive.
   Helper functions are sometimes allowed (and then the recursion could be in the helper rather than main).
2) If you think you need functions not taught in class, you are likely approaching the problem wrong
3) while you make cond cases to handle bases, it is not permitted to simply write
   cond cases for all the unit tests!
4) When you start, most unit tests will be failing. Once you implement the required functions, the unit
   tests associated with those functions should pass. Do not modify the tests unless indicated.


Each of the below questions has two parts.
First, you will be asked to write a Racket function to solve a problem (except in the very first problem,
in which the implementation will be provided for you). 
Secondly, you will be asked to prove by induction with equational reaasoning that your Racket
code is correct by exhibiting the stated property.
IMPORTANT:  since we haven't learned how to do equational reasoning with advanced racket functions (such as
append, length, foldr, map, cond etc), you should not use those in your implementations.
Also, if you think you need helpers or nested-ifs, then you are probably making the code (and hence the proof)
more complicated than necessary.  All functions in this homework should be recursive.
|#

; Question 1 [10 pts total]:
; input contract = L is a list of Boolean constants
; output contract = (myst L) is equivalent to the proposition stating "L has an even number of false booleans"
 
(define/contract (myst L)
     (-> (listof boolean?) boolean?)
     (if (null? L) #t (equal? (first L) (myst (rest L)))))
;end

#| part 1a [2 pts]: list below all contractually legal inputs L such that
                    (or (null? (rest (rest L))) (null? (rest (rest (rest L)))))

  Expression above stating that L must have 2 or 3 elements.
  Only valid inputs with those conditions:
  '(#t #t), '(#t #f), '(#f #t), '(#f #f)
  '(#t #t #t), '(#t #t #f), '(#t #f #t), '(#t #f #f), '(#f #t #t), '(#f #t #f), '(#f #f #t), '(#f #f #f)
  
  part 1b [1 pts]: evaluate the function myst on all of inputs in the previous part and record the results below by
  replacing all the nulls in the unit tests below with your answers to part 1a, and replace all the 0s with
  the actual result.  (if done correctly, you should end up with exactly twelve distinct unit tests)
|#
(display "Question 1 myst Tests\n")
(define-test-suite test_myst
  (test-equal? "" (myst '(#t #t)) #t)
  (test-equal? "" (myst '(#t #f)) #f)
  (test-equal? "" (myst '(#f #t)) #f)
  (test-equal? "" (myst '(#f #f)) #t)
  (test-equal? "" (myst '(#t #t #t)) #t)
  (test-equal? "" (myst '(#t #t #f)) #f)
  (test-equal? "" (myst '(#t #f #t)) #f)
  (test-equal? "" (myst '(#t #f #f)) #t)
  (test-equal? "" (myst '(#f #t #t)) #f)
  (test-equal? "" (myst '(#f #t #f)) #t)
  (test-equal? "" (myst '(#f #f #t)) #t)
  (test-equal? "" (myst '(#f #f #f)) #f))
(define q1b_score (- 12 (run-tests test_myst 'verbose)))

#| part 1c [2 pts]: Fill in the blank in the output contract by conjecturing a result based on your examples.  
  Note that your stated contract should hold for *all* possible legal inputs L (make more tests if need be).
  your answer here must be correct in order to get any credit for the next part, so you may wish to check your
  conjecture with a TA or the instructor.  Hint: what do all the #t results have in common vs the #f results?

ok

 part 1d [5 pts]: Prove the correctness of the provided implementation.
 In other words, use structural induction to prove the output contract is always met.
 Hint: there will end up being four different cases you need to consider. (Take a peek ahead at Question 3b if
 you are not certain the best way to structure this argument)

Proving that (myst L) returns true if L has an even number of #f elements
Base cases:
Even amt of #f:
(myst '(#t)) premise
(if (null? '(#t)) #t (equal? (first '(#t)) (myst (rest '(#t))))) apply def of myst
(if #f #t (equal? (first '(#t)) (myst (rest '(#t))))) eval null?
(equal? (first '(#t)) (myst (rest '(#t)))) eval if-statement
(equal? #t (myst '())) eval first and rest
(equal? #t (if (null? '()) #t (equal? (first '()) (myst (rest '()))))) apply def of myst
(equal? #t (if #t #t (equal? (first '()) (myst (rest '()))))) eval null?
(equal? #t #t) eval if-statement
#t eval equal?

Odd amt of #f:
(myst '(#t #f)) premise
(if (null? '(#t #f)) #t (equal? (first '(#t #f)) (myst (rest '(#t #f))))) apply def of myst
(if #f #t (equal? (first '(#t #f)) (myst (rest '(#t #f))))) eval null?
(equal? (first '(#t #f)) (myst (rest '(#t #f)))) eval if-statement
(equal? #t (myst '(#f))) eval first and rest
(equal? #t (if (null? '(#f)) #t (equal? (first '(#f)) (myst (rest '(#f)))))) apply def of myst
(equal? #t (if #f #t (equal? (first '(#f)) (myst (rest '(#f)))))) eval null?
(equal? #t (equal? (first '(#f)) (myst (rest '(#f))))) eval if-statement
(equal? #t (equal? #f (myst '()))) eval first and rest
(equal? #t (equal? #f (if (null? '()) #t (equal? (first '()) (myst (rest '())))))) apply def of myst
(equal? #t (equal? #f (if #t #t (equal? (first '()) (myst (rest '())))))) eval null?
(equal? #t (equal? #f #t)) eval if-statement
(equal? #t #f) eval equal?
#f eval equal?

Inductive Hypothesis:
let E be a list with even amt of #f and D be a list with odd amt of #f.
The following assumptions can be made:
1. (myst E) would return #t
2. (myst D) would return #f

Leap:
4 cases to consider:
1. (myst (cons #f E)) should return #f
(myst (cons #f E)) premise
(if (null? (cons #f E)) #t (equal? (first (cons #f E)) (myst (rest (cons #f E))))) apply def of myst
(if #f #t (equal? (first (cons #f E)) (myst (rest (cons #f E))))) eval null?
(equal? (first (cons #f E)) (myst (rest (cons #f E))))
(equal? #f (myst E)) eval first and rest
(equal? #f #t) invoke assumption 1
#f eval equal?


2. (myst (cons #t E)) should return #t
(myst (cons #t E)) premise
(if (null? (cons #t E)) #t (equal? (first (cons #t E)) (myst (rest (cons #t E))))) apply def of myst
(if #f #t (equal? (first (cons #t E)) (myst (rest (cons #t E))))) eval null?
(equal? (first (cons #t E)) (myst (rest (cons #t E)))) eval if-statement
(equal? #t (myst E)) eval first and rest
(equal? #t #t) invoke assumption 1
#t eval equal?

3. (myst (cons #f D)) should return #t
(myst (cons #f D)) premise
(if (null? (cons #f D)) #t (equal? (first (cons #f D)) (myst (rest (cons #f D))))) apply def of myst
(if #f #t (equal? (first (cons #f D)) (myst (rest (cons #f D))))) eval null?
(equal? (first (cons #f D)) (myst (rest (cons #f D)))) eval if-statement
(equal? #f (myst D)) eval first and rest
(equal? #f #f) invoke assumption 2
#t eval equal?

4. (myst (cons #t D)) should return #f
(myst (cons #t D)) premise
(if (null? (cons #t D)) #t (equal? (first (cons #t D)) (myst (rest (cons #t D))))) apply def of myst
(if #f #t (equal? (first (cons #t D)) (myst (rest (cons #t D))))) eval null?
(equal? (first (cons #t D)) (myst (rest (cons #t D)))) eval if-statement
(equal? #t (myst D)) eval first and rest
(equal? #t #f) invoke assumption 2
#f eval equal?

The leap has been established. t follows from structural induction that (myst L) returns true if L has an even amount of #f elements.

|#
; Question 2: (5 points)
; Write a recursive function satisifying the following specifications
; Input contract:  n is a nonnegative integer
; Output contract: (cubesum n) is the sum of the first n perfect cubes
; note: it is overkill to use the expt function here.
; Example:  (cubesum 4) would be 100 because of 1 + 8 + 27 + 64

(define (cubesum n)
  (if (zero? n) 0 (+ (* n n n) (cubesum (- n 1))))
  );Implement Me


;Test Bed
(display "Question 2 cubesum Tests\n")
(define-test-suite test_spec_sum
  (test-equal? "" (cubesum 2) 9)
  (test-equal? "" (cubesum 5) 225)
  (test-equal? "" (cubesum 7) 784)
  (test-equal? "" (cubesum 9) 2025)
  (test-equal? "" (cubesum 10) 3025))
(define q2a_score (- 5 (run-tests test_spec_sum 'verbose)))

#| Question 2b (5 points)
Prove by induction that for all nonnegative integers, (cubesum n) = (n^2)*(n+1)^2 /4
Enter your proof below:

Base case: n = 1, (cubesum 1) should equal 1
(cubesum 1) premise
(if (zero? 1) 0 (+ (* 1 1 1) (cubesum (- 1 1)))) apply def of cubesum
(if #f 0 (+ (* 1 1 1) (cubesum (- 1 1)))) eval zero?
(+ (* 1 1 1) (cubesum (- 1 1))) eval if-statement
(+ 1 (cubesum 0)) eval * and -
(+ 1 (if (zero? 0) 0 (+ (* 0 0 0) (cubesum (- 0 1))))) apply def of cubesu,
(+ 1 (if #t 0 (+ (* 0 0 0) (cubesum (- 0 1))))) eval zero?
(+ 1 0) eval if-statement
1 eval +

The base case has been established.

Inductive hypothesis:
assume that (cubesum k) = the sum of the first k'th cubes for k>0

Leap:
If the inductive hypothesis is true, (cubesum (+ k 1)) should equal the sum of the (k+1)th cubes
Alternatively, (cubesum (+ k 1)) should equal (k+1)*(k+1)*(k+1) + (cubesum k)
(cubesum (+ k 1)) premise
(if (zero? (+ k 1)) 0 (+ (* (+ k 1) (+ k 1) (+ k 1)) (cubesum (- (+ k 1) 1)))) apply def of cubesum
(if #f 0 (+ (* (+ k 1) (+ k 1) (+ k 1)) (cubesum (- (+ k 1) 1)))) eval zero?
(+ (* (+ k 1) (+ k 1) (+ k 1)) (cubesum (- (+ k 1) 1))) eval if-statement
(+ (* (+ k 1) (+ k 1) (+ k 1)) (cubesum k)) eval + -
(k+1)*(k+1)*(k+1) + (cubesum k) convert algebraically

Since (cubesum (+ k 1)) = (k+1)*(k+1)*(k+1) + (cubesum k), the leap has been established.
It follows from induction that (cubesum n) equals the sum of the first n'th cubes.

|#

; Question 3: (5 points)
; Write a recursive function satisifying the following specifications
; Input:  L is a (possibly empty) list of integers.
; Output: (evenOnes L) is a boolean value which is true iff the quantity of ones in L is an even amount
(define (evenOnes L)
  (if (null? L) #t (xor (equal? 1 (first L)) (evenOnes (rest L))))
  );Implement Me


;Test Bed
(display "Question 3 evenOnes Tests\n")
(define-test-suite test_evenOnes
  (test-equal? "" (evenOnes '(2)) #t)
  (test-equal? "" (evenOnes '(1)) #f)
  (test-equal? "" (evenOnes '(1 1)) #t)
  (test-equal? "" (evenOnes '(7 1)) #f)
  (test-equal? "" (evenOnes '(2 -1)) #t)
  (test-equal? "" (evenOnes '(1 1 2)) #t)
  (test-equal? "" (evenOnes '(4 1 2)) #f)
  (test-equal? "" (evenOnes '(2 8 1)) #f)
  (test-equal? "" (evenOnes '(1 22 1 -9)) #t)
  (test-equal? "" (evenOnes '(1 22 1 -9 1)) #f))
(define q3a_score (- 10 (run-tests test_evenOnes 'verbose)))

#|
Question 3b (5 points)
Prove by induction, algebra, and equational reasoning that
If L contains an even number of ones then (evenOnes L) = #t (i.e. "L contains an even amt of ones" is a True sentence)
If L contains an odd number of ones then  (evenOnes L) = #f (i.e. "L contains an even amt of ones" is a False sentence)
Hint for the Leap: you need 4 cases (cons 1 E), (cons x E), (cons 1 D), (cons x D)
Where, x!=1, E is a list with an even number of ones and D is a list with an odd number of ones.
Enter your proof below:

Base case:
1. L = '(2) has an even amount of 1's; (evenOnes '(2)) should return #t
(evenOnes '(2)) premise
(if (null? '(2)) #t (xor (equal? 1 (first '(2))) (evenOnes (rest '(2))))) apply def of evenOnes
(if #f #t (xor (equal? 1 (first '(2))) (evenOnes (rest '(2))))) eval null?
(xor (equal? 1 (first '(2))) (evenOnes (rest '(2)))) eval if-statement
(xor (equal? 1 2) (evenOnes '())) eval first and rest
(xor #f (evenOnes '())) eval equal?
(xor #f (if (null? '()) #t (xor (equal? 1 (first '())) (evenOnes (rest '()))))) apply def of evenOnes
(xor #f (if #t #t (xor (equal? 1 (first '())) (evenOnes (rest '()))))) eval null?
(xor #f #t) eval if-statement
#t eval xor

2. L = '(1) has an odd amount of 1's; (evenOnes '(1)) should return #t
(evenOnes '(1)) premise
(if (null? '(1)) #t (xor (equal? 1 (first '(1))) (evenOnes (rest '(1))))) apply def of evenOnes
(if #f #t (xor (equal? 1 (first '(1))) (evenOnes (rest '(1))))) eval null?
(xor (equal? 1 (first '(1))) (evenOnes (rest '(1)))) eval if-statement
(xor (equal? 1 1) (evenOnes '())) eval first and rest
(xor #t (evenOnes '())) eval equal?
(xor #t (if (null? '()) #t (xor (equal? 1 (first '())) (evenOnes (rest '()))))) apply def of evenOnes
(xor #t (if #t #t (xor (equal? 1 (first '())) (evenOnes (rest '()))))) eval null?
(xor #t #t) eval if-statement
#f eval xor

Since (evenOnes '(2)) = #t and (evenOnes '(1)) = #f, the base cases has been established

Inductive hypothesis:
Let E be a list with an even amount of 1's and D be a list with an odd amount of 1's.
The following assumptions can be made as the inductive hypothesis:
1. (evenOnes E) = #t
2. (evenOnes D) = #f

Leap:
With the assumptions of the IH, 4 cases must be considered:
1. (evenOnes (cons 1 E)) must equal #f
(evenOnes (cons 1 E)) premise
(if (null? (cons 1 E)) #t (xor (equal? 1 (first (cons 1 E))) (evenOnes (rest (cons 1 E))))) apply def of evenOnes
(if #f #t (xor (equal? 1 (first (cons 1 E))) (evenOnes (rest (cons 1 E))))) eval null?
(xor (equal? 1 1) (evenOnes E)) eval if-statement
(xor #t (evenOnes E)) eval equal?
(xor #t #t) invoke assumption 1
#f eval xor

2. (evenOnes (cons 0 E)) must equal #t
(evenOnes (cons 0 E)) premise
(if (null? (cons 0 E)) #t (xor (equal? 1 (first (cons 0 E))) (evenOnes (rest (cons 0 E))))) apply def of evenOnes
(if #f #t (xor (equal? 1 (first (cons 0 E))) (evenOnes (rest (cons 0 E))))) eval null?
(xor (equal? 1 0) (evenOnes E)) eval if-statement
(xor #f (evenOnes E)) eval equal?
(xor #f #t) invoke assumption 1
#t eval xor

3. (evenOnes (cons 1 D)) must equal #t
(evenOnes (cons 1 D)) premise
(if (null? (cons 1 D)) #t (xor (equal? 1 (first (cons 1 D))) (evenOnes (rest (cons 1 D))))) apply def of evenOnes
(if #f #t (xor (equal? 1 (first (cons 1 D))) (evenOnes (rest (cons 1 D))))) eval null?
(xor (equal? 1 1) (evenOnes D)) eval if-statement
(xor #t (evenOnes D)) eval equal?
(xor #t #f) invoke assumption 1
#t eval xor

4. (evenOnes (cons 0 D)) must equal #f
(evenOnes (cons 0 D)) premise
(if (null? (cons 0 D)) #t (xor (equal? 1 (first (cons 0 D))) (evenOnes (rest (cons 0 D))))) apply def of evenOnes
(if #f #t (xor (equal? 1 (first (cons 0 D))) (evenOnes (rest (cons 0 D))))) eval null?
(xor (equal? 1 0) (evenOnes D)) eval if-statement
(xor #f (evenOnes D)) eval equal?
(xor #f #f) invoke assumption 1
#f eval xor

Since each of the 4 cases evaluated to the desired result, the leap has been established.
It follows from induciton that (evenOnes L) = #t if L has an even amount of 1's in it and (evenOnes L) = #f if L has an odd amount of 1's in it.

|#

; Question 4: (5 Points)
; Write a recursive function satisifying the following specifications
; Input contract:  L a list (possibly empty)
; Output contract: (duplicate L) is a new list with two copies of each value consecutively in L
(define (duplicate L)
  (if (null? L) '() (cons (first L) (cons (first L) (duplicate (rest L)))))
  );Implement Me


(display "Question 4 duplicate Tests\n")
(define-test-suite test_duplicate
  (test-equal? "" (duplicate '(1)) '(1 1))
  (test-equal? "" (duplicate '(null)) '(null null))
  (test-equal? "" (duplicate '(4 6)) '(4 4 6 6))
  (test-equal? "" (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (test-equal? "" (duplicate '(1 4 5 6 4 3 4 5)) '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5)))
(define q4a_score (- 5 (run-tests test_duplicate 'verbose)))

#|
Question 4b (5 Points)
denote (length L) = n
Prove that (length (duplicate L)) = 2*n
You may use the following properties of length, as long as you properly cite them in your proof
Length Property 1: (length null) = 0 
Length Property 2: If a is any object and B is a list
(length (cons a B)) = (+ 1 (length B))
You may Justify lines by saying "by Length Property 1" or "by Length Property 2"
Enter your proof below:

Prove: (length (duplicate L)) = 2*n

Base case:
L = '(1)
n = (length L) = 1
2*n = 2
(length (duplicate L)) should equal 2
(length (duplicate '(1))) premise
(length (if (null? '(1)) '() (cons (first '(1)) (cons (first '(1)) (duplicate (rest '(1))))))) apply def of duplicate
(length (if #f '() (cons (first '(1)) (cons (first '(1)) (duplicate (rest '(1))))))) eval null?
(length (cons (first '(1)) (cons (first '(1)) (duplicate (rest '(1)))))) eval if-statement
(length (cons 1 (cons 1 (duplicate '())))) eval first and rest
(length (cons 1 (cons 1 (if (null? '()) '() (cons (first '()) (cons (first '()) (duplicate (rest '())))))))) apply def of duplicate
(length (cons 1 (cons 1 (if #t '() (cons (first '()) (cons (first '()) (duplicate (rest '())))))))) eval null?
(length (cons 1 (cons 1 '()))) eval if-statement
(+ 1 (+ 1 (length '()))) Length property 2
1 + 1 + (length '()) Convert algebraically
2 + 0 Length property 1
2 algebra

Since (length (duplicate '(1))) = 2, this establishes the base case.

Inductive Hypothesis:
for K != null:
1. (length K) = b
2. (length (duplicate K)) = 2*b

Leap:
To establish the leap, (length (duplicate (cons x K))) should equal 2*(b+1)
(length (duplicate (cons x K))) premise
(length (if (null? (cons x K)) '() (cons (first (cons x K)) (cons (first (cons x K)) (duplicate (rest (cons x K))))))) apply def of duplicate
(length (if #f '() (cons (first (cons x K)) (cons (first (cons x K)) (duplicate (rest (cons x K))))))) eval null?
(length (cons (first (cons x K)) (cons (first (cons x K)) (duplicate (rest (cons x K)))))) eval if-statement
(length (cons x (cons x (duplicate K)))) eval first and rest
(+ 1 (+ 1 (length (duplicate K))) Length property 2
(+ 1 (+ 1 2*b)) Invoke IH assumption 2
2 + 2*b Convert algebraically
2*(b+1) algebra

Since (length (duplicate (cons x K))) evaluated to 2*(b+1), this establishes the leap.

It follows from induction that (length (duplicate L)) = 2*n where n is the length of L.

|#

; Question 5: (5pts)
; Write a recursive function satisifying the following specifications
; Input contract:  L is a non-empty list
; Output contract: (dropend L) is the same as L but with last element removed
(define (dropend L)
  (if (null? (rest L)) '() (cons (first L) (dropend (rest L))))
  ); Implement Me


(display "Question 5 dropend Tests\n")
(define-test-suite test_cut_end
  (test-equal? "" (dropend '(1)) '())
  (test-equal? "" (dropend '(1 2)) '(1))
  (test-equal? "" (dropend '(3 4 (5))) '(3 4))
  (test-equal? "" (dropend '( (1) (2) (3) )) '( (1) (2) ))
  (test-equal? "" (dropend '((1 2 3 4))) '())
  (test-equal? "" (dropend '((1 2) (3 4))) '((1 2)))
  (test-equal? "" (dropend '(9 9 8)) '(9 9))
  (test-equal? "" (dropend '(/ 10 5)) '(/ 10))
  (test-equal? "" (dropend '(OR A B)) '(OR A))
  (test-equal? "" (dropend '(NOT X)) '(NOT)))
(define q5a_score (- 10 (run-tests test_cut_end 'verbose)))

#|
;Question 5b (5 pts)
;denote (length L) = n
;Prove by Induction that (length (dropend L)) = n-1
;You may use the properties of length from Question 3
Enter your proof below:

Base case: L = '(1)
(length '(1)) = n = 1
n-1 = 0
Proof:
(length (dropend '(1))) premise
(length (if (null? (rest '(1))) '() (cons (first '(1)) (dropend (rest '(1)))))) apply def of dropend
(length (if (null? '()) '() (cons 1 (dropend '())))) eval rest and first
(length (if #t '() (cons 1 (dropend '())))) eval null?
(length '()) eval if-statement
0 length property 1

Since (length (dropend '(1))) = 0, the base case has been verified.

Inductive hypothesis:
Assume that for the non empylist N:
1. (length N) = b
2. (length (dropend N)) = b - 1

Leap:
To establish the leap, (length (dropend (cons x N))) must equal (length N)
Proof:
(length (dropend (cons x N))) premise
(length (if (null? (rest (cons x N))) '() (cons (first (cons x N)) (dropend (rest (cons x N)))))) apply def of dropend
(length (if (null? N) '() (cons x (dropend N)))) eval first and rest
(length (if #f '() (cons x (dropend N)))) eval null?
(length (cons x (dropend N))) eval if-statement
1 + (length (dropend N)) length property 2
1 + b - 1 invoke inductive hypothesis assumption 2
b algebra

Since (length (dropend (cons x N))) = b, this establishes the leap.

It follows from induction that for any non-empy list L with length n, (length (dropend L)) = n - 1.

|#

; Question 6: (5pts)
; Write a recursive function satisifying the following specifications
; Input contract:  L is a list with even length
; Output contract: (multpairs L) is new list with pairs of elements multiplied together.
; Example:  (multpairs '(2 5 3 1)) would return '(10 3) since 2*5=10 and 3*1=3

(define (multpairs L)
  (if (null? L) '() (cons (* (first L) (first (rest L))) (multpairs (rest (rest L)))))
  ); Implement Me

; you couldn't be bothered to fix the typo for this unit test.

(display "Question 6 add_pairs Tests\n")
(define-test-suite test_multpairs
  (test-equal? "" (multpairs '()) '())
  (test-equal? "" (multpairs '(1 2)) '(2))
  (test-equal? "" (multpairs '(1 2 3 4)) '(2 12))
  (test-equal? "" (multpairs '(2 2 2 2)) '(4 4))
  (test-equal? "" (multpairs '(0 -1 -2 3)) '(0 -6))
  (test-equal? "" (multpairs '(1 1 1 1)) '(1 1))
  (test-equal? "" (multpairs '(1 2 3 4 5 6 7 8)) '(2 12 30 56))
  (test-equal? "" (multpairs '(9 9 9 9 9 9)) '(81 81 81))
  (test-equal? "" (multpairs '(7 3 4 6 5 5)) '(21 24 25))
  (test-equal? "" (multpairs '(-9 9 -8 8)) '(-81 -64)))
(define q6a_score (- 10 (run-tests test_multpairs 'verbose)))

#|
Question 6b (5 pts)
denote (length L) = n
Prove by Induction that (length (multpairs L)) = n/2
You may use the properties of length from Question 4
Enter your proof below:

You should say that for this proof, L has to be a list of pairs of numbers or else this proof is not possible...

Base Case: L = '(1 2).
(length '(1 2)) = 2
(length (multpairs '(1 2))) should evaluate to 1

Proof:
(length (multpairs '(1 2))) premise
(length (if (null? '(1 2)) '() (cons (* (first '(1 2)) (first (rest '(1 2)))) (multpairs (rest (rest '(1 2))))))) apply def of multpairs
(length (if #f '() (cons (* (first '(1 2)) (first (rest '(1 2)))) (multpairs (rest (rest '(1 2))))))) eval null?
(length (cons (* (first '(1 2)) (first (rest '(1 2)))) (multpairs (rest (rest '(1 2)))))) eval if-statement
(length (cons (* 1 2) (multpairs '()))) eval first and rest
(length (cons (* 1 2) (if (null? '()) '() (cons (* (first '()) (first (rest '()))) (multpairs (rest (rest '()))))))) apply def of multpairs
(length (cons (* 1 2) (if #t '() (cons (* (first '()) (first (rest '()))) (multpairs (rest (rest '()))))))) eval null?
(length (cons (* 1 2) '())) eval if-statement
(length (cons 2 '())) eval *
(length '(2)) eval cons
1 eval length

Since (length (multpairs '(1 2))) = 1 = (length '(1 2)) / 2, the base case has been verified.

Inductive Hypothesis:
For some non-empy list K with length b, it can be assumed that:
(length (multpairs K)) = b/2

Leap:
To establish the leap, (length (multpairs (cons y (cons x K)))) should evaluate to 1 + b/2
Proof:
(length (multpairs (cons y (cons x K)))) premise
(length (if (null? (cons y (cons x K))) '() (cons (* (first (cons y (cons x K))) (first (rest (cons y (cons x K))))) (multpairs (rest (rest (cons y (cons x K)))))))) apply def of multpairs
(length (if #f '() (cons (* (first (cons y (cons x K))) (first (rest (cons y (cons x K))))) (multpairs (rest (rest (cons y (cons x K)))))))) eval null?
(length (cons (* (first (cons y (cons x K))) (first (rest (cons y (cons x K))))) (multpairs (rest (rest (cons y (cons x K))))))) eval if-statement
(length (cons (* y x) (multpairs K)) eval first and rest
1 + (length (multpairs K)) length property 2
1 + b/2 invoke IH

Since (length (multpairs (cons y (cons x K)))) evaluated to 1 + b/2, the leap has been established.

It follows from induction that for any non-empty list L with length n, (length (multpairs L)) = n/2.

Question7: the stamp line
Part1A asks you to implement a function, and Part1B asks you to use induction to prove
a property about it.  You are permitted to create helper functions if you wish, and you may
also write lemmas to help with your proof.

Imagine a line of people waiting to get into a nightclub. Each time unit, the bouncers come
around and simultaneously stamp everyone's hand that is waiting on the line and also put a
new unstamped person in front of the person that was just stamped - the stamped person does
not mind, since they're guaranteed entry.  At time unit 0, a single unstamped person begins
the line.  

The goal for 7A is to write code that describes the stampline at any given time.
We shall do this by using the digit 0 to represent an unstamped person, and the digit 1 to
be a person that has the handstamp. We shall create a new list of 0s and 1s out of the old
list of 0s and 1s of the previous time unit by applying the following rewrite rules:

(i) where the old list had a 0, the new list will have a 1. This emulates an unstamped
person getting their hand stamped.
(ii) where the old list had a 1, the new list will have a 0 followed by a 1.  This represents
the stamped person staying the same (i.e. they keep their stamp), but with a new unstamped
person now in front of them.

We shall begin my making the 0th list simply be '(0) -- since this is initializing the line
with a single unstamped individual.
This means the next list will be '(1) by following rule-i
This means that after that one, the new list #2 will be '(0 1) by following rule-ii
List #3 would be (1 0 1).  The leftmost 1 in list#3 came from the 0 in list#2, and then the
0 1 after it comes from the 1 in list#2
Further examples can be seen in the unit tests. |#

; Implement the function stampLine according to the following specifications
; input contract: n is a nonnegative integer (representing the time)
; output contract: (stampLine n) is the nth list as generated from the rewrite rules described above.
; Example: (stampLine 4) would be the list (0 1 1 0 1), since the 101 from list#3 would
; turn into 1->01, 0->1, 1->01

; Helper function I wrote to do the stamping and appending 0 actions to a list
(define/contract (doStamp L)
  (-> list? list?)
  (if (null? L) '() (if (zero? (first L)) (cons 1 (doStamp (rest L))) (cons 0 (cons 1 (doStamp (rest L))))))
)
;Question 7A [10 points]: implementing stampLine
(define (stampLine n)
  (if (zero? n) '(0) (doStamp (stampLine (- n 1))))
) ; put your code here


#|
 Question 7B [10 points]: Proof
 With #7 complete, you shall now prove the following property of the stampLine function:
 for all n>=2, (stampLine n) = (append (stampLine (- n 2)) (stampLine (- n 1)))
 Example at n=4:  if you attach list#2, 01, to list#3 101, you get 01101 which is list#4
 note that you can NOT use this append property in your Question1 code since that creates
 a circular argument -- we haven't yet proven that append process always comes out to be the
 same as the rewrite rule.  Enter your proof in this empty space of the comment block.
 Recommendation: the proofs go a lot quicker if you first establish lemmas about the values of
 (stampLine 0) and (stampLine 1)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lemmas:
1. n = 0
(stampLine 0) premise
(if (zero? 0) '(0) (doStamp (stampLine (- 0 1)))) apply def of stampLine
(if #t '(0) (doStamp (stampLine (- 0 1)))) eval zero?
'(0) eval if-statement

2. n = 1
(stampLine 1) premise
(if (zero? 1) '(0) (doStamp (stampLine (- 1 1)))) apply def of stampLine
(if #f '(0) (doStamp (stampLine (- 1 1)))) eval zero?
(doStamp (stampLine (- 1 1))) eval if-statement
(doStamp (stampLine 0)) eval -
(doStamp (if (zero? 0) '(0) (doStamp (stampLine (- 0 1))))) apply def of stampLine
(doStamp (if #t '(0) (doStamp (stampLine (- 0 1))))) eval zero?
(doStamp '(0)) eval if-statement
(if (null? '(0)) '() (if (zero? (first '(0))) (cons 1 (doStamp (rest '(0)))) (cons 0 (cons 1 (doStamp (rest '(0))))))) apply def of doStamp
(if #f '() (if (zero? (first '(0))) (cons 1 (doStamp (rest '(0)))) (cons 0 (cons 1 (doStamp (rest '(0))))))) eval null?
(if (zero? (first '(0))) (cons 1 (doStamp (rest '(0)))) (cons 0 (cons 1 (doStamp (rest '(0)))))) eval if-statement
(if (zero? 0) (cons 1 (doStamp '())) (cons 0 (cons 1 (doStamp '())))) eval first and rest
(if #t (cons 1 (doStamp '())) (cons 0 (cons 1 (doStamp '())))) eval zero?
(cons 1 (doStamp '())) eval if-statement
(cons 1 (if (null? '()) '() (if (zero? (first '())) (cons 1 (doStamp (rest '()))) (cons 0 (cons 1 (doStamp (rest '()))))))) apply def of doStamp
(cons 1 (if #t '() (if (zero? (first '())) (cons 1 (doStamp (rest '()))) (cons 0 (cons 1 (doStamp (rest '()))))))) eval null?
(cons 1 '()) eval if-statement
'(1) eval cons

3. (stampLine (+ y 1)) = (doStamp (stampLine y)) for y >= 0
Base case: y = 0
RHS:
(doStamp (stampLine 0)) premise

LHS:
(stampLine (+ 0 1)) premise
(if (zero? (+ 0 1)) '(0) (doStamp (stampLine (- (+ 0 1) 1)))) apply def of stampLine
(if (zero? 1) '(0) (doStamp (stampLine 0))) eval + -
(if #f '(0) (doStamp (stampLine 0))) eval zero?
(doStamp (stampLine 0))

Since LHS = RHS, base case has been verified.

Inductive Hypothesis:
For x > 0, assume:
(stampLine (+ x 1)) = (doStamp (stampLine x))

Leap:
Prove: (stampLine (+ (+ x 1) 1))) = (doStamp (stampLine (+ x 1)))
RHS:
(doStamp (stampLine (+ x 1))) premise

LHS:
(stampLine (+ (+ x 1) 1))) premise
(stampLine (+ x 2)) eval +
(if (zero? (+ x 2)) '(0) (doStamp (stampLine (- (+ x 2) 1)))) apply def of stampLine
(if #f '(0) (doStamp (stampLine (- (+ x 2) 1)))) eval zero?
(doStamp (stampLine (- (+ x 2) 1))) eval if-statement
(doStamp (stampLine (+ x 1))) eval + -

Since RHS = LHS, this establishes the leap.

It follows from induction that for any y>=0, (stampLine (+ y 1)) = (doStamp (stampLine y))

END OF LEMMAS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Base Case:
n = 2
Prove: (stampLine 2) = (append (stampLine 0) (stampLine 1))
RHS:
(append (stampLine 0) (stampLine 1)) premise
(append '(0) '(1)) apply lemmas 1 and 2
'(0 1) eval append

LHS:
(stampLine 2) premise
(if (zero? 2) '(0) (doStamp (stampLine (- 2 1)))) appy def of stampLine
(if #f '(0) (doStamp (stampLine (- 2 1)))) eval zero?
(doStamp (stampLine (- 2 1))) eval if-statement
(doStamp (stampLine 1)) eval -
(doStamp '(1)) apply lemmas 2
(if (null? '(1)) '() (if (zero? (first '(1))) (cons 1 (doStamp (rest '(1)))) (cons 0 (cons 1 (doStamp (rest '(1))))))) apply def of doStamp
(if #f '() (if (zero? (first '(1))) (cons 1 (doStamp (rest '(1)))) (cons 0 (cons 1 (doStamp (rest '(1))))))) eval null?
(if (zero? (first '(1))) (cons 1 (doStamp (rest '(1)))) (cons 0 (cons 1 (doStamp (rest '(1)))))) eval if-statement
(if (zero? 1) (cons 1 (doStamp '()) (cons 0 (cons 1 (doStamp '()))) eval first and rest
(if #f (cons 1 (doStamp '()) (cons 0 (cons 1 (doStamp '()))) eval zero?
(cons 0 (cons 1 (doStamp '())) eval if-statement
(cons 0 (cons 1 (if (null? '()) '() (if (zero? (first '())) (cons 1 (doStamp (rest '()))) (cons 0 (cons 1 (doStamp (rest '())))))))) apply def of doStamp
(cons 0 (cons 1 (if #f '() (if (zero? (first '())) (cons 1 (doStamp (rest '()))) (cons 0 (cons 1 (doStamp (rest '())))))))) eval null?
(cons 0 (cons 1 '())) eval if-statement
'(0 1) eval cons

Since the LHS equals the RHS, the base case is verified.

Inductive Hypothesis:
For any x>=2, assume:
1. (stampLine x) = (append (stampline (- x 2)) (stampLine (- x 1)))

Leap:
Prove that (stampLine (+ x 1)) = (append (stampLine (- (+ x 1) 2)) (stampLine (- (+ x 1) 1)))

RHS:
(append (stampLine (- (+ x 1) 2)) (stampLine (- (+ x 1) 1))) premise
(append (stampLine (- x 1)) (stampLine x)) eval + -

LHS:
(stampLine (+ x 1)) premise
(if (zero? (+ x 1)) '(0) (doStamp (stampLine (- (+ x 1) 1)))) apply def of stampLine
(if (#f '(0) (doStamp (stampLine (- (+ x 1) 1)))) eval zero?
(doStamp (stampLine (- (+ x 1) 1))) eval if-statement
(doStamp (stampLine x)) eval + -
(doStamp (append (stampline (- x 2)) (stampLine (- x 1)))) invoke IH
(append (doStamp (stampline (- x 2))) (doStamp (stampLine (- x 1))))) Distribute linear doStamp func
(append (stampLine (+ (- x 2) 1)) (stampLine (+ (- x 1) 1))) apply lemmas 3
(append (stampLine (- x 1)) (stampLine x)) eval + -

Since RHS = LHS, the leap has been established.
It follows from induction that for all n>=2, (stampLine n) = (append (stampLine (- n 2)) (stampLine (- n 1))).

Question 8: Triads
There is nothing for you to implement for this question; all the code you need is provided.
A "mix" of a 3 element list is defined as replacing each member by the sum of the other two
and then adding 3 to the middle number and adding 2 to the ends.
For example, doing a "mix" of the list (10 20 30) would give us (52 43 32) since 20+30+2=52
(the 20&30 are the other values that weren't in the 10 position), and similarly the rest are
10+30+3=43, and 10+20+2=32.  This concept is implemented in the functions below  |#
(define/contract (mix a b c)
  (-> integer? integer? integer? list?)
  (cons (+ b c 2) (cons (+ a c 3) (cons (+ a b 2) null))))

; Recommendation: these proofs go much smoother if you establish lemmas about the
; first/second/third of (mix a b c)

; unmix is an inverse of sorts to mix. Note: you do NOT need to directly use this function
; in your proofs.  It is only here to make it easier to implement a certain predicate (below).
; To see the idea of how it works, since (mix 10 20 30) gives (52 43 32),
; if we do (unmix 52 43 32) it will give back (10 20 30)
(define/contract (unmix a b c)
  (-> integer? integer? integer? list?)
  (list (- (quotient (+ a b c -7) 2) a -2) (- (quotient (+ a b c -7) 2) b -3)
        (- (quotient (+ a b c -7) 2) c -2)))
#|
A Triad is a list of three positive integers with the following recursively defined property.
Triad := (8 4 9) | (13 14 15) | (mix Triad)
In plain English, (8 4 9) and (13 14 15) are automatically considered triads, and the mix of
a triad stays a triad. As an example, we can see that (62 63 64) is a triad. The reasoning is
that we know that (13 14 15) is a triad, thus when we mix it to get (31 31 29) that will be a
triad too. And if we mix that new triad, we arrive at the desired triad (62 63 64).
Observe that once the numbers get big, it's not obvious whether a triple is a valid Triad or
not -- e.g. is (294 295 300) a Triad? It turns out the answer is no, but (295 300 294) is.
It's clear that it may be helpful to have code available to quickly determine this for us.
The function provided below implements a predicate checking for Triads:
|#
(define (triad? L)
  (if (not (and (list? L) (equal? (length L) 3)
               (positive? (first L)) (positive? (second L)) (positive? (third L)))) #f
   (or (equal? L '(8 4 9)) (equal? L '(13 14 15))
          (triad? (unmix (first L) (second L) (third L))))))
          ; this last line is checking if L came from the mix of a Triad
#|
Prove or Disprove the following conjectures about any list L=(a b c) of positive integers.
Note: if you think it is true, then you must give a proof by structural induction
(note that your proof will be a combination of Equational Reasoning with racket, wrapped in
some English explanation of the induction template).

If you think it is false, then you must provide a counterexample. Note that if you claim a
specific list is or isn't a Triad, then you must actually PROVE it. Using the Racket
predicate to do so is NOT sufficient, unless you prove its correctness). To prove a list is
a Triad, you simply have to show it follows the mathematical (NOT racket) definition.
To prove a list is NOT a Triad, the best way is to use induction to prove a property that
all Triads have that your list does not have (like the OPOPOPO example from lecture).

In your proof(s) of the parts of Question8 you may use the following theorems for free:

The Commutative Property:
(+ x y) is (+ y x)
[roughly in plain English: "you can shift around the order of inputs in an addition without
changing the answer". and you can do this with any number of inputs in the sum]

The Associative Property:
(+ x (+ y z)) is (+ (+ x y) z)
[roughly in plain English: "you can shift parentheses around in an addition".
and you can do this with any number of inputs in the sum]

The Remainder Property:
(remainder (+ X Y) m) is (remainder (+ (remainder X m) (remainder Y m) m)
[roughly in plain English: "the sum of remainders is the remainder of the sum"
and you can do this with any number of inputs in the sum]


Part8A [10pts]: If L is a triad then (remainder(+(first L)(first(rest L))(first(rest(rest L)))) 7)=0
[in plain English: "the values of a Triad always sum to a multiple of 7"]
note that in future courses, the proofs may be formal and stated 100% in code, like this:
(implies (triad? L) (zero? (remainder (+ (first L)(first(rest L))(first(rest(rest L)))) 7))
But we're not going to do that in cs270

Prove: (remainder (+ (first L) (first (rest L)) (first (rest (rest L)))) 7) = 0
Base cases:
1. L = (8 4 9)
Prove: (remainder (+ (first (8 4 9)) (first (rest (8 4 9))) (first (rest (rest (8 4 9))))) 7) = 0
LHS:
(remainder (+ (first (8 4 9)) (first (rest (8 4 9))) (first (rest (rest (8 4 9))))) 7) premise
(remainder (+ 8 4 9) 7) eval first and rest
(remainder 21 7) eval +
0 eval remainder

Since the LHS = RHS, this base case has been established for L = (8 4 9)

2. L = (13 14 15)
Prove: (remainder (+ (first (13 14 15)) (first (rest (13 14 15))) (first (rest (rest (13 14 15))))) 7) = 0
LHS:
(remainder (+ (first (13 14 15)) (first (rest (13 14 15))) (first (rest (rest (13 14 15))))) 7) premise
(remainder (+ 13 14 15) 7) eval first and rest
(remainder 42 7) eval +
0 eval remainder

Since the LHS = RHS, this base case has been established for L = (13 14 15)

3. Mix triad

Inductive Hypothesis:
For some triad T = (a b c):
(remainder (+ (first T) (first (rest T)) (first (rest (rest T)))) 7) = 0
Or in other words: (remainder (+ a b c) 7) = 0

Leap:
For some triad M = (mix T),
Let M = '(x y z) and by definition of mix:
x = b + c + 2 = (+ b c 2)
y = a + c + 3 = (+ a c 3)
z = a + b + 2 = (+ a b 2)

Prove: (remainder (+ (first '(x y z)) (first (rest '(x y z))) (first (rest (rest '(x y z))))) 7) = 0
LHS:
(remainder (+ (first '(x y z)) (first (rest '(x y z))) (first (rest (rest '(x y z))))) 7) premise
(remainder (+ x y z) 7) eval first and rest
(remainder (+ (+ b c 2) (+ a c 3) (+ a b 2)) 7) plug in values of x, y, & z
(remainder (+ b c 2 a c 3 a b 2) 7) associative property
(remainder (+ b c a c a b 7) 7) communitive property
(remainder (+ (+ a b c) (+ a b c) 7)) 7) associative property
(+ (remainder (+ a b c) 7) (remainder (+ a b c) 7) (remainder 7 7)) remainder property
(+ 0 0 (remainder 7 7)) invoke IH
(+ 0 0 0) eval remainder
0 eval +

Since the LHS evaluated to the RHS, the leap has been established.

It follows from induction that for any triad L, (remainder (+ (first L) (first (rest L)) (first (rest (rest L)))) 7) = 0

Part8B [10pts]: If (remainder(+(first L)(first(rest L))(first(rest(rest L)))) 7)=0, then L is a triad.
[in plain English: "if three numbers sum to a multiple of 7, then they form a Triad"]

This is not provable and therefore is false.
Proof by counter example:

Assume if (remainder (+ (first L) (first (rest L)) (first (rest (rest L)))) 7) = 0, then L is a triad.

For N = '(1 2 4), prove: (remainder (+ (first N) (first (rest N)) (first (rest (rest N)))) 7) = 0
LHS:
(remainder (+ (first '(1 2 4)) (first (rest '(1 2 4))) (first (rest (rest '(1 2 4))))) 7) premise
(remainder (+ 1 2 4) 7) eval first and rest
(remainder 7 7) eval +
0 eval remainder

Since LHS = RHS, this makes the statement (remainder (+ (first N) (first (rest N)) (first (rest (rest N)))) 7) = 0 true.

From the assumption that for any L, if (remainder (+ (first L) (first (rest L)) (first (rest (rest L)))) 7) = 0, then L is a triad, this would prove that N is a triad.

However, L != (8 4 9) & L != (13 14 15) & L != (mix Triad), there is a contradiciton that follows from the assumption.
Therefore, the assumption is false by proof of counter example.

|#
(display "Question 7 stampLine: 10 tests\n")
(define-test-suite test_stampLine
  (test-equal? "" (stampLine 0) '(0))
  (test-equal? "" (stampLine 1) '(1))
  (test-equal? "" (stampLine 2) '(0 1))
  (test-equal? "" (stampLine 3) '(1 0 1))
  (test-equal? "" (stampLine 4) '(0 1 1 0 1))
  (test-equal? "" (stampLine 5) '(1 0 1 0 1 1 0 1))
  (test-equal? "" (stampLine 6) '(0 1 1 0 1 1 0 1 0 1 1 0 1))
  (test-equal? "" (stampLine 7) '(1 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1))
  (test-equal? "" (stampLine 8) '(0 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1))
  (test-equal? "" (stampLine 9) '(1 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1)))
(define q7a_score (- 10 (run-tests test_stampLine 'verbose)))

(display "------Unit Test Summary------\n")
(display "Q1 Passed ")
(display q1b_score)
(display "/12\n")
(display "Q2 Passed ")
(display q2a_score)
(display "/5\n")
(display "Q3 Passed ")
(display q3a_score)
(display "/10\n")
(display "Q4 Passed ")
(display q4a_score)
(display "/5\n")
(display "Q5 Passed ")
(display q5a_score)
(display "/10\n")
(display "Q6 Passed ")
(display q6a_score)
(display "/10\n")
(display "Q7A Passed ")
(display q7a_score)
(display "/10\n")
