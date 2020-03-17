;;; Solution to Problem 1
(defun sum (L)  
    (if (null L)
      0
      (+ (car L) (sum (cdr L)))))

;;; Solution to Problem 2
(defun neg-nums (L)
  (if (null L)
    ()
    (if (< (car L) 0)
      (cons (car L) (neg-nums (cdr L)))
      (neg-nums (cdr L)))))

;;; Solution to Problem 3
(defun inc-list-2 (L N)
  (if (null L)
    ()
    (progn
      (cons (+ N (car L)) (inc-list-2 (cdr L) N)))))

;;; Solution to Problem 4
(defun insert (N L)
  (if (null L)
    (cons N L)
    (if (< N (car L))
      (cons N L)
      (cons (car L) (insert N (cdr L))))))

;;; Solution to Problem 5
(defun isort (L)
  (if (null L)
    ()
    (insert (car L) (isort (cdr L)))))

;;; Solution to Problem 6
(defun split-list (L)
  (if (null L)
    (list () ())
    (list (cons (car L) (cadr (split-list (cdr L)))) (car (split-list (cdr L))))))

;;; Solution to Problem 7
(defun partition (L P)
  (if (null L)
    ()
    (if (and (realp (car L)) (realp P))
      (let ((X (partition (cdr L) P)))
        (if (< (car L) P)
          (list (cons (car L) (car X)) (cadr X))
          (list (car X) (cons (car L) (cadr X))))))))

;;; Solution to Problem 8
(defun pos (E L)
  (COND ((null L) 0)
        ((EQUAL E (CAR L)) 1)
        (T (LET ((X (POS E (CDR L))))
                (if (= X 0)
                     X
                     (+ 1 X))))))

;;; Solution to Problem 9
(defun split-nums (N)
  (cond ((null N) NIL)
        ((= N 0) (list (list N) NIL))
        (T (let ((X (split-nums (- N 1))))
           (if (evenp N)
             (list (cons N (car X)) (cadr X))
             (list (car X) (cons N (cadr X))))))))

;;; Solution to Problem 10
(defun set-union (s1 s2)
  (if (null s1)
    s2
    (if (null s2)
      s1
      (let ((X (set-union s1 (cdr s2))))
        (if (member (car s2) X)
          X
          (cons (car s2) X))))))

;;; Solution to Problem 11
(defun set-remove (x s)
  (if (null s)
    s
    (if (equal x (car s))
      (set-remove x (cdr s))
      (cons (car s) (set-remove x (cdr s))))))

;;; Solution to Problem 12
(defun set-excl-union (s1 s2)
  (if (null s1)
    s2
    (if (member (car S1) (set-excl-union (cdr s1) s2))
      (set-remove (car s1) (set-excl-union (cdr s1) s2))
      (cons (car s1) (set-excl-union (cdr s1) s2)))))

;;; Solution to Problem 13
(defun singletons (E)
  (if (null E)
    ()
    (if (member (car E) (cdr E))
      (set-remove (car E) (singletons (cdr E)))
      (cons (car E) (singletons (cdr E))))))

;;; Test Cases
(print "sum test cases")
(print (sum NIL))
(print (sum '()))
(print (sum '(3 4 8)))
(terpri)
(print "neg-nums test cases")
(print (neg-nums '(-1 0 -8 2 0 8 -1 -8 2 8 4 -3 0)))
(terpri)
(print "inc-list-2 test cases")
(print (inc-list-2 '() 5))
(print (inc-list-2 '(3 2.1 1 7.9) 5))
(terpri)
(print "insert test cases")
(print (insert 8 ()))
(print (insert 4 '(0 0 1 2 4)))
(print (insert 4 '(0 0 1 3 3 7 8 8)))
(terpri)
(print "isort test cases")
(print (isort '(1 8 2 7 5)))
(terpri)
(print "split-list test cases")
(print (split-list ()))
(print (split-list '(A B C D 1 2 3 4 5)))
(print (split-list '(B C D 1 2 3 4 5)))
(print (split-list '(A)))
(terpri)
(print "partition test cases")
(print (partition '(7 5 3 2 1 5) 1))
(print (partition '(4 0 5 3 1 2 4 1 4) 4))
(print (partition () 9))
(terpri)
(print "pos test cases")
(print (pos 5 '(1 2 5 3 5 5 1 5)))
(print (pos 'A '(3 2 1)))
(print (pos '(3 B) '(3 B)))
(print (pos '(A B) '((K) (3 R C) A (A B) (K L L) (A B))))
(print (pos '(3 B) '((3 B))))
(terpri)
(print "split-nums test cases")
(print (split-nums 0))
(print (split-nums 7))
(print (split-nums 8))
(terpri)
(print "set-union test cases")
(print (set-union '(A B C D) '(C E F)))
(terpri)
(print "set-remove test cases")
(print (set-remove 'A '(B B B)))
(print (set-remove 'C '(C A B C)))
(terpri)
(print "set-excl-union test cases")
(print (set-excl-union '(A B C D) '(E C F G A)))
(terpri)
(print "singletons test cases")
(print (singletons ()))
(print (singletons '(G A B C B)))
(print (singletons '(H G A B C B)))
(print (singletons '(A G A B C B)))
(print (singletons '(B G A B C B)))
