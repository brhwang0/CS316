;;; Solution to Problem 1 
(defun MIN-2 (A B)
  (if (and (numberp A) (numberp B))
    (min A B)
    'ERROR))

;;; Solution to Problem 2
(defun SAFE-AVG (A B)
  (if (and (numberp A) (numberp B))
    (/ (+ A B) 2)))

;;; Solution to Problem 3
(defun ODD-GT-MILLION (I)
  (if (and (integerp I) (= (mod I 2) 1) (> I 1000000))
    t))

;;; Solution to Problem 4
(defun MULTIPLE-MEMBER (A L)
  (when (and (atom A) (listp L) (member A L))
    (if (member A (remove A L :count 1))
      t
      nil)))

;;; Solution to Problem 5
(defun MONTH->INTEGER (M)
  (if (member M (list 'JANUARY 'FEBRUARY 'MARCH 'APRIL 'MAY 'JUNE 'JULY 'AUGUST 'SEPTEMBER 'OCTOBER 'NOVEMBER 'DECEMBER))
    (progn
    (+ 1 (position M (list 'JANUARY 'FEBRUARY 'MARCH 'APRIL 'MAY 'JUNE 'JULY 'AUGUST 'SEPTEMBER 'OCTOBER 'NOVEMBER 'DECEMBER))))
  'ERROR))

;;; Solution to Problem 6 
(defun SCORE->GRADE (s)
  (cond
    ((not (numberp s)) nil)
    ((>= s 90.0) 'A)
    ((>= s 87.0) 'A-)
    ((>= s 83.0) 'B+)
    ((>= s 80.0) 'B)
    ((>= s 77.0) 'B-)
    ((>= s 73.0) 'C+)
    ((>= s 70.0) 'C)
    ((>= s 60.0) 'D)
    ((< s 60.0) 'F)))

;;; Solution to Problem 7 
(defun GT (A B)
  (and (numberp A) (numberp B) (> A B)))

;;; Solution to Problem 8 
(defun SAME-PARITY (A B)
  (and (numberp A) (numberp B) (or (and (= (mod A 2) 0) (= (mod B 2) 0)) (and (= (mod A 2) 1) (= (mod B 2) 1)))))

;;; Solution to Problem 9 
(defun SAFE-DIV (A B)
  (and (numberp A) (numberp B) (not (zerop B)) (/ A B)))

;;; Test Cases
(print "MIN-2 Test Cases")
(print (MIN-2 21.3 7/2))
(print (MIN-2 17.5 29))
(print (MIN-2 5 'APPLE))
(print (MIN-2 '(31) '(54)))
(terpri)
(print "SAFE-AVG Test Cases")
(print (SAFE-AVG 23 47.2))
(print (SAFE-AVG 3 8))
(print (SAFE-AVG '(23.1) 47.3))
(print (SAFE-AVG 'ORANGE 'PLUM))
(terpri)
(print "ODD-GT-MILLION Test Cases")
(print (ODD-GT-MILLION 92010231))
(print (ODD-GT-MILLION 17))
(print (ODD-GT-MILLION 92010232))
(print (ODD-GT-MILLION 21/5))
(print (ODD-GT-MILLION 1718671.24))
(print (ODD-GT-MILLION '(2010231)))
(print (ODD-GT-MILLION 'APPLE))
(terpri)
(print "MULTIPLE-MEMBER Test Cases")
(print (MULTIPLE-MEMBER 'A '(B A B B A C A D)))
(print (MULTIPLE-MEMBER 'A '(B A B B C C A D)))
(print (MULTIPLE-MEMBER 'A '(B A B B C D)))
(terpri)
(print "MONTH->INTEGER Test Cases")
(print (MONTH->INTEGER 'MARCH))
(print (MONTH->INTEGER 'JUNE))
(print (MONTH->INTEGER 'C))
(print (MONTH->INTEGER 7))
(print (MONTH->INTEGER 'QUOTE))
(print (MONTH->INTEGER '(MAY)))
(terpri)
(print "SCORE->GRADE Test Cases")
(print (SCORE->GRADE 86.3))
(print (SCORE->GRADE 106))
(print (SCORE->GRADE -10.1))
(print (SCORE->GRADE 59.9))
(print (SCORE->GRADE 83))
(print (SCORE->GRADE 74))
(print (SCORE->GRADE 67))
(print (SCORE->GRADE 87.0))
(print (SCORE->GRADE '(86.3)))
(print (SCORE->GRADE 'DOG))
(terpri)
(print "GT Test Cases")
(print (GT 0 -1))
(print (GT -3 -7))
(print (GT 40 40))
(print (GT 'B 'A))
(terpri)
(print "SAME-PARITY Test Cases")
(print (SAME-PARITY 0 -1))
(print (SAME-PARITY -3 -9))
(print (SAME-PARITY 30 90))
(print (SAME-PARITY 'A 'A))
(print (SAME-PARITY 4.1 3.7))
(terpri)
(print "SAFE-DIV Test Cases")
(print (SAFE-DIV 6 4))
(print (SAFE-DIV 6.0 4))
(print (SAFE-DIV 6 0))
(print (SAFE-DIV 6 0.0))
(print (SAFE-DIV '(6) 4))
(print (SAFE-DIV 6 T))