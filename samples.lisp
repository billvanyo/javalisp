; append two lists
(defun append (l1 l2) (if (nullp l1) l2 (cons (car l1) (append (cdr l1) l2))))

; apply a function to every element of a list, returning a list of the results
(defun mapcar (f l) (if (nullp l) nil (cons (f (car l)) (mapcar f (cdr l)))))

; given a list representation of a set of elements, returna  list of all subsets of that list
(defun subsets (l) (if (nullp l) '(()) ((lambda (s) (append s (mapcar (lambda (x) (cons (car l) x)) s))) (subsets (cdr l)))))

; similar to mapcar, except the function is assumed to return a list, and the lists returned by the function are appened rather than returned within list
(defun mapappend (f l) (if (nullp l) nil (append (f (car l)) (mapappend f (cdr l)))))

; given an element x and a list l, return a list of all possible ways of inserting x somewhere in list l
(defun insert (x l) (if (nullp l) (cons (cons x nil) nil) (cons (cons x l) (mapcar (lambda (l2) (cons (car l) l2)) (insert x (cdr l))))))

; return a list of all permutations of some list (this does not remove duplicates)
; for each permutation of the cdr of the list, generate all ways of inserting the car of the list
(defun perms (l) (if (nullp l) '(()) (mapappend (lambda (p) (insert (car l) p)) (perms (cdr l)))))

; standard recursive factorial
(defun factorial (n) (if (zerop n) 1 (* n (factorial (1- n)))))

; length of a list
(defun length (l) (if (nullp l) 0 (1+ (length (cdr l)))))

; list of integers from n down to 1
(defun ints (n) (if (zerop n) '() (cons n (ints (1- n)))))

; less standard and much less efficient implementation of factorial function
(defun badfact (n) (length (perms (ints n))))

; recursive factorial without defining a named function, using a variant of the Y combinator
((lambda (f x) (f f x)) (lambda (f n) (if (zerop n) 1 (* n (f f (1- n))))) 6)

(factorial 6)

(badfact 6)

