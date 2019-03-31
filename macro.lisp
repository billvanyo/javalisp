(defun mapcar (f l) (if (nullp l) nil (cons (f (car l)) (mapcar f (cdr l)))))

(defun compose (f1 f2) (lambda (x) (f1 (f2 x))))

; define let as a macro
(defmacro let (alist body) `((lambda ,(mapcar 'car alist) ,body) ,@(mapcar (compose 'car 'cdr) alist)))

(let ((a 1) (b 2)) (plus a b))

(let ((a 3) (b 5)) (let ((x (times a b)) (y (plus a b))) (list x y)))


(defun scopetest (a b) (lambda (x y) (let ((n (plus a x)) (m (plus b y))) (list n m))))

(defun scopetest2 (a b x y n m) (list ((scopetest 3 7) a b) ((scopetest 3 7) x y) ((scopetest 3 7) n m)))

(scopetest2 100 200 300 400 500 600)

