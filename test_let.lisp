(load "let_macro.lisp")

(let ((a 1) (b 2)) (+ a b))

(let ((a 3) (b 5)) (let ((x (* a b)) (y (+ a b))) (list x y)))


(defun scopetest (a b) (lambda (x y) (let ((n (+ a x)) (m (+ b y))) (list n m))))

(defun scopetest2 (a b x y n m) (list ((scopetest 3 7) a b) ((scopetest 3 7) x y) ((scopetest 3 7) n m)))

(scopetest2 100 200 300 400 500 600)

