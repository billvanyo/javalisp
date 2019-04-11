(defun mapcar (f l) (if (null l) nil (cons (f (car l)) (mapcar f (cdr l)))))

(defun compose (f1 f2) (lambda (x) (f1 (f2 x))))

; define let as a macro
(defmacro let (alist body) `((lambda ,(mapcar 'car alist) ,body) ,@(mapcar (compose 'car 'cdr) alist)))
