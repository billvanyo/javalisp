;;; compute factorial recursively using anonymous lambda functions

(((LAMBDA (F) ((LAMBDA (X) (F X)) (LAMBDA (X) (F X)))) (LAMBDA (F) (LAMBDA (N) (IF (ZEROP N) 1 (* N ((F F) (1- N))))))) 10)
