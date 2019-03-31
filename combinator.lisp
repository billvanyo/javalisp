;;; compute factorial recursively using anonymous lambda functions

(((LAMBDA (F) ((LAMBDA (X) (F X)) (LAMBDA (X) (F X)))) (LAMBDA (F) (LAMBDA (N) (IF (ZEROP N) 1 (TIMES N ((F F) (SUB1 N))))))) 10)
