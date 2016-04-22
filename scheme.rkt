(let
	((x (cons 1 (cons 2 '()))) (y (cons 3 (cons 4 '()))))
	(+ (car (cdr x)) (car (cdr y))))