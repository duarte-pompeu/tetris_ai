
(defun teste-nos-1 ()
	(let* ((estado-inicial (cria-estado '(o o)))
		(problema (cria-problema estado-inicial nil))
		(no (make-no :estado estado-inicial :operador nil :profundidade 0 :custo-caminho 0)))

	(expande-no no)
))
