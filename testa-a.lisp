
(defun testa-a* (lista-pecas &optional tabuleiro)
	(let* ((estado (cria-estado lista-pecas))
		(sequencia-solucao nil))
		
		(if tabuleiro
			(setf (estado-tabuleiro estado) tabuleiro))
	
	(setf sequencia-solucao (procura-a* (cria-problema estado (function qualidade)) #'sem-heuristica))
	(desenha-estado estado)
	(funcall (desenha-jogada) estado sequencia-solucao)
	
	(mylog "numero pecas colocadas:")
	(mylog (length sequencia-solucao))
	 
	 sequencia-solucao
))

; com 8 o's e 2 i's num tabuleiro vazio, resultado esperado sao 800 pontos
(defun pa*1 ()
	(testa-a* '(o o o o o o o o i i))
)
