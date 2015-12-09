
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

(defun testa-heur (heuristica funcao-custo lista-pecas &optional tabuleiro )
	(let* ((estado (cria-estado lista-pecas))
		(sequencia-solucao nil))
		
		(if tabuleiro
			(setf (estado-tabuleiro estado) tabuleiro))
	
	(setf sequencia-solucao (procura-a* (cria-problema estado funcao-custo) heuristica))
	(desenha-estado estado)
	(funcall (desenha-jogada) estado sequencia-solucao)
	
	(mylog "numero pecas colocadas:")
	(mylog (length sequencia-solucao))
	 
	 sequencia-solucao
))


(defun testa-best (tabuleiro lista-pecas )
	(let* ((estado (cria-estado lista-pecas))
		(sequencia-solucao nil))
		
		(if tabuleiro
			(setf (estado-tabuleiro estado) tabuleiro))
	
	(setf sequencia-solucao (procura-best (tabuleiro->array tabuleiro) lista-pecas))
	(desenha-estado estado)
	(funcall (desenha-jogada) estado sequencia-solucao)
	 
	 sequencia-solucao
))

; com 8 o's e 2 i's num tabuleiro vazio, resultado esperado sao 800 pontos
(defun pa*1 ()
	(testa-a* '(o o o o o o o o i i))
)

(defun pb1 ()
	(testa-best (cria-tabuleiro) '(o i i))
)

(defun pb2 ()
	(testa-best (cria-tabuleiro) '(o o o o o o o o i i))
)
