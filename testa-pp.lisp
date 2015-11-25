; clisp -i "tetris.lisp" -x "(rl) (pp2)" | less
; faz load a todos os comandos (rl) e executa teste (pp2 neste exemplo)


; grupo-executa-jogada - imprime tudo de uma vez
; executa-jogadas (utils.lisp) - interactivo
; para escolher: comentar a funcao a nao usar
(defun desenha-jogada ()
	(function grupo-executa-jogadas)
	;(function executa-jogadas)
)


(defun testa-pp (lista-pecas &optional tabuleiro)
	(let* ((estado (cria-estado lista-pecas))
		(sequencia-solucao nil))
		
		(if tabuleiro
			(setf (estado-tabuleiro estado) tabuleiro))
	
	(setf sequencia-solucao (procura-pp (cria-problema estado nil)))
	(desenha-estado estado)
	(funcall (desenha-jogada) estado sequencia-solucao)
	
	(mylog "numero pecas colocadas:")
	(mylog (length sequencia-solucao))
	 
	 sequencia-solucao
))


(defun pp1 ()
	(testa-pp '(o o))
)


(defun pp2 ()
	(testa-pp '(o o o o o o o o o o o o o o o o o o o o o o o o o o o
		o o o o o o o o o o o o o o o o o o o o o ))
)

(defun pp3()
	(testa-pp '(o))
)

(defun pp4()
	(testa-pp '())
)


(defun grupo-executa-jogadas (estado-inicial sequencia-jogadas)
	(let ((estado estado-inicial))
		(loop for peca in sequencia-jogadas
		do (progn
			(setf estado (resultado estado peca))
			(desenha-estado estado)))
))

; procuras com tabuleiros inicializados
(defun ppt1 ()
	(testa-pp '(o o) tabteste1))

(defun ppt2 ()
	(testa-pp '(o o) tabteste2))

(defun pptimpossivel ()
	(testa-pp '(i j l l l l l l) tabteste-impossivel))

; ver se alguma peca da porcaria
(defun stress-impossivel ()
	(let ((pecas '(i j l s z t o)))
	
	(loop for peca in pecas
	do (testa-pp (list peca) tabteste-impossivel)))
)

; uso: (rand-stress [tabuleiro] [numero de pecas a colocar] [numero de loops]
; clisp -i "tetris.lisp" -x "(rl) (rand-stress tabteste2 30 3)"
; termina quando chega a estados impossiveis (ou quando da erro)
(defun rand-stress (tabuleiro n-pecas n-loops)
	(setf *random-state* (make-random-state t))

	(loop for i upto (1- n-loops)
		do (let* ((pecas (n-pecas-aleatorias n-pecas))
			(resultado (testa-pp pecas tabuleiro)))
			(mylog i)
		
		(if (null resultado)
			(progn 
			(mylog "nao encontrou solucao")
			(mylog pecas)
			(return-from rand-stress resultado)))))
)
