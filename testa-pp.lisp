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
	
	(setf sequencia-solucao (procura-pp (cria-problema estado (function qualidade))))
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

;; teste 15
(ignore-value (setf t15 (cria-tabuleiro)))
;;deve retornar IGNORE
(ignore-value (dotimes (linha 17) (dotimes (coluna 10) (tabuleiro-preenche! t15 linha coluna))))
;;deve retornar NIL (nao existe solucao)
(setf e15 (make-estado :pontos 0 :tabuleiro t15 :pecas-colocadas () :pecas-por-colocar '(i)))
(setf p15 (make-problema :estado-inicial e15 :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'(lambda (x) 0)))
(setf pp15 (procura-pp p15))

(defun t15-solucoes ()
	(loop for acc in (accoes e15)
do ( mylog (solucao (resultado e15 acc))))
)

(defun t15-topo ()
	(loop for acc in (accoes e15)
do (progn
	(let* ((estado-resultante (resultado e15 acc))
		(tab (estado-tabuleiro estado-resultante)))
	(mylog tab)
	(mylog "topo preenchido?") 
	(mylog (tabuleiro-topo-preenchido-p tab))))
))
