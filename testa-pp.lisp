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
	(funcall (desenha-jogada) estado sequencia-solucao)
	 
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
	(testa-pp '(o o) (nth 0 *tabuleiros*))
)

(defun ppt2 ()
	(testa-pp '(o o) (nth 1 *tabuleiros*))
)

(defun ppt3 ()
	(testa-pp '(o o) (nth 2 *tabuleiros*))
)


