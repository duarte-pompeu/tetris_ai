; dica: isto faz load a todos os ficheiros (devido ao (rl))
; e interpreta uma ou varias funcoes e envia para o less
; exemplo para correr o teste pp2
; clisp -i "tetris.lisp" -x "(rl) (pp2)" | less

(defun testa-pp (lista-pecas)
	(let* ((estado (cria-estado lista-pecas))
		(sequencia-solucao
			(procura-pp (cria-problema estado nil))))

	(loop for peca in sequencia-solucao
	do (progn
		(setf estado (resultado estado peca))
		(desenha-estado estado)))
))

(defun pp1 ()
	(testa-pp '(o o))
)

(defun pp2 ()
	(testa-pp '(o o o o o o o o o o o o o o o o o o o o o o o o o o o
		o o o o o o o o o o o o o o o o o o o o o ))
)
