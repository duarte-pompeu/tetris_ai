; invocar (rl) faz reload do ficheiro
(defun rl ()
	(load "tetris.lisp")
	(load "tests.lisp")
)

; meter T para fazer print do mylog, nil para nao fazer;
; TODO: meter a nil antes de submeter no mooshak
(setq *DEBUG-MODE* T)

(defun mylog (message)
	(if *DEBUG-MODE*
		(format t "~a ~%" message)
	)
)


; TODO: estrutura com par ou simplesmente um par (sem defstruct, tipo defun) ??
; 2.1.1 - Tipo accao

; TODO: pode-se usar array ou tem que ser copia?
(defun cria-accao (inteiro array)
	(cons inteiro array)
)

(defun accao-coluna (accao)
	(car accao)
)

(defun accao-peca (accao)
	(cdr accao)
)

; 2.1.2 - Tipo tabuleiro
(defstruct tabuleiro
	campo-jogo
	altura-colunas
)

(defun converte-linha (tabuleiro linha)
	(let* ((campo (tabuleiro-campo-jogo tabuleiro))
		(tamanho-campo (array-dimension campo 0))
		(max-linha (- tamanho-campo 1)))

		(- max-linha linha))
)

(defun n-colunas (tabuleiro)
	(array-dimension (tabuleiro-campo-jogo tabuleiro) 1)
)

(defun n-linhas (tabuleiro)
	(array-dimension (tabuleiro-campo-jogo tabuleiro) 0)
)

(defun max-coluna (tabuleiro)
	(- (n-colunas tabuleiro) 1)
)

(defun max-linha (tabuleiro)
	(- (n-linhas tabuleiro) 1)
)

(defun cria-tabuleiro ()
	(let ((tabuleiro (make-tabuleiro)))

		(setf (tabuleiro-campo-jogo tabuleiro)
			(make-array '(18 10)))
		(setf (tabuleiro-altura-colunas tabuleiro)
			(make-array `(10) :initial-element 0))

		tabuleiro)
)

(defun copia-tabuleiro (tabuleiro)
	(let* ((tabuleiro-velho tabuleiro)
		(tabuleiro-novo (cria-tabuleiro))
		(campo-velho (tabuleiro-campo-jogo tabuleiro-velho))
		(campo-novo (tabuleiro-campo-jogo tabuleiro-novo))
		(altura-colunas-velho (tabuleiro-altura-colunas tabuleiro-velho))
		(altura-colunas-novo (tabuleiro-altura-colunas tabuleiro-novo))
		(max-altura (reduce #'max altura-colunas-velho))
		(n-linhas (array-dimension campo-velho 0))
		(n-colunas (array-dimension campo-velho 1)))

		(loop for i from 0 to (- (array-dimension altura-colunas-novo 0) 1)
			do (setf (aref altura-colunas-novo i) (aref altura-colunas-velho i)))

		(loop for coluna upto (- n-colunas 1)
		do (loop for linha from (- n-linhas 1)
							downto (- n-linhas
								(aref altura-colunas-velho coluna))
			do (setf (aref campo-novo linha coluna)
				(aref campo-velho linha coluna))))

		tabuleiro-novo)
)

(defun tabuleiro-preenchido-p (tabuleiro n-linha n-coluna)
	(aref (tabuleiro-campo-jogo tabuleiro)
		(converte-linha tabuleiro n-linha)
		n-coluna)
)

(defun tabuleiro-altura-coluna (tabuleiro n-coluna)
	(aref (tabuleiro-altura-colunas tabuleiro) n-coluna)
)

(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(let* ((campo (tabuleiro-campo-jogo tabuleiro))
		(ultima-coluna (max-coluna tabuleiro))
		(resultado t))

		(loop for coluna upto ultima-coluna
		do (if (not (tabuleiro-preenchido-p tabuleiro linha coluna))
			(progn
			(setq resultado nil)
			(return))))

	resultado)
)

(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(let ((sucesso
		(tabuleiro-coloca-simbolo! tabuleiro linha coluna t)))

		; verificar se altura foi alterada e actualizar altura-colunas
		(if sucesso
			(if (> (+ linha 1) (aref (tabuleiro-altura-colunas tabuleiro) coluna))
				(setf (aref (tabuleiro-altura-colunas tabuleiro) coluna)
					(+ linha 1)))))
)

(defun tabuleiro-remove! (tabuleiro linha coluna)
	(let* ((sucesso
		(tabuleiro-coloca-simbolo! tabuleiro linha coluna nil))
		(alturas (tabuleiro-altura-colunas tabuleiro))
		(altura (aref alturas coluna)))

		; verificar se altura foi alterada e actualizar altura-colunas
		(if sucesso
			(if (or (tabuleiro-preenchido-p tabuleiro linha coluna)
					(= linha (- altura 1)))
				(progn (mylog (list "looog" tabuleiro coluna))
				(decf (aref alturas coluna)))))
))

(defun tabuleiro-coloca-simbolo! (tabuleiro linha coluna simbolo)
	(if (or (< linha 0) (> linha 17) (< coluna 0) (> coluna 9))
		nil
		(let ((campo (tabuleiro-campo-jogo tabuleiro))
			(linha-real (converte-linha tabuleiro linha))
			(incremento-altura 1))

			(setf (aref campo linha-real coluna) simbolo)
			t)
	)
)

(defun tabuleiro-remove-linha! (tabuleiro linha)
	; TODO: optimizar com altura das colunas
	; FIXME: actualizar valor das alturas

	(let* ((campo (tabuleiro-campo-jogo tabuleiro))
		(linha-real (converte-linha tabuleiro linha)))

		; remover linha
		(loop for coluna upto (max-coluna tabuleiro)
		do (tabuleiro-remove! tabuleiro linha coluna))

		; descer linhas acima
		(loop for l from linha upto (- (max-linha tabuleiro) 1)
		do (loop for c upto (max-coluna tabuleiro)
			do (progn
				(tabuleiro-remove! tabuleiro l c))

				(if (tabuleiro-preenchido-p tabuleiro (+ l 1) c)
					(tabuleiro-preenche! tabuleiro l c))))
			;~ do (tabuleiro-coloca-simbolo! tabuleiro l c
				;~ (tabuleiro-preenchido-p tabuleiro (+ l 1) c))))
	)
)

(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(let* ((alturas (tabuleiro-altura-colunas tabuleiro))
		(resultado nil))

		(loop for i upto (- (array-dimension alturas 0) 1)
			do (if (equal (aref alturas i) 18)
				(progn
					(setq resultado T)
					(return)))
		)
	resultado
	)
)




; 2.1.3 - tipo Estado

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)


(defun copia-estado (estado-orig) "copia um estado"
	(setf estado-novo
		(make-estado
			:pontos (estado-pontos estado-orig)
			:pecas-por-colocar (estado-pecas-por-colocar estado-orig)
			:pecas-colocadas (estado-pecas-colocadas estado-orig)
			:tabuleiro (copia-tabuleiro (estado-tabuleiro estado-orig))
		)
	)
)


(defun estados-iguais-p (e1 e2) "estados iguais?"
	(if (equal (estado-pontos e1) (estado-pontos e2))
		(if (equal (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
			(if (equal (estado-pecas-colocadas e1) (estado-pecas-colocadas e2))
				(if (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))
					T
					nil
				)
			)
		)
	)
)


(defun estado-final-p (e1) "verifica se um estado e' estado final"
	(or
		(if (tabuleiro-topo-preenchido-p (estado-tabuleiro e1)) T nil)
		(if (null (estado-pecas-por-colocar e1)) T nil)
	)
)

(defstruct problema
	(estado-inicial)

	; um estado e' solucao quando nao ha mais pecas por colocar
	(solucao (function (lambda (estado) (not (null (estado-pecas-por-colocar estado))))))
	(accoes 0)
	(resultado 0)
	(custo 0)
	(caminho 0)
)

