(load "tetris.lisp")

(defun teste-accao ()
	(let* ((inteiro 5)
		(arr #2A ((T T nil) (nil T T)))
		(acc (cria-accao inteiro arr)))

		(mylog acc)
		(cond ((not (equal inteiro (accao-coluna acc)))
				(mylog "accao-coluna failed"))
			((not (equal arr (accao-peca acc)))
				(mylog "accao-peca failed"))
				(T (progn
					(mylog "teste-accao passed")
					T))
		)
	)
)

(defun tabuleiro-pa-testes ()
	(let* ((tabuleiro (cria-tabuleiro))
		(ultima-coluna (max-coluna tabuleiro))
		(ultima-linha (max-linha tabuleiro))
		(col-end ultima-coluna))

		(loop for linha upto ultima-linha
		do (progn
			(loop for coluna upto col-end
			do (tabuleiro-preenche! tabuleiro linha coluna)))
			(decf col-end))

	tabuleiro)
)

; teste funcoes tabuleiro 1-4
(defun teste-tabuleiro1-4 ()
	(let* ((tab1 (cria-tabuleiro))
		(tab2 (copia-tabuleiro tab1))
		(tab3 "set it later"))

		(if (not (and (tabuleiro-p tab1) (tabuleiro-p tab2)))
			(mylog "FALHA teste 1 - nao sao tabuleiros")
			(mylog "passa teste 1"))

		(if (not (eq tab1 tab2))
			(mylog "FALHA teste 2 - tabuleiros diferentes")
			(mylog "passa teste 2"))

		(let ((i 0)
			(campo (tabuleiro-campo-jogo tab1))
			(altura-colunas (tabuleiro-altura-colunas tab1)))

			(loop for coluna upto (- (array-dimension altura-colunas 0) 1)
				do (progn
					(setf (aref altura-colunas coluna) i)
					(incf i)))

			(setf i 0)

			(loop for linha upto (- (array-dimension campo 0) 1)
			do (loop for coluna upto (- (array-dimension campo 1) 1)
				do (setf (aref campo linha coluna) (format nil "~3d" i))
					(incf i))))

		(setq tab3 (copia-tabuleiro tab1))

		(mylog tab1)
		(mylog tab2)
		(mylog tab3)
		(mylog "")
		(mylog "resultados esperados:")
		(mylog "tabuleiro1: ordenado de 0 a 179, alturas de 0 a 9")
		(mylog "tabuleiro2: campo a nil, alturas a 0")
		(mylog "tabuleiro3: +/- igual a tabuleiro1")
		(mylog "tabuleiro3 so deve copiar consoante altura-colunas")

		(if (and (not (equal (tabuleiro-altura-coluna tab3 0) 0))
				(not (equal (tabuleiro-altura-coluna tab3 9) 9)))
			(mylog "FALHA teste 3: erro ao verificar altura de colunas")
			(mylog "passa teste3"))
	)
)

(defun teste-tabuleiro-preenche ()
	(let ((tabuleiro (cria-tabuleiro)))
		; posicoes validas
		(tabuleiro-preenche! tabuleiro 0 0)
		(tabuleiro-preenche! tabuleiro 1 1)
		(tabuleiro-preenche! tabuleiro 2 2)
		(tabuleiro-preenche! tabuleiro 17 9)
		; posicoes invalidas
		(tabuleiro-preenche! tabuleiro -1 -1)
		(tabuleiro-preenche! tabuleiro 18 0)
		(tabuleiro-preenche! tabuleiro 18 5)
		(tabuleiro-preenche! tabuleiro 0 10)
		(tabuleiro-preenche! tabuleiro 18 10)

		(mylog tabuleiro))
)

(defun teste-tabuleiro-topo ()
	(let ((tabuleiro (cria-tabuleiro)))
		(mylog (tabuleiro-topo-preenchido-p tabuleiro))

		(tabuleiro-preenche! tabuleiro 0 0)
		(mylog (tabuleiro-topo-preenchido-p tabuleiro))

		(tabuleiro-preenche! tabuleiro 17 9)
		(mylog (tabuleiro-topo-preenchido-p tabuleiro))

		(mylog tabuleiro)
		(mylog "! so contam os resultados antes do print do tabuleiro")
		(mylog "1º resultado deve ser nil, 2º false")

	)
)

(defun teste-tabuleiro-linha-completa ()

	(let* ((tab (copia-tabuleiro (tabuleiro-pa-testes)))
		(resultado-teste t))

		(if (not (tabuleiro-linha-completa-p tab 0))
			(progn
			(mylog "FALHA: diz que linha 0 nao e completa")
			(setq resultado-teste nil)))

		(if (tabuleiro-linha-completa-p tab 1)
			(progn
			(mylog "FALHA: diz que linha 1 e completa")
			(setq resultado-teste nil)))

		(if (tabuleiro-linha-completa-p tab 9)
			(progn
			(mylog "FALHA: diz que linha é completa")
			(setq resultado-teste nil)))

		resultado-teste
		)
)

(defun teste-tabuleiro-remove ()
	(let* ((tab (copia-tabuleiro (tabuleiro-pa-testes))))
		(tabuleiro-remove-linha! tab 0)
		
		(mylog tab)
	)
)

(defun testa-tudo ()
	(let* ((func-names '(teste-accao
				teste-tabuleiro1-4
				teste-tabuleiro-preenche
				teste-tabuleiro-topo
				teste-tabuleiro-linha-completa
				teste-tabuleiro-remove
		))
		(conta-sucessos 0)
		(conta-total (list-length func-names)))

	(loop for func in func-names
	do (progn
		(let ((resultado (funcall func)))
			(mylog func)
			(mylog resultado)
			
			(if resultado
				(incf conta-sucessos)))))

	; nem todos os testes retornam valores adequados para avaliar aqui
	; pode haver falsos negativos
	(mylog "TESTES QUE FALHAM:")
	(- conta-total conta-sucessos))
)
