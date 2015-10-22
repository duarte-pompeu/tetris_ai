(load "tetris.lisp")

(defun teste-accao ()
	(let* ((inteiro 5)
		(arr #2A ((T T nil) (nil T T)))
		(acc (cria-accao inteiro arr)))

		(cond ((not (accao-p acc))
				(mylog "accao-p failed"))
			((not (equal inteiro (accao-coluna acc)))
				(mylog "accao-coluna failed"))
			((not (equal arr (accao-peca acc)))
				(mylog "accao-peca failed"))
				(T (progn
					(mylog "teste-accao passed")
					T))
		)
	)
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
