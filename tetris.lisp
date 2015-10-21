; invocar (rl) faz reload do ficheiro
(defun rl ()
	(load "tetris.lisp")
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
(defstruct accao
	par
)

; TODO: pode-se usar array ou tem que ser copia?
(defun cria-accao (inteiro array)
	(make-accao :par (cons inteiro array))
)

(defun accao-coluna (accao)
	(car (accao-par accao))
)

(defun accao-peca (accao)
	(cdr (accao-par accao))
)

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

; 2.1.2 - Tipo tabuleiro
(defstruct tabuleiro
	campo-jogo
	altura-colunas
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
		(campo-velho (tabuleiro-altura-colunas tabuleiro-velho))
		(campo-novo (tabuleiro-altura-colunas tabuleiro-novo))
		(altura-colunas-velho (tabuleiro-altura-colunas tabuleiro-velho))
		(altura-colunas-novo (tabuleiro-altura-colunas tabuleiro-novo))
		(max-altura (reduce #'max altura-colunas-velho)))

		(loop for i from 0 to (- (array-dimension altura-colunas-novo 0) 1)
			do (setf (aref altura-colunas-novo i) (aref altura-colunas-velho i)))

		;TODO: copiar campo de jogo
		; usar tabuleiro-altura-coluna para reduzir nº de ciclos
		(loop for coluna upto  (- (array-dimension altura-colunas-velho 0) 1)
			for linha upto (- (tabuleiro-altura-coluna tabuleiro coluna) 1)
		do (setf campo-novo linha coluna
			(aref campo-velho linha coluna)))

		tabuleiro-novo)
)

(defun tabuleiro-preenchido-p (tabuleiro n-linha n-coluna)
	(aref (tabuleiro-campo-jogo tabuleiro) n-linha n-coluna)
)

(defun tabuleiro-altura-coluna (tabuleiro n-coluna)
	(aref (tabuleiro-altura-colunas tabuleiro) n-coluna)
)

; teste funcoes tabuleiro 1-4
(defun teste-tabuleiro1-4 ()
	(let* ((tab1 (cria-tabuleiro))
		(tab2 (copia-tabuleiro tab1)))
		
		(setf (tabuleiro-campo-jogo tab1)
			(make-array '(18 10) :initial-element 5))
		(setf (tabuleiro-altura-colunas tab1)
			(make-array `(10) :initial-element 3))
			
		(mylog tab1)
		; these should differ: create table 1, copy to table2, modify table 2
		(mylog tab2)
	)
)
