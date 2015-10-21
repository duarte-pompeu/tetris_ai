; invocar (rl) faz reload do ficheiro
(defun rl ()
	(load "tetris.lisp")
)

; meter T para fazer print do mylog, nil para nao fazer;
; TODO: meter a nil antes de submeter no mooshak
(setq *DEBUG-MODE* T)

(defun mylog (message)
	(if *DEBUG-MODE*
		(format T message)
	)
)


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
