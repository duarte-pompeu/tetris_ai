(load "tetris.lisp")
(load "testes-tab.lisp")

;
(defun preenche-1 ()
	"coloca um i em 0 0"

	(let ((tab (cria-tabuleiro)))

		(coloca tab (first peca-i) 0 0)
))

(defun preenche-2 ()
	"coloca um varios i's deitados no tabuleiro
	e depois uma peca com casas vazias por cima
	objectivo: verificar que casas a NIL na peca nao eliminam pecas no tabuleiro"

	(let ((tab (cria-tabuleiro)))

	(coloca tab (first (rest peca-i)) 0 0)
	(coloca tab (first (rest peca-i)) 0 4)
	(coloca tab (first peca-z) 0 7)
))

(defun preenche (tab peca l c)
	(tabuleiro-preenche-peca! tab peca l c)

	(desenha-estado (make-estado :tabuleiro tab))
)

(defun larga-1 ()
	(larga (cria-tabuleiro) peca-l0 0)
)

(defun larga-u-1 ()
	(let ((tab (copia-tabuleiro (tab-u))))
	(larga tab peca-l0 2)
	(larga tab peca-l0 2)

	(larga tab peca-l0 6)
	(larga tab peca-l0 6)
	(larga tab peca-l2 6)

))

(defun larga (tab peca col)
	(tabuleiro-larga-peca! tab peca col)

	(desenha-estado (make-estado :tabuleiro tab))
)
