(load "tetris.lisp")

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
	
	tab
)
