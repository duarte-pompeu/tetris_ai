;(load (compile-file "tetris.lisp"))

; ctrl-f FIXME -> erros mais graves

; ctrl-f BUG -> todos os erros, alguns nao obedecem as regras do tetris
; (nao sei se o mooshak testa isso, por isso dar prioridade aos FIXME)

(defun tab-u ()
	"tabuleiro em u"

; BUG: preencher a mesma casa varias vezes aumenta numero de ocupadas-na-linha e numero total de casas ocupadas
; no entanto, isto nao obedece as regras do jogo

	(let ((tab (cria-tabuleiro)))

		(loop for c upto 9
		do (tabuleiro-preenche! tab 0 c))

		(loop for l upto 16
		do (progn (tabuleiro-preenche! tab l 0)
			(tabuleiro-preenche! tab l 9)))

		tab
))

(defun u-desce ()
	"remover linha do U e descer"

	(let ((tab (copia-tabuleiro (tab-u))))
	(tabuleiro-remove-linha! tab 0)

	tab
))

(defun preenche-100 ()
; BUG: :OCUPADAS-NA-LINHA #(101 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :TOTAL-OCUPADAS 101)
; no entanto, isto nao obedece as regras do jogo

	"tabuleiro em que se preenche uma so casa (0,0) varias vezes"
	(let ((tab (cria-tabuleiro)))

	(loop for i upto 100
	do (tabuleiro-preenche! tab 0 0))

	tab
))

(defun remove-linhas-100 ()
; BUG: :PAR-POS-MAIS-ALTA (-101 . 0)
; no entanto, isto nao obedece as regras do jogo

	"tabuleiro em que se remove linhas nao existentes"
	(let ((tab (cria-tabuleiro)))

	(loop for i upto 100
	do (tabuleiro-remove-linha! tab 0))

	tab
))

(defun tab-pr1 ()
; BUG: :PAR-POS-MAIS-ALTA (-1 . 9)
; FIXME: este bug e uma situacao +- normal de jogo, sem input fora do esperado

	"prenche e remove linha - 1x"
	(let ((tab (cria-tabuleiro)))

		(loop for c upto 9
		do (tabuleiro-preenche! tab 0 c))

		(tabuleiro-remove-linha! tab 0)

	tab
))

(defun tab-pr100 ()
; BUG: :PAR-POS-MAIS-ALTA (-1 . 9) (igual ao 1x)
; FIXME: este bug e uma situacao +- normal de jogo, sem input fora do esperado

	"prenche e remove linha - 100x"
	(let ((tab (cria-tabuleiro)))

		(loop for i upto 99
		do (progn
			(loop for c upto 9
			do (tabuleiro-preenche! tab 0 c))

			(tabuleiro-remove-linha! tab 0)))

	tab
))

(defun topo-p ()
	"topo preenchido"

	(let ((tab (cria-tabuleiro)))

		(loop for c upto 0
		do (tabuleiro-preenche! tab 17 c))

	(tabuleiro-topo-preenchido-p tab)
))

(defun topo-p-mal ()
	"preenchidas casas inexistentes na linha 17"

	(let ((tab (cria-tabuleiro)))

		(loop for c from 10 upto 30
		do (tabuleiro-preenche! tab 17 c))

		(loop for c from -10 upto -1
		do (tabuleiro-preenche! tab 17 c))

	(tabuleiro-topo-preenchido-p tab)
))

(defun igual-1 ()
	"copia simples"

	(let* ((tab1 (cria-tabuleiro))
		(tab2 (copia-tabuleiro tab1)))

	(tabuleiros-iguais-p tab1 tab2)
))

(defun igual-2 ()
	"copia, dps altera tab1 -> nao sao iguais"

	(let* ((tab1 (cria-tabuleiro))
		(tab2 (copia-tabuleiro tab1)))

	(tabuleiro-preenche! tab1 0 0)
	(tabuleiros-iguais-p tab1 tab2)
))

(defun igual-3 ()
	"cria tabuleiro com casa preenchida, copia dps -> sao iguais"

	(let* ((tab1 (cria-tabuleiro))
		(tab2 nil))

	(tabuleiro-preenche! tab1 0 0)
	(setf tab2 (copia-tabuleiro tab1))
	(tabuleiros-iguais-p tab1 tab2)
))


(defun tab-cheio ()
  "cria um tabuleiro cheio"

  (let ((tabuleiro (cria-tabuleiro)))
    (dotimes (i +linhas+ tabuleiro)
      (dotimes (j  +colunas+)
	(tabuleiro-preenche! tabuleiro i j)))))
