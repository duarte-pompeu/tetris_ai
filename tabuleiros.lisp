; tabuleiros e pecas geradas aleatoriamente
; estao a ser guardados porque nao convem perder o tabuleiro enquanto
; se corrigem bugs

; caso queiram usar a funcao random, inicializem nova seed primeiro:
;(setf *random-state* (make-random-state t))
(defvar *todas-pecas* '(i j l s z t o))
(defvar *tabuleiros* nil)
(defvar *pecas-aleatorias* nil)

(defun n-pecas-aleatorias (n)
	(let ((pecas nil))
	
	(loop for i upto n
	do (push (nth (random 7) *todas-pecas*) pecas))
	
	pecas
))

(setf tabteste1 #S(TABULEIRO   :CAMPO-JOGO   #2A((NIL NIL T T NIL NIL T NIL NIL NIL)       (NIL NIL NIL NIL T T NIL NIL NIL NIL)       (NIL NIL NIL NIL T NIL NIL NIL NIL T)       (NIL NIL NIL NIL NIL T NIL NIL NIL NIL)       (NIL T T NIL T NIL NIL NIL NIL NIL)       (NIL NIL NIL T NIL NIL NIL T NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL T NIL NIL T NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (T NIL T NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL T NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL T)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(10 5 10 6 5 8 1 12 0 14) :PAR-POS-MAIS-ALTA (13 . 9)   :OCUPADAS-NA-LINHA #(3 2 2 1 3 2 0 2 0 2 0 1 0 1 0 0 0 0 0)   :TOTAL-OCUPADAS 19))
;~ +---------------------+
;~ |                     |
;~ |                     |
;~ |                     |
;~ |                   # |
;~ |                     |
;~ |               #     |
;~ |                     |
;~ | #   #               |
;~ |                     |
;~ |     #     #         |
;~ |                     |
;~ |       #       #     |
;~ |   # #   #           |
;~ |           #         |
;~ |         #         # |
;~ |         # #         |
;~ |     # #     #       |
;~ +---------------------+
(setf tabteste2 #S(TABULEIRO   :CAMPO-JOGO   #2A((T T T T T T T T NIL T)       (NIL T T NIL T T T NIL T T)       (NIL T T T T T NIL T NIL T)       (T NIL NIL T T T T T T NIL)       (T T T NIL T NIL NIL NIL NIL T)       (NIL NIL NIL T NIL T NIL T NIL NIL)       (T NIL NIL T NIL T NIL T T NIL)       (NIL NIL T NIL T T NIL NIL NIL NIL)       (T NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (T NIL T NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(10 5 10 7 8 8 4 7 7 5) :PAR-POS-MAIS-ALTA (9 . 2)   :OCUPADAS-NA-LINHA #(9 7 7 7 5 3 5 3 1 2 0 0 0 0 0 0 0 0 0)   :TOTAL-OCUPADAS 49))
;~ +---------------------+
;~ |                     |
;~ |                     |
;~ |                     |
;~ |                     |
;~ |                     |
;~ |                     |
;~ |                     |
;~ | #   #               |
;~ | #                   |
;~ |     #   # #         |
;~ | #     #   #   # #   |
;~ |       #   #   #     |
;~ | # # #   #         # |
;~ | #     # # # # # #   |
;~ |   # # # # #   #   # |
;~ |   # #   # # #   # # |
;~ | # # # # # # # #   # |
;~ +---------------------+
(setf tabteste-impossivel #S(TABULEIRO   :CAMPO-JOGO   #2A((T T T T T T T T NIL T)       (NIL T T T T T T T T T)       (T T T T T T T T NIL T)       (T T NIL T T T T T T T)       (T T T T T T T T NIL T)       (T T T T T T T T NIL T)       (T T T T T T T T T NIL)       (NIL T T T T T T T T T)       (T NIL T T T T T T T T)       (T T T T T T T T NIL T)       (T T T T T T T NIL T T)       (T T T T NIL T T T T T)       (T T T T T T NIL T T T)       (T T T NIL T T T T T T)       (T T T NIL T T T T T T)       (NIL T T T T T T T T T)       (T T T T NIL T T T T T)       (T NIL T T T T T T T T)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(18 17 18 18 18 18 18 18 18 18) :PAR-POS-MAIS-ALTA (17 . 9)   :OCUPADAS-NA-LINHA #(9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 0)   :TOTAL-OCUPADAS 162))
;~ +---------------------+ 
;~ | # # # #   # # # # # |
;~ |   # # # # # # # # # |
;~ | # # #   # # # # # # |
;~ | # # #   # # # # # # |
;~ | # # # # # #   # # # |
;~ | # # # #   # # # # # |
;~ | # # # # # # #   # # |
;~ | # # # # # # # #   # |
;~ | #   # # # # # # # # |
;~ |   # # # # # # # # # |
;~ | # # # # # # # # #   |
;~ | # # # # # # # #   # |
;~ | # # # # # # # #   # |
;~ | # #   # # # # # # # |
;~ | # # # # # # # #   # |
;~ |   # # # # # # # # # |
;~ | # # # # # # # #   # |
;~ +---------------------+
(push tabteste-impossivel *tabuleiros*)
(push tabteste2 *tabuleiros*)
(push tabteste1 *tabuleiros*)


; GERAR PECAS:
; clisp -i "tetris.lisp" -x "(rl) (setf *random-state* (make-random-state t)) (n-pecas-aleatorias 9)"

; 3 pecas
(setf pteste-3a '(Z S T))
(setf pteste-3b '(O L L))
(setf pteste-3c '(T S J))

; 10 pecas
(setf pteste-10a '(S I Z I T O O L S I))
(setf pteste-10b '(O J T S I L Z T Z S))
(setf pteste-10c '(L I J S S Z O Z I Z))

; 50 pecas
(setf pteste-50a '(Z O O Z S I T S Z I T O O O Z O L S L J T Z J T I Z J O O L S I S J Z T S T T S S J O O S Z J T Z S T))
(setf pteste-50b '(Z Z S I I L O J I O L O J L O O J L Z O O S J L T S S S S O J J Z O Z L I Z O L J O O I Z Z J L I L))
(setf pteste-50c '((O T Z J L T S I S O J Z S S J Z O S J I J O O S O Z T T I T J I Z O T L L L T L L Z Z L L Z Z S L S))

(defun obtem-estados (&optional pecas-por-colocar)
	(let ((estados nil))
		(loop for tab in *tabuleiros*
		do (push (make-estado :pontos 0 :pecas-por-colocar pecas-por-colocar  :pecas-colocadas nil :tabuleiro tab) estados))
		
	estados
))

(defun desenha-tabs (&optional pecas-por-colocar)
	(loop for estado in (obtem-estados pecas-por-colocar)
	do (desenha-estado estado))
)

