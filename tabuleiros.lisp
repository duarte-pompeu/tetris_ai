; tabuleiros e pecas geradas aleatoriamente
; estao a ser guardados porque nao convem perder o tabuleiro enquanto
; se corrigem bugs

; caso queiram usar a funcao random, inicializem nova seed primeiro:
;(setf *random-state* (make-random-state t))
(defvar *todas-pecas* '(i j l s z t o))
(defvar *tabuleiros* nil)
(defvar *pecas-aleatorias* nil)

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


; tabteste3: pecas sem solucao:
; (S S O Z O O L O L I I L T I T L Z S I L Z)
(setf tabteste3 #S(TABULEIRO   :CAMPO-JOGO   #2A((NIL T T T T T T T T T)       (T NIL T T T T T T T T)       (T T T T T T T NIL T T)       (T NIL T NIL T NIL T T NIL T)       (NIL T NIL NIL T T NIL NIL T T)       (NIL T T NIL NIL T NIL T NIL T)       (T T NIL NIL NIL T T T T T)       (NIL NIL T NIL T NIL T T T NIL)       (NIL T T T NIL NIL T NIL NIL NIL)       (T T NIL NIL NIL T NIL NIL T T)       (NIL T NIL NIL NIL NIL NIL T NIL T)       (T T NIL T NIL T T T T NIL)       (NIL T NIL T NIL T T T T NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL T NIL NIL NIL NIL T T NIL)       (NIL NIL T T T NIL NIL T T T)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(12 13 16 16 16 13 13 16 16 16) :PAR-POS-MAIS-ALTA (15 . 9)   :OCUPADAS-NA-LINHA #(9 9 9 6 5 5 7 5 4 5 3 7 6 0 3 6 0 0 0)   :TOTAL-OCUPADAS 89))
;~ +---------------------+
;~ |                     |
;~ |     # # #     # # # |
;~ |     #         # #   |
;~ |                     |
;~ |   #   #   # # # #   |
;~ | # #   #   # # # #   |
;~ |   #           #   # |
;~ | # #       #     # # |
;~ |   # # #     #       |
;~ |     #   #   # # #   |
;~ | # #       # # # # # |
;~ |   # #     #   #   # |
;~ |   #     # #     # # |
;~ | #   #   #   # #   # |
;~ | # # # # # # #   # # |
;~ | #   # # # # # # # # |
;~ |   # # # # # # # # # |
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
(push tabteste3 *tabuleiros*)
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

; 20 pecas
(setf pteste-20a '(S I T L Z L I I S S T O L L J T T Z Z I))
(setf pteste-20b '(L L I L S L S O S I J I L I J L J S J L))
(setf pteste-20c '(L L S I Z L I T I I S L Z L Z J T T I Z))

; 50 pecas
; um bocado abusado, estas demoram muito tempo
(setf pteste-50a '(Z O O Z S I T S Z I T O O O Z O L S L J T Z J T I Z J O O L S I S J Z T S T T S S J O O S Z J T Z S T))
(setf pteste-50b '(Z Z S I I L O J I O L O J L O O J L Z O O S J L T S S S S O J J Z O Z L I Z O L J O O I Z Z J L I L))
(setf pteste-50c '(O T Z J L T S I S O J Z S S J Z O S J I J O O S O Z T T I T J I Z O T L L L T L L Z Z L L Z Z S L S))

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

(defun pbr0 ()
	(testa-best (cria-tabuleiro) '(o i i))
)

( defun ppr0 ()
	(testa-pp  '(o i i) (cria-tabuleiro))
)

(defun pbr1 ()
	(testa-best tabteste3 '(o i i))
)

( defun ppr1 ()
	(testa-pp  '(o i i) tabteste3)
)


(defun pbr2 ()
	(testa-best tabteste3 '(S I Z I T O O L S I))
)

( defun ppr2 ()
	(testa-pp  '(S I Z I T O O L S I) tabteste3)
)

(defun pbr3 ()
	(testa-best tabteste3 pteste-20a)
)

( defun ppr3 ()
	(testa-pp  pteste-20a tabteste3)
)


;; heuristicas

(defun pah11 ()
	(testa-heur (function heuristica-dif-colunas) (function custo-oportunidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah12 ()
	(testa-heur (function heuristica-casas-ocupadas) (function custo-oportunidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah13 ()
	(testa-heur (function heuristica-buracos) (function custo-oportunidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah14 ()
	(testa-heur (function heuristica-altos-e-baixos) (function custo-oportunidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah15 ()
	(testa-heur (function heuristica-best) (function custo-oportunidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah21 ()
	(testa-heur (function heuristica-dif-colunas) (function qualidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah22 ()
	(testa-heur (function heuristica-casas-ocupadas) (function qualidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah23 ()
	(testa-heur (function heuristica-buracos) (function qualidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah24 ()
	(testa-heur (function heuristica-altos-e-baixos) (function qualidade) '(S I Z I T O O L S I) tabteste3)
)

(defun pah25 ()
	(testa-heur (function heuristica-best) (function qualidade) '(S I Z I T O O L S I) tabteste3)
)

 


