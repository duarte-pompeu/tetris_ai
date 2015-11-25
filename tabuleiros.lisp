; tabuleiros gerados aleatoriamente

(defvar *tabuleiros* nil)

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

