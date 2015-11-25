; tabuleiros gerados aleatoriamente

(defvar *tabuleiros* nil)

; FIXME:
; nao se pode alterar as pecas sem alterar alturas
; alterei tabuleiros para testes e nao tive isso em conta
(defun t1 ()
	#S(TABULEIRO   :CAMPO-JOGO   #2A((T T NIL T T T T T T T)       (T T T T NIL T T T T T)       (T T T NIL T T T T T T)       (T NIL T T T T T NIL NIL T)       (NIL T T T T NIL NIL NIL T T)       (NIL T T T NIL NIL NIL T T T)       (T NIL T T NIL T T T NIL T)       (NIL NIL T NIL T NIL T T T T)       (NIL T T T T T T T T NIL)       (NIL T T T T NIL NIL T NIL NIL)       (T NIL NIL T NIL NIL NIL T NIL T)       (NIL NIL NIL T NIL T NIL T T NIL)       (NIL NIL NIL NIL NIL NIL NIL T NIL T)       (NIL NIL NIL T T T T NIL T NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(11 10 17 14 14 14 14 13 15 13) :PAR-POS-MAIS-ALTA (16 . 2)   :OCUPADAS-NA-LINHA #(9 9 9 7 6 6 7 6 8 5 4 4 2 5 2 0 1 0 0)   :TOTAL-OCUPADAS 90)
)

(defun t2 ()
	#S(TABULEIRO   :CAMPO-JOGO   #2A((T T T T T T T T NIL T)       (T T NIL T T NIL T T T T)       (T T NIL T T T T T T NIL)       (T T T T T NIL T T T T)       (NIL T T T NIL T T T T T)       (T T NIL T NIL T NIL NIL T T)       (T NIL T NIL NIL T NIL NIL T T)       (T NIL NIL T T NIL T T T T)       (NIL T NIL T T NIL T NIL T T)       (T T T NIL T T NIL T NIL NIL)       (NIL NIL T NIL NIL NIL NIL T NIL NIL)       (NIL NIL T NIL T NIL T NIL NIL T)       (NIL NIL NIL T NIL NIL T T NIL T)       (T NIL NIL NIL NIL T NIL NIL NIL NIL)       (NIL T T T NIL T NIL NIL T NIL)       (NIL NIL NIL NIL NIL NIL T NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL T NIL)       (NIL T NIL T NIL NIL NIL NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(14 18 15 18 12 15 16 13 17 13) :PAR-POS-MAIS-ALTA (17 . 3)   :OCUPADAS-NA-LINHA #(9 8 8 9 8 6 5 7 6 6 2 4 4 2 5 1 1 2 0)   :TOTAL-OCUPADAS 93)
)

(defun t3 ()
	#S(TABULEIRO   :CAMPO-JOGO   #2A((NIL T NIL NIL T T NIL T NIL T)       (T NIL NIL NIL T NIL T T NIL NIL)       (T NIL NIL NIL T NIL NIL T NIL NIL)       (T T NIL NIL T NIL NIL T T T)       (NIL T NIL T NIL T NIL T NIL NIL)       (T NIL NIL NIL NIL T NIL NIL T NIL)       (T NIL T NIL NIL T NIL NIL T NIL)       (T NIL NIL T T NIL T T T T)       (NIL T NIL T T NIL T NIL T T)       (T T T NIL T NIL NIL T NIL NIL)       (NIL NIL T NIL NIL NIL NIL T NIL NIL)       (NIL NIL T NIL T NIL T NIL NIL T)       (NIL NIL NIL T T NIL T T NIL T)       (T T NIL NIL NIL T NIL NIL NIL NIL)       (NIL T T T NIL T NIL T T NIL)       (NIL NIL NIL NIL NIL NIL T NIL NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL T T T)       (T T NIL T NIL T T T NIL NIL)       (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))   :ALTURA-COLUNAS #(18 18 15 18 13 18 18 18 17 17) :PAR-POS-MAIS-ALTA (17 . 7)   :OCUPADAS-NA-LINHA #(5 4 3 6 4 3 4 7 6 5 2 4 5 3 6 1 3 6 0)   :TOTAL-OCUPADAS 77)
)

(push (t3) *tabuleiros*)
(push (t2) *tabuleiros*)
(push (t1) *tabuleiros*)

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

