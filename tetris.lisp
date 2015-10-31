; invocar (rl) faz reload do ficheiro
(defun rl ()
	(load "tetris.lisp")
	(load "tests.lisp")
)

; meter T para fazer print do mylog, nil para nao fazer;
; TODO: meter a nil antes de submeter no mooshak
(defvar  *DEBUG-MODE* T)

(defun mylog (message)
	(if *DEBUG-MODE*
		(format t "~a ~%" message)
	)
)

;;; DEFINICOES
(defconstant +linhas+ 18
  "o tabuleiro de jogo tem 18 linhas")
(defconstant +linha-maxima+ 17
  "o tabuleiro de jogo tem 0-17 linhas")
(defconstant +colunas+ 10
   "o tabuleiro de jogo tem 10 colunas")
(defconstant +coluna-maxima+ 9
   "o tabuleiro de jogo tem 0-9 colunas")
(defconstant +livre+ nil
   "o valor nil simboliza uma casa livre")
(defconstant +ocupada+ T
   "o valor T simboliza uma casa ocupada")





; 0.0.0 - Pecas (funcoes e variaveis auxiliares)
(defvar peca-i (list peca-i0 peca-i1))
(defvar peca-l (list peca-l0 peca-l1 peca-l2 peca-l3))
(defvar peca-j (list peca-j0 peca-j1 peca-j2 peca-j3))
(defvar peca-o (list peca-o0))
(defvar peca-s (list peca-s0 peca-s1))
(defvar peca-z (list peca-z0 peca-z1))
(defvar peca-t (list peca-t0 peca-t1 peca-t2 peca-t3))

(defun altura-peca (p)
	(array-dimension p 0)
)

(defun largura-peca (p)
	(array-dimension p 1)
)

; !!! talvez seja preciso fazer (quote peca) porque elas sao simbolos
(defun rotacoes-peca (p)
	(cond
		((equal p 'i) peca-i)
		((equal p 'l) peca-l)
		((equal p 'j) peca-j)
		((equal p 'o) peca-o)
		((equal p 's) peca-s)
		((equal p 'z) peca-z)
		((equal p 't) peca-t))
)


; TODO: estrutura com par ou simplesmente um par (sem defstruct, tipo defun) ??
; 2.1.1 - Tipo accao

; TODO: pode-se usar array ou tem que ser copia?
(defun cria-accao (inteiro array)
	(cons inteiro array)
)

(defun accao-coluna (accao)
	(car accao)
)

(defun accao-peca (accao)
	(cdr accao)
)

;;; 2.1.2 - TIPO TABULEIRO
(defstruct tabuleiro
  "estrutura que define o tipo tabuleiro de jogo
	  campo-jogo: um array que representa o campo de jogo cada posicao
		e' representada por um boolean em que
		T representa uma casa preenchida e nil uma vazia
	  altura-colunas: array numero de linhas por coluna
	  par-pos-mais-alta: par com linha e coluna da posicao mais alta
	  usadas-por-linha: array com numero de casas preenchidas por linha
	  total-ocupadas: numero de casas preenchidas no tabuleiro"
	campo-jogo
	altura-colunas
	par-pos-mais-alta
	ocupadas-na-linha
	total-ocupadas)


(defun cria-tabuleiro ()
  "cria e inicia a estrutura tabuleiro"
  (let ((campo (make-array (list +linhas+ +colunas+):initial-element nil))
	(alturas (make-array +colunas+ :initial-element 0))
	(altura-maxima (cons 0 0))
	(ocupadas-na-linha (make-array +linhas+ :initial-element 0))
	(casas-preenchidas 0))
	(make-tabuleiro :campo-jogo campo
			:altura-colunas alturas
			:par-pos-mais-alta altura-maxima
			:ocupadas-na-linha ocupadas-na-linha
			:total-ocupadas casas-preenchidas)))


(defun copia-array (array)
  "recebe um array e devolve uma co'pia do array sem alterar o original"
  (let* ((dims (array-dimensions array))
	 (new-array (make-array dims)))
	 (dotimes (i (array-total-size array))
	   (setf (row-major-aref new-array i) ; o array e' uma zona continua de memoria ordenada por linhas
		 (row-major-aref array i)))
	 new-array)) ;return


(defun copia-tabuleiro (tabuleiro)
  "recebe um tabuleiro de devolve uma co'pia do tabuleiro original"
  (let ((novo-tabuleiro (cria-tabuleiro)))
	(setf (tabuleiro-campo-jogo novo-tabuleiro)
	  (copia-array (tabuleiro-campo-jogo tabuleiro)))
	(setf (tabuleiro-altura-colunas novo-tabuleiro)
	  (copia-array (tabuleiro-altura-colunas tabuleiro)))
	(setf (tabuleiro-par-pos-mais-alta novo-tabuleiro)
	  (cons (car (tabuleiro-par-pos-mais-alta tabuleiro))
		(cdr (tabuleiro-par-pos-mais-alta tabuleiro))))
	(setf (tabuleiro-ocupadas-na-linha tabuleiro)
	  (copia-array (tabuleiro-ocupadas-na-linha tabuleiro)))
	(setf (tabuleiro-total-ocupadas novo-tabuleiro)
	  (tabuleiro-total-ocupadas tabuleiro))
	novo-tabuleiro))


(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  "predicado que devolve T se a casa linha coluna esta preenchida nil caso contra'rio"
  (aref (tabuleiro-campo-jogo tabuleiro) linha coluna))


(defun tabuleiro-altura-coluna (tabuleiro coluna)
  "recebe um tabuleiro e uma coluna e devolve a linha mais alta preenchida"
  (aref (tabuleiro-altura-colunas tabuleiro) coluna))


(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "recebe um tabuleiro e uma linha e devolve T caso a linha esteja preenchida ou completa falso caso contra'rio"
  (eq (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha) +colunas+))


(defun tabuleiro-preenche! (tabuleiro linha coluna)
  "marca a posicao linha coluna ocupada"
  (and (< linha +linhas+) (< coluna +colunas+) (>= linha 0) (>= coluna 0)
  (progn
	(setf (aref (tabuleiro-campo-jogo tabuleiro) linha coluna) +ocupada+)
	(incf (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha))
	(incf (tabuleiro-total-ocupadas tabuleiro))
	(when (< (aref (tabuleiro-altura-colunas tabuleiro) coluna) linha)
	  (progn
	(setf (aref (tabuleiro-altura-colunas tabuleiro) coluna) linha)
	(when (< (car (tabuleiro-par-pos-mais-alta tabuleiro)) linha)
	 (setf (tabuleiro-par-pos-mais-alta tabuleiro) (cons linha coluna))))))))


(defun encontra-maximo (array)
  "devolve o elemento maximo num array de numeros nao vazio"
  (let ((maximo (row-major-aref array 0)))
	(dotimes (i (array-total-size array))
	  (let ((elemento (row-major-aref array i)))
	(when (> elemento maximo) ;row-major-aref trata o array como um vector
	  (setf maximo elemento))))
	maximo))

;	(format T "~A " maximo)


(defun desce-linha! (tabuleiro linha)
  "copia sobre linha anterior apagando assim a informação que la' estava"
  (dotimes (i +colunas+)
	(setf (aref (tabuleiro-campo-jogo tabuleiro)(- linha 1) i) (aref (tabuleiro-campo-jogo tabuleiro) linha i))))


(defun tabuleiro-remove-linha! (tabuleiro linha)
  "remove a linha fazendo com que as linhas de cima descam uma casa"
  (let ((altura-max (encontra-maximo (tabuleiro-altura-colunas tabuleiro))))
	(dotimes (i  (+ altura-max 2)) ;queremos martelar a altura maxima com a linha acima dela
		  (desce-linha! tabuleiro (+ linha (+ i 1))))))


(defun tabuleiro-topo-preenchido-p (tabuleiro)
  "devolve T se houver uma peca que ocupa a linha 17"
  (eq (car (tabuleiro-par-pos-mais-alta tabuleiro)) +linha-maxima+))

;;Esta funcao pode comparar arrays com dimensoes garantidamente identicas que desta forma se encontram em memoria como um vector e as coordenadas de cada elemento vao concidir
(defun vectores-iguais-p (vec1 vec2)
  "dados dois vectores devolve T se tiverem a mesma composicao e nil caso contrario"
  (let ((dim-vec1 (array-total-size vec1))
	(dim-vec2 (array-total-size vec2))
	(result t))
	(and (eq dim-vec1 dim-vec2)
		 (dotimes (i dim-vec2 result)
		   (when (not (equal (row-major-aref vec1 i) (row-major-aref vec2 i)))
		 (progn (setf result nil)
			 (return)))))
		 result))


(defun pares-iguais-p (par outro)
  "predicado que compara dois pares de numeros e devolve T se forem identicos nil caso contr'ario"
  (and (eq (car par) (car outro)) (eq (cdr par) (cdr outro))))


(defun tabuleiros-iguais-p (tabuleiro outro)
  "dados dois tabuleiro devolve T se forem iguais e nil caso contr'ario"
  (cond ((/= (tabuleiro-total-ocupadas tabuleiro)  (tabuleiro-total-ocupadas tabuleiro)) nil)
	((not (pares-iguais-p (tabuleiro-par-pos-mais-alta tabuleiro) (tabuleiro-par-pos-mais-alta outro))) nil)
	((not (vectores-iguais-p (tabuleiro-altura-colunas tabuleiro) (tabuleiro-altura-colunas outro))) nil)
	((not (vectores-iguais-p (tabuleiro-ocupadas-na-linha tabuleiro) (tabuleiro-ocupadas-na-linha outro))) nil)
	((not (vectores-iguais-p (tabuleiro-campo-jogo tabuleiro) (tabuleiro-campo-jogo outro))) nil)
	(t t)))


(defun tabuleiro->array (tabuleiro)
  "recebe um tabuleiro e devvolve o array de booleans correspondente"
 (copia-array (tabuleiro-campo-jogo tabuleiro)))


(defun array->tabuleiro (array)
  "devolve o tabuleiro construido a partir de um array de 18 linhas e 10 colunas"
  (let ((tabuleiro (cria-tabuleiro)))
	(dotimes (i (array-total-size array))
	  (when (row-major-aref array i)
	(multiple-value-bind (linha coluna) (floor i 10)  ;floor devolve dois valores que ficam associados a linha coluna
	  (tabuleiro-preenche! tabuleiro linha coluna))))
	tabuleiro))

; 2.1.3 - tipo Estado

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)


(defun copia-estado (estado-orig) "copia um estado"
	(let ((estado-novo
		(make-estado
			:pontos (estado-pontos estado-orig)
			:pecas-por-colocar (estado-pecas-por-colocar estado-orig)
			:pecas-colocadas (estado-pecas-colocadas estado-orig)
			:tabuleiro (copia-tabuleiro (estado-tabuleiro estado-orig))
		)))
		estado-novo
	)
)


(defun estados-iguais-p (e1 e2) "estados iguais?"
	(if (equal (estado-pontos e1) (estado-pontos e2))
		(if (equal (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
			(if (equal (estado-pecas-colocadas e1) (estado-pecas-colocadas e2))
				(if (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))
					T
					nil
				)
			)
		)
	)
)


(defun estado-final-p (e1) "verifica se um estado e' estado final"
	(or
		(if (tabuleiro-topo-preenchido-p (estado-tabuleiro e1)) T nil)
		(if (null (estado-pecas-por-colocar e1)) T nil)
	)
)




(defstruct problema
	(estado-inicial)

	; um estado e' solucao quando nao ha mais pecas por colocar
	(solucao (function (lambda (estado)
		(not (null (estado-pecas-por-colocar estado))))))

	; 1. se (estado-final-p estado), retornar () (nada a fazer)
	; 2. tirar a primeira peca do estado
	;	1. para cada peca, obter possiveis rotacoes
	;	2. para cada rotacao, obter posicoes possiveis
	;	3. gerar accao com par peca e posicao
	; 4. devolver lista
	(accoes (function (lambda (estado)
		; 1
		(if (estado-final-p estado)
			t
			; 2
			(let* ((primeira-peca (first (pecas-por-colocar estado) 0))
				(rotacoes (rotacoes-peca primeira-peca))
				(accoes-possiveis '()))
				;2.1
				(loop for peca-rodada in rotacoes
				do (let* ((largura-peca (largura-peca peca-rodada))
						; NOTA: nao se esta a verificar se uma peca e' mais larga que o tabuleiro
						; segundo o enunciado e as pecas dadas, isso e' impossivel
						(max-col (- largura-tabuleiro largura-peca)))
					;2.2
					(loop for posicao upto max-col
					do (setq accoes-possiveis
							(cons (cria-accao posicao peca-rodada) accoes-possiveis))
					)
				))
			accoes-possiveis)
		)
	)))

	(resultado (function (lambda (estado accao) (not "placeholder"))))

	(custo-caminho (function (lambda (estado) (not "placeholder"))))
)

