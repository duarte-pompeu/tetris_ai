; CARACTERES NAO ASCII: [^\x00-\x7F]+
; basta pesquisarem com regex, caso o vosso editor suporte
; em Linux, nao funciona com cat | grep, nao sei pq

;invocar (rl) faz reload do ficheiro
(defun rl ()
 	(load "tetris.lisp")
	(load "tests.lisp")
	(load "testes-tab.lisp")
	(load "tests-problema.lisp")
)


;; ; meter T para fazer print do mylog, nil para nao fazer;
;; ; TODO: meter a nil antes de submeter no mooshak
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

(defconstant +PONTUACAO-LINHAS+ '#(0 100 300 500 800))


;;; 2.1.1 - Tipo accao
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
      par-pos-mais-alta: par com linha e coluna da posicao mais alta e ultima posicao de memoria
      usadas-por-linha: array com numero de casas preenchidas por linha
      total-ocupadas: numero de casas preenchidas no tabuleiro"
	campo-jogo
	altura-colunas
	par-pos-mais-alta
	ocupadas-na-linha
	total-ocupadas)


(defun cria-tabuleiro ()
  "cria e inicia a estrutura tabuleiro"
  (let ((campo (make-array '(19  10) :initial-element nil)) ;uma linha extra 19 em vez de 18  para facilitar o remove linha
	(alturas (make-array +colunas+ :initial-element 0))
	(altura-maxima (cons 0 0))
	(ocupadas-na-linha (make-array 19 :initial-element 0)) ;um nil extra para facilitar o remove linha
	(casas-preenchidas 0))
	(make-tabuleiro :campo-jogo campo
			:altura-colunas alturas
			:par-pos-mais-alta altura-maxima
			:ocupadas-na-linha ocupadas-na-linha
			:total-ocupadas casas-preenchidas)))


(defun copia-array (array &optional (limite (array-total-size array)) init-element (dims (array-dimensions array)))
  "recebe um array e devolve uma co'pia do array sem alterar o original"
  (let ((new-array (make-array dims :initial-element init-element)))
    (dotimes (i limite)
      (setf (row-major-aref new-array i) ; o array e' uma zona continua de memoria ordenada por linhas
	    (row-major-aref array i)))
    new-array)) ;return


(defun copia-tabuleiro (tabuleiro)
  "recebe um tabuleiro de devolve uma co'pia do tabuleiro original"
  (let ((novo-tabuleiro (cria-tabuleiro))
	(campo-jogo (tabuleiro-campo-jogo tabuleiro))
	(alturas (tabuleiro-altura-colunas tabuleiro))
	(linha-mais-alta (car (tabuleiro-par-pos-mais-alta tabuleiro)))
	(coluna-mais-alta (cdr (tabuleiro-par-pos-mais-alta tabuleiro)))
	(ocupadas-na-linha (tabuleiro-ocupadas-na-linha tabuleiro))
	(total-ocupadas (tabuleiro-total-ocupadas tabuleiro)))
    (setf (tabuleiro-campo-jogo novo-tabuleiro)
	  (copia-array campo-jogo))
    (setf (tabuleiro-altura-colunas novo-tabuleiro)
	  (copia-array alturas))
    (setf (tabuleiro-par-pos-mais-alta novo-tabuleiro)
	  (cons linha-mais-alta coluna-mais-alta))
    (setf (tabuleiro-ocupadas-na-linha novo-tabuleiro)
	  (copia-array ocupadas-na-linha (1+ linha-mais-alta) 0))
    (setf (tabuleiro-total-ocupadas novo-tabuleiro )
	  total-ocupadas)
    novo-tabuleiro))


(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  "predicado que devolve T se a casa linha coluna esta preenchida nil caso contra'rio"
  (aref (tabuleiro-campo-jogo tabuleiro) linha coluna))


(defun tabuleiro-altura-coluna (tabuleiro coluna)
  "recebe um tabuleiro e uma coluna e devolve a linha mais alta (1-18) preenchida 0 significa nao preenchida"
  (aref (tabuleiro-altura-colunas tabuleiro) coluna))


(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "recebe um tabuleiro e uma linha e devolve T caso a linha esteja preenchida ou completa falso caso contra'rio"
  (eq (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha) +colunas+))


(defun tabuleiro-preenche! (tabuleiro linha coluna)
  "marca a posicao linha coluna ocupada"
    (and (< linha +linhas+) (< coluna +colunas+) (>= linha 0) (>= coluna 0)
	 (let ((alturas (tabuleiro-altura-colunas tabuleiro))
	       (ocupadas-na-linha (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha)))
	   (progn
	     (setf (aref (tabuleiro-campo-jogo tabuleiro) linha coluna) +ocupada+)
	     (setf (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha) (+ ocupadas-na-linha 1))
	     (incf (tabuleiro-total-ocupadas tabuleiro))
	     (if (>= linha (aref alturas coluna)) ;nova altura ->
					;se altura da coluna = linha  e' nova altura
					;se = linha+1 nao e' nova altura mas pode ser nova ultima posicao de memoria preenchida  se a coluna for mais alta
		 (progn (setf (aref alturas coluna) (+ linha 1))
			(let ((ultima-linha (car (tabuleiro-par-pos-mais-alta tabuleiro)))
			      (ultima-coluna (cdr (tabuleiro-par-pos-mais-alta tabuleiro))))
			  (when (or (and (= ultima-linha linha) ; caso linha maior que ultima linha esta coberto pelo if
					 (> coluna ultima-coluna))
				    (> linha ultima-linha))
			    (setf (tabuleiro-par-pos-mais-alta tabuleiro) (cons linha coluna))))))))))


 (defun encontra-maximo (array)
   "devolve o elemento maximo num array de numeros nao vazio"
   (let ((maximo (row-major-aref array 0)))
     (dotimes (i (array-total-size array))
       (let ((elemento (row-major-aref array i)))
 	(when (> elemento maximo) ;row-major-aref trata o array como um vector
 	  (setf maximo elemento))))
     maximo))


(defun encontra-altura (campo-jogo indice-coluna &optional (limite +linhas+))
  "encontra a altura de uma coluna com indice-coluna no array campo-jogo"
  (let ((resultado 0))
    (dotimes (i limite resultado)
      (when (aref campo-jogo i indice-coluna)
	(setf resultado (+ i 1))))))


(defun repoe-alturas! (campo-jogo alturas altura-maxima linha-removida)
  "repoe as alturas num evento remove-linha!"
  (let ((maxima 0))
    (dotimes (i +colunas+ maxima) ;actualiza alturas
      (let ((altura (aref alturas i)))
	(if (> altura (1+ linha-removida))
	    (setf (aref alturas i) (- altura 1)) ;ha' casas preenchidas por cima da linha
	    (setf (aref alturas i) (encontra-altura campo-jogo i altura-maxima)))
	(when (> altura (aref alturas maxima))
	  (setf maxima i)))))) ; pode haver vazio por baixo da linha


(defun desce-linha! (tabuleiro linha)
  "copia a linha sobre a linha anterior apagando assim a informacao que la' estava"
  (let ((campo-jogo (tabuleiro-campo-jogo tabuleiro))
	(linha-de-baixo (- linha 1)))
    (let ((ocupadas-linha-de-cima (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha)))
      (setf (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha-de-baixo) ocupadas-linha-de-cima) ;actualiza ocupadas na linha
      (dotimes (i +colunas+) ;actualiza campo-jogo
	(setf (aref campo-jogo linha-de-baixo i)
	      (aref campo-jogo linha i))))))


(defun tabuleiro-remove-linha! (tabuleiro linha)
  "remove a linha fazendo com que as linhas de cima descam uma casa"
    (let ((campo-jogo (tabuleiro-campo-jogo tabuleiro))
	  (alturas (tabuleiro-altura-colunas tabuleiro))
	  (total-ocupadas (tabuleiro-total-ocupadas tabuleiro))
	  (ocupadas-na-linha (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha))
	  (par-mais-alto (tabuleiro-par-pos-mais-alta tabuleiro))
	  (linha-mais-alta (car (tabuleiro-par-pos-mais-alta tabuleiro))))
      (setf (tabuleiro-total-ocupadas tabuleiro)
	    (- total-ocupadas ocupadas-na-linha)) ;actualiza total ocupadas
      (dotimes (i (- (+ 2 linha-mais-alta) (incf linha)))  ; linha passa a ser linha+1 (linha de cima) no ciclo linha-mais-alta  mais dois por causa do incf para nao fazer inf linha sempreem cada iteracao do cilo
	(desce-linha! tabuleiro (+ linha i)))
      (if (= linha-mais-alta 0)
	  (setf  (tabuleiro-par-pos-mais-alta tabuleiro) '(0 . 0)) ;linha mais alta nao pode ser negativa
	  (setf (car par-mais-alto)
		(- linha-mais-alta 1)))  ;actualiza linha do par posicao mais alta
      (setf (cdr par-mais-alto) (repoe-alturas! campo-jogo alturas linha-mais-alta (decf linha))))) ;chamado com a linha de cima (incf ) se a ultima linha tivesse preenchida o jogo tinha acabado


(defun tabuleiro-topo-preenchido-p (tabuleiro)
  "devolve T se houver uma peca que ocupa a linha 17"
  (eq (car (tabuleiro-par-pos-mais-alta tabuleiro)) +linha-maxima+))


;;Esta funcao pode comparar arrays com dimensoes garantidamente identicas
;;  que desta forma se encontram em memoria como um vector
;;  assim,  as coordenadas de cada elemento vao concidir
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


(defun pares-iguais-P (Par Outro)
  "Predicado Que Compara Dois Pares De Numeros E Devolve T Se Forem Identicos Nil caso contr'ario"
  (and (eq (car par) (car outro)) (eq (cdr par) (cdr outro))))


(defun tabuleiros-iguais-p (tabuleiro outro)
  "dados dois tabuleiro devolve T se forem iguais e nil caso contr'ario"
  (cond ((/= (tabuleiro-total-ocupadas tabuleiro)  (tabuleiro-total-ocupadas outro)) nil)
	((not (pares-iguais-p (tabuleiro-par-pos-mais-alta tabuleiro) (tabuleiro-par-pos-mais-alta outro))) nil)
	((not (vectores-iguais-p (tabuleiro-altura-colunas tabuleiro) (tabuleiro-altura-colunas outro))) nil)
	((not (vectores-iguais-p (tabuleiro-ocupadas-na-linha tabuleiro) (tabuleiro-ocupadas-na-linha outro))) nil)
	((not (vectores-iguais-p (tabuleiro-campo-jogo tabuleiro) (tabuleiro-campo-jogo outro))) nil)
	(t t)))


(defun tabuleiro->array (tabuleiro)
  "recebe um tabuleiro e devvolve o array de booleans correspondente"
  (let ((linha-mais-alta (car (tabuleiro-par-pos-mais-alta tabuleiro)))
	(coluna-mais-alta (cdr (tabuleiro-par-pos-mais-alta tabuleiro))))
    (copia-array (tabuleiro-campo-jogo tabuleiro) (+ (* linha-mais-alta 10) coluna-mais-alta 1) nil '(18 10))))


(defun array->tabuleiro (array)
  "devolve o tabuleiro construido a partir de um array de 18 linhas e 10 colunas"
  (let ((tabuleiro (cria-tabuleiro)))
    (dotimes (i (array-total-size array))
      (when (row-major-aref array i)
	(multiple-value-bind (linha coluna) (floor i 10)  ;floor devolve dois valores que ficam associados a linha coluna
	  (tabuleiro-preenche! tabuleiro linha coluna))))
    tabuleiro))



;;; 2.1.3 - tipo Estado

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

; a unica coisa que muda num estado inicial sao as pecas
(defun cria-estado (pecas-por-colocar)
	(make-estado :pontos 0
				:pecas-por-colocar pecas-por-colocar
				:pecas-colocadas '()
				:tabuleiro (cria-tabuleiro))
)

(defun copia-estado (estado-orig) "copia um estado"
	(let ((estado-novo
		(make-estado
			:pontos (estado-pontos estado-orig)
			:pecas-por-colocar (copy-list (estado-pecas-por-colocar estado-orig))
			:pecas-colocadas (copy-list (estado-pecas-colocadas estado-orig))
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
	        (tabuleiro-topo-preenchido-p (estado-tabuleiro e1))
	        (null (estado-pecas-por-colocar e1))
	)
)

;;;; 2.1.4 - tipo Problema
(defstruct problema
	estado-inicial solucao accoes resultado custo-caminho)

;auxiliar
(defun cria-problema (estado funcao-custo-caminho)
	(make-problema :estado-inicial estado
					:solucao #'solucao
					:accoes #'accoes
					:resultado #'resultado
					:custo-caminho funcao-custo-caminho)
)

;;;; 2.2.1

; um estado e' solucao quando nao ha mais pecas por colocar
; e quando o topo nao esta preenchido !!
(defun solucao (estado)
	(and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
		(null (estado-pecas-por-colocar estado)))
)

; 1. se (estado-final-p estado), retornar () (nada a fazer)
; 2. tirar a primeira peca do estado
;	2.1. para cada peca, obter possiveis rotacoes
;	2.2. para cada rotacao, obter posicoes possiveis
;	2.3. gerar accao com par peca e posicao e meter na lista
; 3. devolver lista
(defun accoes (estado)
	; 1
	(if (estado-final-p estado)
		nil ;t
		; 2
		(let* ((primeira-peca (first (estado-pecas-por-colocar estado)))
			(rotacoes (rotacoes-peca primeira-peca))
			(accoes-possiveis '()))
			;2.1
			(loop for peca-rodada in rotacoes
			do (let* ((largura-peca (largura-peca peca-rodada))
					; NOTA: nao se esta a verificar se uma peca e' mais larga que o tabuleiro
					; segundo o enunciado e as pecas dadas, isso e' impossivel
					(max-col (- +colunas+ largura-peca))
					(lista-auxiliar '()))
				;2.2
				(loop for posicao upto max-col
				do (setq lista-auxiliar (nconc lista-auxiliar (list(cria-accao posicao peca-rodada))));2.3
				)
				(setq accoes-possiveis
						(nconc accoes-possiveis lista-auxiliar))
			))
		accoes-possiveis) ;3
	)
)

; 1. fazer copia do estado velho -> mudar sempre campos do estado novo !!
	; 2. meter peca no tabuleiro novo
	; 3. se houver linhas completas: verificar quais sao
	;		Se a pecca e colocada num intervalo de colunas, basta verificar a altura dessas
	;		A unica altura relevante e a minima de todas essas: se h(0)= 2 e h(1) = 5 -> linhas 3 a 5 nao podem estar preenchidas
	;		Tambem se pode considerar o intervalo relevante tendo em conta a altura da peca:
	;		se a peca tem altura=3, as unicas linhas que podem ter sido preenchidas sao h(colunas) - h(peca)
	; 4. calcular pontuacao
	; 5. eliminar linhas preenchidas
	; 6. aumentar lista de pecas-colucadas
	; 7. reduzir lista de pecas-por-colocar
	; 8. retornar estado-novo
(defun resultado (estado accao)
	;1
	(let* ((estado-novo (copia-estado estado))
			(tab-novo (estado-tabuleiro estado-novo))
			(peca (accao-peca accao))
			(coluna (accao-coluna accao)))
		; 2
		(tabuleiro-larga-peca! tab-novo peca coluna)

		(let* ((coluna-final (+ coluna (largura-peca peca)))
			(todas-alturas (tabuleiro-altura-colunas tab-novo))
			(alturas (subseq todas-alturas coluna coluna-final))
			(altura-max-possivel (reduce #'min alturas))
			(altura-min-possivel (- altura-max-possivel (altura-peca peca)))
			(linhas-preenchidas '())
			(pontuacao-obtida 0))

			;3
			; FIXME: este max e' um hack temporario para evitar indices negativos
			(loop for l from (max 0 altura-min-possivel) upto (1- altura-max-possivel)
			do (if (tabuleiro-linha-completa-p tab-novo l)
				(setf linhas-preenchidas (cons l linhas-preenchidas))
			))

			;4
			(setf pontuacao-obtida (aref +PONTUACAO-LINHAS+
				(length linhas-preenchidas)))
			(setf (estado-pontos estado-novo) (+ pontuacao-obtida
				(estado-pontos estado-novo)))
			;5
			(loop for l in linhas-preenchidas
			do (tabuleiro-remove-linha! tab-novo l))
		)
		; 6
		(setf (estado-pecas-colocadas estado-novo)
			(cons (first (estado-pecas-por-colocar estado)) (estado-pecas-colocadas estado-novo)))
		; 7
		(setf (estado-pecas-por-colocar estado-novo) (rest (estado-pecas-por-colocar estado-novo)))
	; 8
	estado-novo)
)

; funcoes auxiliares para resolver problemas

(defun tabuleiro-preenche-peca! (tab peca linha coluna)
	"preenche-peca! recebe uma linha e coluna
	e uma funcao auxiliar a funcao jogada, que so recebe uma coluna"

	(let ((largura (largura-peca peca))
		(altura (altura-peca peca)))

		(loop for l upto (- altura 1)
			do (loop for c upto (- largura 1)
				; so preencher se posicao na peca for T
				do (if (aref peca l c)
					(tabuleiro-preenche! tab (+ linha l) (+ coluna c)))))
))

(defun tabuleiro-larga-peca! (tab peca coluna)
	"larga-peca! recebe uma peca e coluna e faz uma jogada de tetris"

	(let* ((base (peca-base peca))
		(largura (largura-peca peca))
		(alturas-tab (tabuleiro-altura-colunas tab))
		(alturas-ajustadas (subseq (copia-array alturas-tab)
			coluna (+ coluna largura)))
		(max-altura 0))

		(loop for c upto (1- largura)
			do (setf (aref alturas-ajustadas c)
				(- (aref alturas-ajustadas c) (aref base c))))

		(setq max-altura (encontra-maximo alturas-ajustadas))
		; ajustar linhas consoante a forma da peca


		(tabuleiro-preenche-peca! tab peca max-altura coluna)

	tab
))


(defun qualidade (estado) "retorna o simetrico do numero de pontos"

	(- (estado-pontos estado))
)


(defun custo-oportunidade (estado) "retorna maximo-pontos-possivel - pontos"

	(let ((max-pontos 0))
		(dolist (peca (estado-pecas-colocadas estado) max-pontos)
			(incf max-pontos (pontos-por-peca peca))
		)
		(- max-pontos (estado-pontos estado))
	)
)



; FIXME: o load ficou neste sitio estranho porque aparentemente:
; - se chama as funcoes x e y do "tetris.lisp", tem que ser depois de elas serem definidas
; - se chamamos as funcoes m e n do "utils.fas", temos que fazer load do ficheiro primeiro

; FIXME: o "utils.fas" usa uma funcao "formulacao-problema" que nao esta definida. what do?
 (load "utils.fas")
;(load (compile-file "utils.lisp"))


; 0.0.0 - Pecas (funcoes e variaveis auxiliares)
(defconstant peca-i (list peca-i0 peca-i1))
(defconstant peca-l (list peca-l0 peca-l1 peca-l2 peca-l3))
(defconstant peca-j (list peca-j0 peca-j1 peca-j2 peca-j3))
(defconstant peca-o (list peca-o0))
(defconstant peca-s (list peca-s0 peca-s1))
(defconstant peca-z (list peca-z0 peca-z1))
(defconstant peca-t (list peca-t0 peca-t1 peca-t2 peca-t3))

; precisamos de saber a pontuacao maxima para cada peca

(defconstant +pontos-peca-i+ 800)
(defconstant +pontos-peca-j+ 500)
(defconstant +pontos-peca-l+ 500)
(defconstant +pontos-peca-s+ 300)
(defconstant +pontos-peca-z+ 300)
(defconstant +pontos-peca-t+ 300)
(defconstant +pontos-peca-o+ 300)

(defun pontos-por-peca (peca) "pontos maximos que a peca permite ganhar"
	(cond
		((equal peca 'i) +pontos-peca-i+)
		((equal peca 'j) +pontos-peca-j+)
		((equal peca 'l) +pontos-peca-l+)
		((equal peca 's) +pontos-peca-s+)
		((equal peca 'z) +pontos-peca-z+)
		((equal peca 't) +pontos-peca-t+)
		((equal peca 'o) +pontos-peca-o+)
	)
)

(defun peca-base (p)
; FIXME: como as pecas estao bem definidas, e possivel tornar isto uma serie de constantes
	"e importante conhecer a base da peca para saber onde a encaixar
	enquanto um quadrado e' trivial - encaixa na altura mais alta que encontrar
	um l deitado nao - pode encaixar no meio de buracos

	exemplos:
		linhas-base (peca-i0): (0 0 0 0)
		linhas-base (peca-s0): (0 0 1)"

	(let* ((largura (largura-peca p))
		(altura (altura-peca p))
		; o valor 100 e' usado para calcular a altura da base, ver comentarios abaixo
		(alturas-base (make-array largura :initial-element 100)))

		(loop for l upto (- altura 1)
		do (loop for c upto (- largura 1)
			do (if (aref p l c)
					(setf (aref alturas-base c)
						; a base e so o primeiro quadrado preenchido
						; com min, asseguramos que nao volta a contar quadrados
						; requisitos: o valor inicial deve ser bem alto devido ao min: entao escolheu-se 100
						; TODO: isto nao e' muito eficiente:
						; uma melhoria seria fazer isto uma unica vez por execucao ja que as pecas nunca mudam
						(min l (aref alturas-base c))))))

		alturas-base
))

(defun altura-peca (p)
	(array-dimension p 0)
)

(defun largura-peca (p)
	(array-dimension p 1)
)

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

;(load "utils.lisp")
