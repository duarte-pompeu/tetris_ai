; CARACTERES NAO ASCII: [^\x00-\x7F]+
; basta pesquisarem com regex, caso o vosso editor suporte
; em Linux, nao funciona com cat | grep, nao sei pq

; invocar (rl) faz reload do ficheiro
;; (defun rl ()
;; 	(load "tetris.lisp")
;; 	(load "tests.lisp")
;; )


;; ; meter T para fazer print do mylog, nil para nao fazer;
;; ; TODO: meter a nil antes de submeter no mooshak
;; (defvar  *DEBUG-MODE* T)

;; (defun mylog (message)
;; 	(if *DEBUG-MODE*
;; 		(format t "~a ~%" message)
;; 	)
;; )

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
  (let ((campo (make-array (list +linhas+ +colunas+) :initial-element nil))
	(alturas (make-array +colunas+ :initial-element 0))
	(altura-maxima (cons 0 0))
	(ocupadas-na-linha (make-array +linhas+ :initial-element 0))
	(casas-preenchidas 0))
	(make-tabuleiro :campo-jogo campo
			:altura-colunas alturas 
			:par-pos-mais-alta altura-maxima
			:ocupadas-na-linha ocupadas-na-linha
			:total-ocupadas casas-preenchidas)))


(defun copia-array (array &optional (limite (array-total-size array)) init-element)
  "recebe um array e devolve uma co'pia do array sem alterar o original"
  (let* ((dims (array-dimensions array))
	 (new-array (make-array dims :initial-element init-element)))
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
  (let ((alturas (tabuleiro-altura-colunas tabuleiro))
	(ocupadas-na-linha (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha)))
    (and (< linha +linhas+) (< coluna +colunas+) (>= linha 0) (>= coluna 0)
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
    

;; (defun encontra-maximo (array)
;;   "devolve o elemento maximo num array de numeros nao vazio"
;;   (let ((maximo (row-major-aref array 0)))
;;     (dotimes (i (array-total-size array))
;;       (let ((elemento (row-major-aref array i)))	
;; 	(when (> elemento maximo) ;row-major-aref trata o array como um vector
;; 	  (setf maximo elemento))))
;;     maximo))


(defun encontra-altura (campo-jogo indice-coluna &optional (limite +linhas+))
  "encontra a altura de uma coluna com indice-coluna no array campo-jogo"
  (let ((resultado 0))
    (dotimes (i limite)
      (when (aref campo-jogo i indice-coluna)
	(setf resultado (+ i 1))))
    resultado))


(defun repoe-alturas! (campo-jogo alturas altura-maxima linha-removida)
  "repoe as alturas num evento remove-linha!"
  (dotimes (i +colunas+) ;actualiza alturas
    (let ((altura (aref alturas i)))
      (if (> altura (1+ linha-removida))
	  (setf (aref alturas i) (- altura 1)) ;ha' casas preenchidas por cima da linha
	  (setf (aref alturas i) (encontra-altura campo-jogo i altura-maxima)))))) ; pode haver vazio por baixo da linha


(defun desce-linha! (tabuleiro linha)
  "copia a linha sobre a linha anterior apagando assim a informacao que la' estava"
  (let ((campo-jogo (tabuleiro-campo-jogo tabuleiro))
	(linha-de-baixo (- linha 1)))
    (let ((ocupadas-linha-de-cima (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha)))
      (setf (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha-de-baixo) ocupadas-linha-de-cima) ;actualiza ocupadas na linha
      (dotimes (i +colunas+) ;actualixa campo-jogo
	(setf (aref campo-jogo linha-de-baixo i)
	      (aref campo-jogo linha i))))))


(defun tabuleiro-remove-linha! (tabuleiro linha)
  "remove a linha fazendo com que as linhas de cima descam uma casa"
  (let ((campo-jogo (tabuleiro-campo-jogo tabuleiro))
	(alturas (tabuleiro-altura-colunas tabuleiro))
	(total-ocupadas (tabuleiro-total-ocupadas tabuleiro))
	(ocupadas-na-linha (aref (tabuleiro-ocupadas-na-linha tabuleiro) linha))
	(linha-mais-alta (car (tabuleiro-par-pos-mais-alta tabuleiro))))
    (setf (tabuleiro-total-ocupadas tabuleiro)
	  (- total-ocupadas ocupadas-na-linha)) ;actualiza total ocupadas
    (setf (car (tabuleiro-par-pos-mais-alta tabuleiro))
	 (- linha-mais-alta 1))  ;actualiza linha do par posicao mais alta
    (dotimes (i (- (+ 2 linha-mais-alta) (incf linha)))  ; linha passa a ser linha+1 (linha de cima) no ciclo linha-mais-alta  mais dois por causa do incf para nao fazer inf linha sempre no cilo
      (desce-linha! tabuleiro (+ linha i)))
    (repoe-alturas! campo-jogo alturas linha-mais-alta (decf linha)))) ;chamado com a linha de cima (incf ) se a ultima linha tivesse preenchida o jogo tinha acabado 


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



;;; 2.1.3 - tipo Estado

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
	        (tabuleiro-topo-preenchido-p (estado-tabuleiro e1))
		(if (null (estado-pecas-por-colocar e1)) T nil)
	)
)



; FIXME: problema ainda nao foi nada testado
(defstruct problema
	(estado-inicial)

	; um estado e' solucao quando nao ha mais pecas por colocar
	(solucao (function (lambda (estado)
		(not (null (estado-pecas-por-colocar estado))))))

	; 1. se (estado-final-p estado), retornar () (nada a fazer)
	; 2. tirar a primeira peca do estado
	;	2.1. para cada peca, obter possiveis rotacoes
	;	2.2. para cada rotacao, obter posicoes possiveis
	;	2.3. gerar accao com par peca e posicao e meter na lista
	; 3. devolver lista
	(accoes (function (lambda (estado)
		; 1
		(if (estado-final-p estado)
			t
			; 2
			(let* ((primeira-peca (first (estado-pecas-por-colocar estado)))
				(rotacoes (rotacoes-peca primeira-peca))
				(accoes-possiveis '()))
				;2.1
				(loop for peca-rodada in rotacoes
				do (let* ((largura-peca (largura-peca peca-rodada))
						; NOTA: nao se esta a verificar se uma peca e' mais larga que o tabuleiro
						; segundo o enunciado e as pecas dadas, isso e' impossivel
						(max-col (- +colunas+ largura-peca)))
					;2.2
					(loop for posicao upto max-col
					do (setq accoes-possiveis
							(cons (cria-accao posicao peca-rodada) accoes-possiveis)) ;2.3
					)
				))
			accoes-possiveis) ;3
		)
	)))

	; 1. fazer copia do estado velho
	; 2. calcular a linha onde a peca vai encaixar (so sabemos as colunas)
	; 3. preencher tabuleiro do **estado novo**
	; 4. se houver linhas completas: elimina-las e calcular nova pontuacao
	; 5. aumentar lista de pecas-colucadas
	; 6. reduzir lista de pecas-por-colocar
	; 7. retornar estado-novo
	;~ (resultado (function (lambda (estado accao)
		;~ ;1
		;~ (let* ((estado-novo (copia-estado estado))
			;~ (largura (largura-peca (accao-peca accao)))
			;~ (coluna-inicial (accao-coluna accao))
			;~ (coluna-final (+ coluna-inicial (- largura 1)))
			;~ ; 2
			;~ (altura-colunas (tabuleiro-altura-colunas (estado-tabuleiro estado)))
			;~ ; TODO: (subseq array i j) retorna array[i -> j-1]
			;~ ; > (subseq #(0 1 2 3) 0 2)
			;~ ; #(0 1)
			;~ ; ver se isto esta a seleccionar a subarray que queremos

			;~ ; FIXME: tirar comentario (esta assim para compilar enquanto var nao e usada
			;~ ;(linha-mais-alta (max-array (subseq altura-colunas coluna-inicial coluna-final)))
			;~ )

			;~ ; 3

			;~ ; 4

			;~ ; 5
			;~ (setf (estado-pecas-colocadas estado-novo)
				;~ (cons (first (estado-pecas-por-colocar estado)) (estado-pecas-colocadas estado-novo)))
			;~ ; 6
			;~ (setf (estado-pecas-por-colocar estado-novo) (rest (estado-pecas-por-colocar estado-novo)))
		;~ ; 7
		;~ estado-novo)
	;~ )))

	; usar diferencas de pontuacoes
	; algum caso especial quando o novo estado leva a que o jogo se perca?
	; talvez nao, ja que a arvore de decisoes nesse caso acaba sem se chegar a objectivo
	; FIXME: como aceder a 2 estados se o lambda so aceita um ???
	(custo-caminho (function (lambda (estado) (or "placeholder" estado))))
)

; FIXME: o load ficou neste sitio estranho porque aparentemente:
; - se chama as funcoes x e y do "tetris.lisp", tem que ser depois de elas serem definidas
; - se chamamos as funcoes m e n do "utils.fas", temos que fazer load do ficheiro primeiro

; FIXME: o "utils.fas" usa uma funcao "formulacao-problema" que nao esta definida. what do?
 (load "utils.fas")

; 0.0.0 - Pecas (funcoes e variaveis auxiliares)
(defconstant peca-i (list peca-i0 peca-i1))
(defconstant peca-l (list peca-l0 peca-l1 peca-l2 peca-l3))
(defconstant peca-j (list peca-j0 peca-j1 peca-j2 peca-j3))
(defconstant peca-o (list peca-o0))
(defconstant peca-s (list peca-s0 peca-s1))
(defconstant peca-z (list peca-z0 peca-z1))
(defconstant peca-t (list peca-t0 peca-t1 peca-t2 peca-t3))

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

