; Grupo 64:  	73008 	Duarte Pinto Pompeu	 	76765 	Antonio Joaquim Sousa Gloria	 	76338 	Marcio Antonio Filipe dos Santos

; CARACTERES NAO ASCII: [^\x00-\x7F]+

(defun rl ()
	"faz load a varios ficheiros do projecto"

 	(load "tetris.lisp")
	(load "tests.lisp")
	(load "testes-tab.lisp")
	(load "tests-problema.lisp")
	(load "testa-pp.lisp")
	(load "nos.lisp")
	(load "gerador.lisp")
	(load "tabuleiros.lisp")
	(load "hangman.lisp")
)

(defun rlalt ()
	(load "tab_alt.lisp")
)


(defvar  *DEBUG-MODE* T)

(defun mylog (message)
	"imprime mensagem caso DEBUG-MODE seja T"

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
(defconstant +casas+ 180
	"o tabuleiro tem 18*10=180 casas")
(defconstant +livre+ nil
   "o valor nil simboliza uma casa livre")
(defconstant +ocupada+ T
   "o valor T simboliza uma casa ocupada")

(defconstant +PONTUACAO-LINHAS+ '#(0 100 300 500 800))

;;; GLOBAIS
(defvar +LINHAS_TAB_ACTUAL+ 0)
(defvar +COLUNAS_TAB_ACTUAL+ 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.1.1 - TIPO ACCAO
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cria-accao (inteiro array)
	(cons inteiro array)
)


(defun accao-coluna (accao)
	(car accao)
)


(defun accao-peca (accao)
	(cdr accao)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.1.2 - TIPO TABULEIRO
;
; FUNCOES PUBLICAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
			    (setf (tabuleiro-par-pos-mais-alta tabuleiro) (cons linha coluna))))))))) T)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SECCAO 2.1.1 - TIPO TABULEIRO
;
; FUNCOES INTERNAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun copia-array (array &optional (limite (array-total-size array)) init-element (dims (array-dimensions array)))
  "recebe um array e devolve uma co'pia do array sem alterar o original"

  (let ((new-array (make-array dims :initial-element init-element)))
    (dotimes (i limite)
      (setf (row-major-aref new-array i) ; o array e' uma zona continua de memoria ordenada por linhas
	    (row-major-aref array i)))
    new-array)) ;return

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; funcoes auxiliares que usam a API do tabuleiro
; necessarias para generalizar problemas (adeus optimizacoes)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gen-n-colunas (tabuleiro-generico)
	(loop for c upto 1000
		do (let ((current-value (ignore-errors (tabuleiro-altura-coluna tabuleiro-generico c))))
			(if (not (and (numberp current-value) (>= current-value 0)))
				(loop-finish)))
	finally (return c))
)


(defun gen-h-colunas (tabuleiro-generico)
	"obtem altura-max, pois os nossos campos optimizads nao se aplicam para problemas genericos"

	(let ((n-colunas 0)
		(arr-colunas nil))

		; nao existe API para contar numero de colunas (sem ser atraves de tabuleiro-> array, uma operacao demasiado pesada)
		; este loop conta numero de colunas atraves de um magnifico hack - ignore-errors
		(loop for c upto 1000
		do (let ((current-value (ignore-errors (tabuleiro-altura-coluna tabuleiro-generico c))))
			(if (numberp current-value)
				(incf n-colunas)
				(loop-finish))))

		; cria array com tamanho certo
		(setf arr-colunas (make-array n-colunas))

		; mete valores na array
		(loop for c upto (1- n-colunas)
		do (setf (aref arr-colunas c)
				(tabuleiro-altura-coluna tabuleiro-generico c)))

	arr-colunas
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.1.3 - TIPO ESTADO
;
; FUNCOES PUBLICAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)


(defun cria-estado (pecas-por-colocar)
"Funcao que cria estados com tabuleiros vazios."

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SECCAO 2.1.4 - TIPO PROBLEMA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problema
	estado-inicial solucao accoes resultado custo-caminho)

(defun cria-problema (estado funcao-custo-caminho)
"Criador de problemas auxiliar: as unicas coisas que variam sao o estado inicial e as funcoes de custo de caminho"

	(make-problema :estado-inicial estado
					:solucao #'solucao
					:accoes #'accoes
					:resultado #'resultado
					:custo-caminho funcao-custo-caminho)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.2.1 - FUNCOES DO PROBLEMA DE PROCURA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun solucao (estado)
"Um estado e' solucao quando nao ha mais pecas por colocar e o topo nao esta preenchido."

	(and (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
		(null (estado-pecas-por-colocar estado)))
)


(defun accoes (estado)
"Retorna a lista de accoes possiveis para um dado estado:
	- combinacao de rotacoes e posicoes para a peca seguinte
	- nil quando nao ha mais pecas
	- nil quando o topo esta preenchido

Descricao do algoritmo:
	1. se (estado-final-p estado), retornar () (nada a fazer)
	2. tirar a primeira peca do estado
		2.1. para cada peca, obter possiveis rotacoes
		2.2. para cada rotacao, obter posicoes possiveis
		2.3. gerar accao com par peca e posicao e meter na lista
	3. devolver lista"

	; 1
	(if (estado-final-p estado)
		nil
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
				(loop for posicao from max-col downto 0
				; as insercoes sao feitas no inicio da lista, entao o loop vai de coluna-maxima ate' 0
				do (setq accoes-possiveis (cons (cria-accao posicao peca-rodada) accoes-possiveis)) ;2.3
				)
			))
		accoes-possiveis) ;3
	)
)


(defun resultado (estado accao)
"Aplica uma accao a um estado e retorna o estado resultante.

Descricao do algoritmo:
1. fazer copia do estado velho -> mudar sempre campos do estado novo !!
	2. meter peca no tabuleiro novo
	3. se houver linhas completas: verificar quais sao
			Se a pecca e colocada num intervalo de colunas, basta verificar a altura dessas
			A unica altura relevante e a minima de todas essas: se h(0)= 2 e h(1) = 5 -> linhas 3 a 5 nao podem estar preenchidas
			Tambem se pode considerar o intervalo relevante tendo em conta a altura da peca:
			se a peca tem altura=3, as unicas linhas que podem ter sido preenchidas sao h(colunas) - h(peca)
	4. calcular pontuacao
	5. eliminar linhas preenchidas
	6. aumentar lista de pecas-colucadas
	7. reduzir lista de pecas-por-colocar
	8. retornar estado-novo"

	;1
	(let* ((estado-novo (copia-estado estado))
			(tab-novo (estado-tabuleiro estado-novo))
			(peca (accao-peca accao))
			(coluna (accao-coluna accao)))
		; 2
		(tabuleiro-larga-peca! tab-novo peca coluna)

		(if (tabuleiro-topo-preenchido-p tab-novo)
			(return-from resultado estado-novo))

		(if (tabuleiro-topo-preenchido-p tab-novo)
			(return-from resultado estado-novo))

		(let* ((coluna-final (+ coluna (largura-peca peca)))
			(todas-alturas (gen-h-colunas tab-novo))
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


(defun qualidade (estado) "Retorna o simetrico do numero de pontos"

	(- (estado-pontos estado))
)


(defun custo-oportunidade (estado) "Retorna maximo-pontos-possivel - pontos"

	(let ((max-pontos 0))
		(dolist (peca (estado-pecas-colocadas estado) max-pontos)
			(incf max-pontos (pontos-por-peca peca))
		)
		(- max-pontos (estado-pontos estado))
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.2.1 - FUNCOES AUXILIARES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tabuleiro-preenche-peca! (tab peca linha coluna)
"Funcao auxiliar que transpoe uma peca para um tabuleiro.
Nao faz nenhuma verificao em especial - pode preencher por cima de outras pecas,
ou colocar a peca numa posicao impossivel, por exemplo, a 'levitar' sem ter pecas por baixo.
Essas verificoes devem ser feitas por outras funcoes, que tenham as regras do jogo em conta."

	(let ((largura (largura-peca peca))
		(altura (altura-peca peca)))

		(loop for l upto (- altura 1)
			do (loop for c upto (- largura 1)
				; so preencher se posicao da peca for T
				; caso contrario, vai apagar pecas do tabuleiro para meter o nil da peca
				do (if (aref peca l c)
					(tabuleiro-preenche! tab (+ linha l) (+ coluna c)))))
))

(defun tabuleiro-larga-peca! (tab peca coluna)
"Faz uma jogada de tetris - dados um tabuleiro, peca e coluna, coloca a peca no sitio correcto."

	(let* ((base (peca-base peca))
		(largura (largura-peca peca))
		(alturas-tab (gen-h-colunas tab))
		(alturas-ajustadas (subseq (copia-array alturas-tab)
			coluna (+ coluna largura)))
		(max-altura 0))

		(loop for c upto (1- largura)
			do (setf (aref alturas-ajustadas c)
				(- (aref alturas-ajustadas c) (aref base c))))

		; ajustar linhas consoante a forma da peca
		(setq max-altura (encontra-maximo alturas-ajustadas))

		(tabuleiro-preenche-peca! tab peca max-altura coluna)

	tab
))

; FIXME: o load ficou neste sitio estranho porque aparentemente:
; - se chama as funcoes x e y do "tetris.lisp", tem que ser depois de elas serem definidas
; - se chamamos as funcoes m e n do "utils.fas", temos que fazer load do ficheiro primeiro
 (load "utils.fas")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PECAS - FUNCOES E VARIAVEIS AUXILIARES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; rotacoes de cada peca
(defconstant peca-i (list peca-i1 peca-i0))
(defconstant peca-l (list peca-l3 peca-l2 peca-l1 peca-l0))
(defconstant peca-j (list peca-j3 peca-j2 peca-j1 peca-j0))
(defconstant peca-o (list peca-o0))
(defconstant peca-s (list peca-s1 peca-s0))
(defconstant peca-z (list peca-z1 peca-z0))
(defconstant peca-t (list peca-t3 peca-t2 peca-t1 peca-t0))

; precisamos de saber a pontuacao maxima para cada peca
(defconstant +pontos-peca-i+ 800)
(defconstant +pontos-peca-j+ 500)
(defconstant +pontos-peca-l+ 500)
(defconstant +pontos-peca-s+ 300)
(defconstant +pontos-peca-z+ 300)
(defconstant +pontos-peca-t+ 300)
(defconstant +pontos-peca-o+ 300)

(defun pontos-por-peca (peca)
"Pontos maximos que a peca permite ganhar."

	(cond
		((equal peca 'i) +pontos-peca-i+)
		((equal peca 'j) +pontos-peca-j+)
		((equal peca 'l) +pontos-peca-l+)
		((equal peca 's) +pontos-peca-s+)
		((equal peca 'z) +pontos-peca-z+)
		((equal peca 't) +pontos-peca-t+)
		((equal peca 'o) +pontos-peca-o+)
))


(defun peca-base (p)
; TODO: como as pecas estao bem definidas, e possivel tornar isto uma serie de constantes
"E' importante conhecer a base da peca para saber onde a encaixar
enquanto que um quadrado e' trivial - encaixa na altura mais alta por baixo dele
um l deitado nao - pode encaixar no meio de buracos

esta funcao ajuda outras funcoes a corrigirem a altura a' qual uma peca largada deve ficar
dependendo da forma da forma da sua base e das pecas que estao por debaixo dela

exemplos:
	linhas-base (peca-i0): (0 0 0 0)
	linhas-base (peca-s0): (0 0 1)"

	(let* ((largura (largura-peca p))
		(altura (altura-peca p))
		; o valor 100 e' um valor alto arbitrario
		(alturas-base (make-array largura :initial-element 100)))

		(loop for l upto (- altura 1)
		do (loop for c upto (- largura 1)
			do (if (aref p l c)
					(setf (aref alturas-base c)
						; a base e so o primeiro quadrado preenchido
						; com min, asseguramos que repete a contagem de quadrados
						; requisitos: o valor inicial deve ser bem alto devido ao min: entao escolheu-se 100

						;FIXME: talvez se possa usar um break no loop mais de dentro
						(min l (aref alturas-base c))))))
		alturas-base
))


(defun altura-peca (p)
"Calcula a altura de uma peca de forma mais abstracta."
	(array-dimension p 0))


(defun largura-peca (p)
"Calcula a largura de uma peca de forma mais abstracta."
	(array-dimension p 1))


(defun rotacoes-peca (p)
"Devolve uma lista de rotacoes possiveis para cada peca."
	(cond
		((equal p 'i) peca-i)
		((equal p 'l) peca-l)
		((equal p 'j) peca-j)
		((equal p 'o) peca-o)
		((equal p 's) peca-s)
		((equal p 'z) peca-z)
		((equal p 't) peca-t))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NOS - ESTRUTURA E FUNCOES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct no
	estado no-pai operador profundidade custo-caminho funcao-h funcao-f
)


(defun expande-no (no-pai problema heuristica)
	(let* ((estado-inicial (no-estado no-pai))
		; reverse porque queremos tirar elemento de uma lista, transformalo num no e metelo numa nova lista sem alterar a ordem
		(accoes (reverse (funcall (problema-accoes problema) estado-inicial)))
		(profundidade (1+ (no-profundidade no-pai)))
		(lista-nos nil))

		(loop for accao in accoes
		do (let* ((estado-resultante (funcall (problema-resultado problema) estado-inicial accao))

			;FIXME: tem que receber a funcao qualidade como argumento
			(custo-caminho (funcall (problema-custo-caminho problema) estado-resultante))
			(valor-h (funcall heuristica estado-resultante))
			(valor-f (+ custo-caminho valor-h))
			(no-filho (make-no :estado estado-resultante :no-pai no-pai :operador accao :profundidade profundidade :custo-caminho custo-caminho :funcao-h valor-h :funcao-f valor-f)))

			(push no-filho lista-nos)))
		#|; debug only
		(dolist (no lista-nos)
			(desenha-estado (no-estado no) (no-operador no))
			(print (no-estado no))
			(print (solucao (no-estado no))) ;e solucao? o problema esta aqui! mas o solucao parece-me bem, o tabuleiro-topo-preenchido tambem, deve ser no resultado!!!
			(read-char)
		)
		; end of debug
		|#
		lista-nos
))


(defun nos->accoes (no-objectivo)
	(if (null no-objectivo)
		(return-from nos->accoes nil))

	(let ((no-actual no-objectivo)
		(accoes nil))

		(loop while (no-no-pai no-actual)
		do (progn
			(push (no-operador no-actual) accoes))
			(setf no-actual (no-no-pai no-actual))
		)

	accoes
))


(defun no-menor-f (n1 n2)
	"Devolve T se o valor de f de n1 for menor que o de n2"

	(<= (no-funcao-f n1) (no-funcao-f n2))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FILAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-queue (&rest args)
	"Faz uma fila com os elementos de 'args'"

	args
)


(defun empty (queue)
	"Verifica se fila esta' vazia"

	(and (listp queue)
		(null queue))
)


(defun enqueue-front (nos-actuais nos-expandidos)
	"Adiciona novos nos a frente dos nos-actuais.
	Nao tem em atencao o custo de caminho."

	;~ (let ((nos-a-adicionar (reverse nos-expandidos)))
	; alteracao segundo instrucoes do professor
	(let ((nos-a-adicionar nos-expandidos))

		(loop while (not (null nos-a-adicionar))
		do (push (pop nos-a-adicionar) nos-actuais))

	nos-actuais
))


(defun enqueue-by-value (nos-actuais nos-novos funcao-avaliacao)
	"Adiciona os novos nos e mantem a lista ordenada"

	(let* ((todos (nconc (reverse nos-actuais) nos-novos)))

		(stable-sort todos #'<= :key funcao-avaliacao)
		
		todos
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.2.2 - PROCURAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun general-search (problema queuing-fn heuristica)
	"Executa uma pesquisa generica.
	 Pode ser utilizada por varios algoritmos de pesquisa, que apenas tem que fornecer uma queuing-function adequada.
	 Tenta implementar rigorosamente o pseudo-codigo do manual da disciplina."
	; os comentarios em ingles sao retirados do pseudo-codigo do manual, para melhor referencia

	; nodes <- MAKE-QUEUE (MAKE-NODE (INITIAL-STATE [problem]))
	(let* ((estado-inicial (problema-estado-inicial problema))
		(no-inicial (make-no :estado estado-inicial :no-pai nil :operador nil :profundidade 0 :custo-caminho 0 :funcao-h 0 :funcao-f 0))
		(nos (make-queue no-inicial)))

	(loop while T
	do (progn

		; if nodes is an empty list: return failure
		(if (empty nos)
			(return-from general-search nil))

		; node <- remove-front (nodes)
		(let ((no (pop nos)))

		; if goal-test(problem) applied to state(node) succeeds: return node
		(if (funcall (problema-solucao problema) (no-estado no))
			(return-from general-search no))

		; nodes <- queuing-fn (nodes, expand (node, operators(problem)))
		(setf nos (funcall queuing-fn nos (expande-no no problema heuristica))))
	))
))


; exemplo de utilizacao
; > (procura-pp (cria-problema (cria-estado '(o o)) nil))
(defun procura-pp (problema)
	"Utiliza general-search e enqueue-front (funcao de enqueue em que nos expandidos vao para o inicio da fila)
	para efectar uma procura em profundidade primeiro."
	(nos->accoes
		(general-search problema (function enqueue-front) #'sem-heuristica))
)


;; best first search
(defun best-first-search (problema funcao-avaliacao heuristica)
	"algoritmo generico para as procuras melhor-primeiro"

	(general-search problema
					#'(lambda (nos-actuais nos-novos) (enqueue-by-value nos-actuais nos-novos funcao-avaliacao))
					heuristica
	)
)


;; procura-A*
(defun procura-A* (problema heuristica)
	"ordenacao por valor de f = g + h"

	(nos->accoes (best-first-search problema #'no-funcao-f heuristica))
)

;; procura-best
(defun procura-best (array lista-pecas)
	"implementada como uma procura A* com heuristica ..."
	(let* ((tabuleiro (array->tabuleiro array))
		   (estado (make-estado :pontos 0 :pecas-por-colocar lista-pecas :pecas-colocadas nil :tabuleiro tabuleiro))
		   (problema (make-problema :estado-inicial estado :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'custo-oportunidade)))

		;(procura-A* problema #'heuristica-dif-colunas)
		
		; procura A* com heuristica-casas-ocupadas
		(procura-A* problema #'heuristica-casas-ocupadas)
		
		; procura A* com heuristica-buracos
		;(procura-A* problema #'heuristica-buracos)
		
		; procura A* com heuristica-altos-e-baixos
		;(procura-A* problema #'heuristica-altos-e-baixos)
		
		; procura A* com heuristica-best
		;(procura-A* problema #'heuristica-best)
	)
)

;; heuristicas ;;

(defun heuristica-dif-colunas (estado)
	"calcula a diferenca entre a coluna mais alta e a coluna mais baixa"

	(let* ((tabuleiro (estado-tabuleiro estado))
		   (coluna-mais-alta (+ 1 (car (tabuleiro-par-pos-mais-alta tabuleiro)))) ; + 1 pq altura coluna esta' incrementada em 1
		   (coluna-mais-baixa +linhas+)
		   (colunas (tabuleiro-altura-colunas tabuleiro))
		   (factor 100)
		  )

		; se o topo estiver preenchido devolve o maior valor possivel da heuristica (nao queremos este estado!)
		(when (tabuleiro-topo-preenchido-p tabuleiro)
			(return-from heuristica-dif-colunas (* factor +linhas+))
		)
	
		; achar coluna mais baixa  
		(dotimes (col +colunas+)
			
			(let* ((altura (aref colunas col)))
			
				(when (< altura coluna-mais-baixa)
					(setf coluna-mais-baixa altura)
				)
			)
		)

		; devolver a diferenca
		(* factor (- coluna-mais-alta coluna-mais-baixa))
	)
)


(defun heuristica-casas-ocupadas (estado)
	"calcula o numero total de posicoes (casas) ocupadas"

	(let* ((tabuleiro (estado-tabuleiro estado))
		   (factor 25)
		  )
		  
		; se o topo estiver preenchido devolve o maior valor possivel da heuristica (nao queremos este estado!)
			(when (tabuleiro-topo-preenchido-p tabuleiro)
				(return-from heuristica-casas-ocupadas (* factor +casas+))
			)
		
		(* factor (tabuleiro-total-ocupadas (estado-tabuleiro estado)))
	)
)


(defun heuristica-buracos (estado)
	"calcula a diferenca entre a soma das posicoes mais altas das colunas e o numero de casas ocupadas"
	
	(let* ((tabuleiro (estado-tabuleiro estado))
		   (colunas (tabuleiro-altura-colunas tabuleiro))
		   (casas-ocupadas (tabuleiro-total-ocupadas (estado-tabuleiro estado)))
		   (soma-pos-mais-altas 0)
		   (factor 700)
		  )

		; se o topo estiver preenchido devolve o maior valor possivel da heuristica (nao queremos este estado!)
		(when (tabuleiro-topo-preenchido-p tabuleiro)
			(return-from heuristica-buracos (* factor +casas+))
		)
		
		; achar soma das posicoes mais altas das colunas
		(dotimes (col +colunas+)
			
			(let* ((altura (aref colunas col)))
			
				(setf soma-pos-mais-altas (+ soma-pos-mais-altas altura))
			)
		)

		; devolver a diferenca
		(* factor (- soma-pos-mais-altas casas-ocupadas))
	)
)


(defun heuristica-altos-e-baixos (estado)
	"calcula a soma do valor absoluto das diferencas de alturas de colunas adjacentes"
	
	(let* ((tabuleiro (estado-tabuleiro estado))
		   (colunas (tabuleiro-altura-colunas tabuleiro))
		   (resultado 0)
		   (factor 75)
		  )
		
		; se o topo estiver preenchido devolve o maior valor possivel da heuristica (nao queremos este estado!)
		(when (tabuleiro-topo-preenchido-p tabuleiro)
			(return-from heuristica-altos-e-baixos (* factor (- +casas+ +linhas+)))
		)
		
		; achar soma do valor absoluto das diferencas de alturas de colunas adjacentes
		(dotimes (col (- +colunas+ 1))
			
			(setf resultado (+ resultado (abs (- (aref colunas (+ col 1)) (aref colunas col)))))
		)
		
		; devolver resultado
		(* factor resultado)
	)
)


(defun heuristica-best (estado)
	"a melhor funcao heuristica (combinacao de heuristicas)"
	
	(let* ((h1 (heuristica-dif-colunas estado))
		   (h2 (heuristica-casas-ocupadas estado))
		   (h3 (heuristica-buracos estado))
		   (h4 (heuristica-altos-e-baixos estado))
		  )
		
		(+ (* 0.40 h1) (* 0.05 h2) (* 0.30 h3) (* 0.25 h4))
	)
)



;; auxiliares
(defun sem-heuristica (estado)
	"funcao heuristica chamada nas procuras cegas"
	(ignore-value estado)
	0
)


