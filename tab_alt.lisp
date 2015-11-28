;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 2.1.2 - TIPO TABULEIRO
;
; FUNCOES PUBLICAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tabuleiro
  "estrutura que define o tipo tabuleiro de jogo
      game_field: um array que representa o campo de jogo cada posicao
        e' representada por um boolean em que
        T representa uma casa preenchida e nil uma vazia
      columns_height: array numero de linhas por coluna
      highest_pair: par com linha e coluna da posicao mais alta e ultima posicao de memoria
      usadas-por-linha: array com numero de casas preenchidas por linha
      total_filled: numero de casas preenchidas no tabuleiro"
	game_field
	columns_height
	highest_pair
	filled_on_line
	total_filled)


(defun cria-tabuleiro ()
  "cria e inicia a estrutura tabuleiro"
  (let ((campo (make-array '(19  10) :initial-element nil)) ;uma linha extra 19 em vez de 18  para facilitar o remove linha
	(alturas (make-array +colunas+ :initial-element 0))
	(altura-maxima (cons 0 0))
	(filled_on_line (make-array 19 :initial-element 0)) ;um nil extra para facilitar o remove linha
	(casas-preenchidas 0))
	(make-tabuleiro :game_field campo
			:columns_height alturas
			:highest_pair altura-maxima
			:filled_on_line filled_on_line
			:total_filled casas-preenchidas)))


(defun copia-tabuleiro (tabuleiro)
  "recebe um tabuleiro de devolve uma co'pia do tabuleiro original"
  (let ((novo-tabuleiro (cria-tabuleiro))
	(game_field (tabuleiro-game_field tabuleiro))
	(alturas (tabuleiro-columns_height tabuleiro))
	(linha-mais-alta (car (tabuleiro-highest_pair tabuleiro)))
	(coluna-mais-alta (cdr (tabuleiro-highest_pair tabuleiro)))
	(filled_on_line (tabuleiro-filled_on_line tabuleiro))
	(total_filled (tabuleiro-total_filled tabuleiro)))
    (setf (tabuleiro-game_field novo-tabuleiro)
	  (copy-array game_field))
    (setf (tabuleiro-columns_height novo-tabuleiro)
	  (copy-array alturas))
    (setf (tabuleiro-highest_pair novo-tabuleiro)
	  (cons linha-mais-alta coluna-mais-alta))
    (setf (tabuleiro-filled_on_line novo-tabuleiro)
	  (copy-array filled_on_line (1+ linha-mais-alta) 0))
    (setf (tabuleiro-total_filled novo-tabuleiro )
	  total_filled)
    novo-tabuleiro))


(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  "predicado que devolve T se a casa linha coluna esta preenchida nil caso contra'rio"
  (aref (tabuleiro-game_field tabuleiro) linha coluna))


(defun tabuleiro-altura-coluna (tabuleiro coluna)
  "recebe um tabuleiro e uma coluna e devolve a linha mais alta (1-18) preenchida 0 significa nao preenchida"
  (aref (tabuleiro-columns_height tabuleiro) coluna))


(defun tabuleiro-linha-completa-p (tabuleiro linha)
  "recebe um tabuleiro e uma linha e devolve T caso a linha esteja preenchida ou completa falso caso contra'rio"
  (eq (aref (tabuleiro-filled_on_line tabuleiro) linha) +colunas+))


(defun tabuleiro-preenche! (tabuleiro linha coluna)
  "marca a posicao linha coluna ocupada"
    (and (< linha +linhas+) (< coluna +colunas+) (>= linha 0) (>= coluna 0)
	 (let ((alturas (tabuleiro-columns_height tabuleiro))
	       (filled_on_line (aref (tabuleiro-filled_on_line tabuleiro) linha)))
	   (progn
	     (setf (aref (tabuleiro-game_field tabuleiro) linha coluna) +ocupada+)
	     (setf (aref (tabuleiro-filled_on_line tabuleiro) linha) (+ filled_on_line 1))
	     (incf (tabuleiro-total_filled tabuleiro))
	     (if (>= linha (aref alturas coluna)) ;nova altura ->
					;se altura da coluna = linha  e' nova altura
					;se = linha+1 nao e' nova altura mas pode ser nova ultima posicao de memoria preenchida  se a coluna for mais alta
		 (progn (setf (aref alturas coluna) (+ linha 1))
			(let ((ultima-linha (car (tabuleiro-highest_pair tabuleiro)))
			      (ultima-coluna (cdr (tabuleiro-highest_pair tabuleiro))))
			  (when (or (and (= ultima-linha linha) ; caso linha maior que ultima linha esta coberto pelo if
					 (> coluna ultima-coluna))
				    (> linha ultima-linha))
			    (setf (tabuleiro-highest_pair tabuleiro) (cons linha coluna))))))))) T)


(defun tabuleiro-remove-linha! (tabuleiro linha)
  "remove a linha fazendo com que as linhas de cima descam uma casa"
    (let ((game_field (tabuleiro-game_field tabuleiro))
	  (alturas (tabuleiro-columns_height tabuleiro))
	  (total_filled (tabuleiro-total_filled tabuleiro))
	  (filled_on_line (aref (tabuleiro-filled_on_line tabuleiro) linha))
	  (par-mais-alto (tabuleiro-highest_pair tabuleiro))
	  (linha-mais-alta (car (tabuleiro-highest_pair tabuleiro))))
      (setf (tabuleiro-total_filled tabuleiro)
	    (- total_filled filled_on_line)) ;actualiza total ocupadas
      (dotimes (i (- (+ 2 linha-mais-alta) (incf linha)))  ; linha passa a ser linha+1 (linha de cima) no ciclo linha-mais-alta  mais dois por causa do incf para nao fazer inf linha sempreem cada iteracao do cilo
	(down-line! tabuleiro (+ linha i)))
      (if (= linha-mais-alta 0)
	  (setf  (tabuleiro-highest_pair tabuleiro) '(0 . 0)) ;linha mais alta nao pode ser negativa
	  (setf (car par-mais-alto)
		(- linha-mais-alta 1)))  ;actualiza linha do par posicao mais alta
      (setf (cdr par-mais-alto) (stabilize-heights game_field alturas linha-mais-alta (decf linha))))) ;chamado com a linha de cima (incf ) se a ultima linha tivesse preenchida o jogo tinha acabado


(defun tabuleiro-topo-preenchido-p (tabuleiro)
  "devolve T se houver uma peca que ocupa a linha 17"
  (eq (car (tabuleiro-highest_pair tabuleiro)) +linha-maxima+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; SECCAO 2.1.1 - TIPO TABULEIRO
;
; FUNCOES INTERNAS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun copy-array (array &optional (limite (array-total-size array)) init-element (dims (array-dimensions array)))
  "recebe um array e devolve uma co'pia do array sem alterar o original"

  (let ((new-array (make-array dims :initial-element init-element)))
    (dotimes (i limite)
      (setf (row-major-aref new-array i) ; o array e' uma zona continua de memoria ordenada por linhas
	    (row-major-aref array i)))
    new-array)) ;return

 (defun find-max (array)
   "devolve o elemento maximo num array de numeros nao vazio"

   (let ((maximo (row-major-aref array 0)))
     (dotimes (i (array-total-size array))
       (let ((elemento (row-major-aref array i)))
 	(when (> elemento maximo) ;row-major-aref trata o array como um vector
 	  (setf maximo elemento))))
     maximo))


(defun find-height (game_field indice-coluna &optional (limite +linhas+))
  "encontra a altura de uma coluna com indice-coluna no array game_field"

  (let ((resultado 0))
    (dotimes (i limite resultado)
      (when (aref game_field i indice-coluna)
	(setf resultado (+ i 1))))))


(defun stabilize-heights (game_field alturas altura-maxima linha-removida)
  "repoe as alturas num evento remove-linha!"

  (let ((maxima 0))
    (dotimes (i +colunas+ maxima) ;actualiza alturas
      (let ((altura (aref alturas i)))
	(if (> altura (1+ linha-removida))
	    (setf (aref alturas i) (- altura 1)) ;ha' casas preenchidas por cima da linha
	    (setf (aref alturas i) (find-height game_field i altura-maxima)))
	(when (> altura (aref alturas maxima))
	  (setf maxima i)))))) ; pode haver vazio por baixo da linha


(defun down-line! (tabuleiro linha)
  "copia a linha sobre a linha anterior apagando assim a informacao que la' estava"

  (let ((game_field (tabuleiro-game_field tabuleiro))
	(linha-de-baixo (- linha 1)))
    (let ((ocupadas-linha-de-cima (aref (tabuleiro-filled_on_line tabuleiro) linha)))
      (setf (aref (tabuleiro-filled_on_line tabuleiro) linha-de-baixo) ocupadas-linha-de-cima) ;actualiza ocupadas na linha
      (dotimes (i +colunas+) ;actualiza game_field
	(setf (aref game_field linha-de-baixo i)
	      (aref game_field linha i))))))

;;Esta funcao pode comparar arrays com dimensoes garantidamente identicas
;;  que desta forma se encontram em memoria como um vector
;;  assim,  as coordenadas de cada elemento vao concidir
(defun vectors-equal-p (vec1 vec2)

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


(defun pairs-equal-p (Par Outro)
  "Predicado Que Compara Dois Pares De Numeros E Devolve T Se Forem Identicos Nil caso contr'ario"

  (and (eq (car par) (car outro)) (eq (cdr par) (cdr outro))))


(defun tabuleiros-iguais-p (tabuleiro outro)
  "dados dois tabuleiro devolve T se forem iguais e nil caso contr'ario"

  (cond ((/= (tabuleiro-total_filled tabuleiro)  (tabuleiro-total_filled outro)) nil)
	((not (pares-iguais-p (tabuleiro-highest_pair tabuleiro) (tabuleiro-highest_pair outro))) nil)
	((not (vectors-equal-p (tabuleiro-columns_height tabuleiro) (tabuleiro-columns_height outro))) nil)
	((not (vectors-equal-p (tabuleiro-filled_on_line tabuleiro) (tabuleiro-filled_on_line outro))) nil)
	((not (vectors-equal-p (tabuleiro-game_field tabuleiro) (tabuleiro-game_field outro))) nil)
	(t t)))


(defun tabuleiro->array (tabuleiro)
  "recebe um tabuleiro e devvolve o array de booleans correspondente"

  (let ((linha-mais-alta (car (tabuleiro-highest_pair tabuleiro)))
	(coluna-mais-alta (cdr (tabuleiro-highest_pair tabuleiro))))
    (copy-array (tabuleiro-game_field tabuleiro) (+ (* linha-mais-alta 10) coluna-mais-alta 1) nil '(18 10))))


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

(setf t_alt1 (cria-tabuleiro))
(setf e_alt1 (make-estado :pontos 0 :pecas-por-colocar '(i o j i) 
:pecas-por-colocar '() :tabuleiro (copia-tabuleiro t_alt1)))
(setf pp_alt1 (procura-pp (cria-problema e_alt1 (function sem-heuristica))))
