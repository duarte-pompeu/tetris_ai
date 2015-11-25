
(defun n-pecas-iguais (peca n)
	"gera n pecas iguais
	exemplo: (n-pecas-iguais 'o 2)"

	(let ((lista-pecas nil))
		(loop for i from 1 upto n
		do (push peca lista-pecas))

		lista-pecas
))

(defun n-pecas-aleatorias (n)
	(let ((pecas nil))
	
	(loop for i upto n
	do (push (nth (random 7) *todas-pecas*) pecas))
	
	pecas
))

;  (testa-pp (cria-problema (estado-10-i) nil))
(defun pecas-10-i ()
	"estado com 10 'i's
	uma solucao optima deve devolver uma pontuacao de 800"

	(n-pecas-iguais 'i 10)
)
