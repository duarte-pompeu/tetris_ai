
(defun n-pecas-iguais (peca n)
	"gera n pecas iguais
	exemplo: (n-pecas-iguais 'o 2)"

	(let ((lista-pecas nil))
		(loop for i from 1 upto n
		do (push peca lista-pecas))

		lista-pecas
))

(defun prob-10-i ()
	"estado com 10 'i's
	uma solucao optima deve devolver uma pontuacao de 800"

	(cria-estado (n-pecas-iguais 'l 10))
)
