
; hangman is an alternative problem to test the searches abstraction

(defun rlh ()
	(load "hangman.lisp")
)


; problema com solucao
; (procura-pp (cria-problema-forca "ia" 5))

; problema sem solucao - nao da para escrever ia com 1 caracter :)
; (procura-pp (cria-problema-forca "ia" 1))

(defun cria-problema-forca (palavra n-tentativas)
	(make-problema 
		:estado-inicial (create-h-state palavra n-tentativas)
		:solucao (function solution)
		:accoes (function actions)
		:resultado (function result)
		:custo-caminho (function cost)
))

(defstruct h-state
	word
	guessed-word
	n-tries
	score
)


(defun create-h-state (word n-tries)
	(make-h-state :word word 
		:guessed-word (make-array (length word) :initial-element nil)
		:n-tries n-tries
		:score 0
	)
)


(defun copy-state (h-state)
	(let ((new-state (make-h-state :word (h-state-word h-state) 
		:guessed-word (make-array (length (h-state-word h-state)) :initial-element nil)
		:n-tries (h-state-n-tries h-state) :score (h-state-score h-state))))
		
	(loop for i upto (1- (length (h-state-guessed-word h-state)))
		do (setf (aref (h-state-guessed-word new-state) i)
				(aref (h-state-guessed-word h-state) i)))
				
	new-state
))


(defun solution (h-state)
	(loop for i upto (1-(length (h-state-word h-state)))
		do (if (not (equal
				(aref (h-state-word h-state) i)
				(aref (h-state-guessed-word h-state) i)))
			
			(return-from solution nil)))
	t
)


(defun actions (h-state)
	(if	(<= (h-state-n-tries h-state) 0)
		nil
		'(#\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\a #\s #\d #\f #\g #\h #\j #\k #\l #\z #\x #\c #\v #\b #\n #\m)
))


; each try subs 20 points
; each correct letter adds 100 points
(defun result (h-state action)
	(let ((copy (copy-state h-state)))
		
	(setf (h-state-n-tries copy) (1- (h-state-n-tries copy)))
	(setf (h-state-score copy) (- (h-state-score copy) 20))
	
	(loop for i upto (1-(length (h-state-word h-state)))
		do (if (equal
				(aref (h-state-word h-state) i)
				action)
				
				(progn
				(setf (aref (h-state-guessed-word copy) i) action)
				(setf (h-state-score copy) (+ (h-state-score copy) 100)))))
	copy
))

(defun cost (h-state)
	(- 0 (h-state-score h-state))
)


(setf s1 (create-h-state "ia" 3))
(setf s2 (result s1 #\a))
(setf s3 (result s2 #\i))
(setf s4 (result s1 #\b))
;s3 should be solution


