#!/usr/bin/fish

time clisp -i "tetris.lisp" -x "(rl) (pb2)" &

set i 0
while true
	
	echo $i
	set i (math $i + 1)
	sleep 1
end
