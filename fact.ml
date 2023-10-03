let rec trfact goal current last_res =
	if (current = (goal+1)) then last_res
	else trfact goal (current + 1) (current * last_res);;

let fact n =
	if n <= 2 then n
	else trfact n 2 1;;
