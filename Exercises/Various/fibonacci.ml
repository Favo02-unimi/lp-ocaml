let rec trfibo goal current last next =
	if (goal = current) then next
	else (trfibo goal (current + 1) next (last + next));;

let fibo n =
	if n <= 1 then 1
	else trfibo n 1 0 1;;
