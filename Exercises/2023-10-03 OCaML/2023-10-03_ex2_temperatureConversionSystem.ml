(* scale di temperatura disponibili *)
type scale = 
  | Celsius
  | Kelvin
  | Fahrenheit
  | Rankine
  | Delisle
  | Newton
  | Reaumur
  | Romer;;

(* una temperatura (non pura) *)
type temperature = {
  value : float;
  scale : scale
}

(* tipo tabella di conversione: i valori in tutte le scale *)
type conversion_table = {
  celsius : float;
  kelvin : float;
  fahrenheit : float;
  rankine : float;
  delisle : float;
  newton : float;
  reaumur : float;
  romer : float;
};;

(* il valore in celsius data una scala (tutti gli altri sono evidentemente sbagliati) *)
(* esempio: (to_celsius 5).kelvin è il valore in celsius se la scala di 5 è kelvin *)
let to_celsius temp = {
  celsius = temp;
  kelvin = temp -. 273.15;
  fahrenheit = (temp -. 32.) *. (5. /. 9.);
  rankine = (temp -. 491.67) *. (5. /. 9.);
  delisle = 100. -. (temp *. (2. /. 3.));
  newton = temp *. (100. /. 33.);
  reaumur = temp *. (5. /. 4.);
  romer = (temp -. 7.5) *. (40. /. 21.);
};;

(* data un valore in celsius, calcolare tutti i valori nelle altre scale *)
(* esempio: (from_celsius 5).kelvin è il valore in kelvin della temperatura 5°C *)
let from_celsius temp = {
  celsius = temp;
  kelvin = temp +. 273.15;
  fahrenheit = temp *. (9. /. 5.) +. 32.;
  rankine = (temp +. 273.15) *. (9. /. 5.);
  delisle = (100. -. temp) *. (3. /. 2.);
  newton = temp *. (33. /. 100.);
  reaumur = temp *. (4. /. 5.);
  romer = temp *. (21. /. 40.) +. 7.5;
};;

(* restituisce il valore in tutte le unità data una temperatura *)
(* passa dalla scala attuale a celsius e da celsius a tutte le altre *)
let to_scale cur_temp =
  match cur_temp.scale with
    | Celsius -> from_celsius cur_temp.value
    | Kelvin -> from_celsius (to_celsius cur_temp.value).kelvin
    | Fahrenheit -> from_celsius (to_celsius cur_temp.value).fahrenheit
    | Rankine -> from_celsius (to_celsius cur_temp.value).rankine
    | Delisle -> from_celsius (to_celsius cur_temp.value).delisle
    | Newton -> from_celsius (to_celsius cur_temp.value).newton
    | Reaumur -> from_celsius (to_celsius cur_temp.value).reaumur
    | Romer -> from_celsius (to_celsius cur_temp.value).romer;;

(* print di una tabella di conversione *)
let print_table table =
  Printf.printf "Celsius \t %f" table.celsius;
  Printf.printf "\nKelvin \t\t %f" table.kelvin;
  Printf.printf "\nFahrenheit \t %f" table.fahrenheit;
  Printf.printf "\nRankine \t %f" table.rankine;
  Printf.printf "\nDelisle \t %f" table.delisle;
  Printf.printf "\nNewton \t\t %f" table.newton;
  Printf.printf "\nReaumur \t %f" table.reaumur;
  Printf.printf "\nRomer \t\t %f" table.romer;
  print_string "\n\n";;
  ();;

print_table (to_scale {value=0.;scale=Celsius});;
print_table (to_scale {value=0.;scale=Kelvin});;
print_table (to_scale {value=0.;scale=Fahrenheit});;
print_table (to_scale {value=0.;scale=Rankine});;
print_table (to_scale {value=0.;scale=Delisle});;
print_table (to_scale {value=0.;scale=Newton});;
print_table (to_scale {value=0.;scale=Reaumur});;
print_table (to_scale {value=0.;scale=Romer});;
