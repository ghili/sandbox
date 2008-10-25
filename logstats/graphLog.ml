open Graphics
open LogParsers

let grey = 0x888888;;


(* affiche les numeros de jour *)
let add_label_day x lastday= 
  let (a,b) = current_point() in
  moveto a 550;
  lastday := x.day;
  draw_string (string_of_int !lastday);
  set_color grey;
  draw_segments [|(a,0,a,5)|];
  set_color black;
  moveto a b
;;


(* affiche les heures *)
let add_label_hour x lasthour= 
  let (a,b) = current_point() in
  moveto a 530;
  lasthour := x.hour;
  draw_string (string_of_int !lasthour);
  set_color grey;
  draw_segments [|(a,0,a,5)|];
  set_color black;
  moveto a b
;;


(* cherche une approximation du max a partir d'un echantillon de valeurs d'une liste *)
let rec max_approx l len currentmax n= 
  if n <= 0 then currentmax 
  else let point = (List.nth l (Random.int len)) in 
  let memValue = point.value in
  if memValue>currentmax then
	max_approx l len memValue (n-1)
  else
	max_approx l len currentmax (n-1)
;;


(* modifie le max pour l'affichage*)
let smooth_max max = 
  let len = String.length (string_of_int max) in
  let fact = int_of_float (10. ** (float_of_int (1 + len/ 2))) in
  fact * ( 1 + max / fact)


(* affiche les ordonnees *)
let add_indic_oordonnee max maxscaled = 
  let (a,b) = current_point() in
  moveto a maxscaled;
  set_color blue;
  draw_string ("_"^string_of_int (max/1000) ^"M");
  set_color black;
  moveto a b
;;

(* determine si on doit afficher les abscisses par heure *)
let is_by_hour l = 
  let firstpoint = (List.nth l 0) in 
  let lastpoint = (List.nth l (List.length l - 1)) in
  Str.string_match (Str.regexp (String.sub firstpoint.content 0 10)) (String.sub lastpoint.content 0 10) 0

let display_approx_hour_by_pos status fact_abscisse l = 
  let len = List.length l in
  let indice = int_of_float ((float_of_int status.mouse_x ) /. fact_abscisse) in
  if indice < len then (
	let point = List.nth l indice in 
	Printf.printf "content= %s \n" point.content;
	flush stdout)

(* event handling *)
let rec handle_event fact_abscisse l =
  let status = wait_next_event [Key_pressed; Button_down] in
  if status.button then (display_approx_hour_by_pos status fact_abscisse l; handle_event fact_abscisse l)

(* affichage du graphe *)
let display_logs filename l =  
  let dimension = ( 1050, 600) in
  open_graph (Printf.sprintf " %ix%i" (fst dimension) (snd dimension)); 
 set_window_title filename;
  let lastday = ref 0 
  and lasthour = ref 0 
  and indice = ref 0 
  and  by_hour = is_by_hour l 
  and len = List.length l in
  Printf.printf "%i points to draw\n" len;
  let max = smooth_max (max_approx l len 0 40) 
  and  fact_abscisse = (1000.0 /. (float_of_int len)) in
  let scale_abscisse abscisse = int_of_float ((float_of_int abscisse) *. fact_abscisse) 
  and scale_val value = int_of_float (200.0 *. (float_of_int (value)) /. (float_of_int max )) in
  add_indic_oordonnee max (scale_val max);
  add_indic_oordonnee (2*max) (scale_val (2*max));
  List.iter 
	(fun x -> let abscisse = scale_abscisse !indice in
	if x.day != !lastday then (add_label_day x lastday);
	if by_hour && (x.hour != !lasthour) then (add_label_hour x lasthour);
	lineto abscisse (scale_val x.value) ; indice:= !indice +1 ) l;
  handle_event fact_abscisse l


