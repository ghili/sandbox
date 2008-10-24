open Str
open Stack

type logRecord = { day: int; hour :int ;content :string; value: int};;
type representationStatistique = Courbe of logRecord list | Courbes of (string,logRecord) Hashtbl.t;;

module CommonLogBehaviour = struct
  let extrait_jour tokens = int_of_string (String.sub (List.nth tokens 0) 8 2)
  let extrait_heure tokens = int_of_string (String.sub (List.nth tokens 1) 0 2)
end

module type LOGBEHAVIOUR = sig
  val init_representation : unit -> representationStatistique
  val dispatche : logRecord -> representationStatistique -> representationStatistique
  val extrait_point : string -> (string -> bool ) -> logRecord Stack.t -> logRecord option
end


module MemoryLogBehaviour : LOGBEHAVIOUR = struct
  include CommonLogBehaviour

  let init_representation () = Courbe []

  let dispatche logRecord (Courbe courbe) = 
	Courbe (logRecord::courbe)

  let get_mem tokens = 
	let chaine =  (List.nth tokens 9) in 
	let len = String.length chaine in 
	int_of_string (String.sub chaine 0 (len -2))

  let extrait_point ligne filtre stack = 
	let tokens = Str.bounded_split (regexp " ") ligne 11 in
	if (List.length tokens < 10 || not (filtre ligne))
	then None
	else Some {day= extrait_jour tokens ; hour = extrait_heure tokens; content = ligne ; value = get_mem tokens}

end


module PerfLogBehaviour: LOGBEHAVIOUR = struct
  include CommonLogBehaviour

  let init_representation() = Courbes (Hashtbl.create 10)

  let dispatche pt (Courbes courbes) = 
	Hashtbl.add courbes pt.content pt;
	Courbes courbes

  let extrait_point ligne filtre stack = 
	let tokens = Str.bounded_split (regexp " ") ligne 11 in
	if (List.length tokens < 10 || not (filtre ligne))
	then None
	else Some {day= extrait_jour tokens ; hour = extrait_heure tokens; content = ligne ; value = 1}

end

(*
  parser to read memory.log 
module LogMemParser = struct
  let load_from_file filename l filter = 
	let stream = open_in filename in
	  try
 		while true do
		  let ligne = input_ligne stream in
		  if String.length ligne != 0 then
			let point = to_point ligne filter in 
			match point with 
			  None -> ignore ligne
			| Some pt -> l:= pt::!l
 		done;
   with End_of_file -> close_in stream
end*)

module StandardParser
	(Behaviour:LOGBEHAVIOUR)= struct 

  let load_from_file filename filtre = 
    let fn_table :(string,logRecord)Hashtbl.t =  Hashtbl.create 10 in
    let fn_stack :(logRecord)Stack.t =  Stack.create() in
    let stream = open_in filename in
    let representation = ref (Behaviour.init_representation()) in
      try
      while true do
        let ligne = input_line stream in
        if (String.length ligne != 0) then
        match Behaviour.extrait_point ligne filtre fn_stack with
        | Some point -> representation := (Behaviour.dispatche point !representation);
        (*if string_match (regexp ".*Start of \\(.*\\)") ligne 0 then 
          push {day=1;hour=1;content=(Str.matched_group 1 ligne);value=0} fn_stack
        else 
          if string_match (regexp ".*End of  \\(.\\*") ligne 0 then
          let pt = pop fn_stack in Hashtbl.add fn_table pt.content pt*)
      done;
      !representation
      with End_of_file -> close_in stream;
  !representation
	
  let to_sec word = 
	if Str.string_match (regexp "\\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\),\\([0-9]*\\)") word 0 then
	  3600000 * int_of_string (Str.matched_group 1 word) + 60000 * int_of_string (Str.matched_group 2 word) + 1000 * int_of_string (Str.matched_group 3 word) + int_of_string (Str.matched_group 4 word)
	else 
	  0

end
