open Str
open LogParsers
open Unix
open Graphics

let _ =
  let usage = "Usage: logstats [OPTION]... filename " 
  and filename = ref None
  and filter = ref "" in 
  Arg.parse ["-filter", Arg.Set_string filter, "filter the date in the logs" ] 
	(fun x -> filename := Some x) 
	usage;
  let fn = (match !filename with 
    None-> Printf.eprintf "%s\n%!" usage; exit 1
    | Some fn -> fn) in
    let module Parser = StandardParser(MemoryLogBehaviour) in let courbes = 
		(match !filter with
			"" -> Parser.load_from_file fn (fun _ -> true)
			| _  -> Parser.load_from_file fn (fun line -> (Str.string_match (Str.regexp !filter) line 0))) in
	  	match courbes with
				Courbe [] -> print_string "nothing to draw\n"; exit 0
				| Courbe (_ as l)   -> GraphLog.display_logs fn (List.rev l)
;;
