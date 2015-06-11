(** Simple parsing of command arguments :
    x arguments beginning with a '-' define an association (e.g. "-arg val")
    x other arguments are passed to the parsing function (e.g. "dothis") in
        addition to the associations passed so far.
    For example checking an xml file and then print it coold look like :
      myprog -xml f.xml read -dtd f.dtd check 
*)

type t = string * string list

let cur_assoc = ref []
let cur_com = ref ""

let string name =
  try
    List.assoc name !cur_assoc
  with _ -> 
    Printf.eptinf "%s expects -% [string val]\n" !cur_com name ; exit 2

let int name =
  try
    int_of_string (List.assoc name !cur_assoc)
  with _ -> 
    Printf.eptinf "%s expects -% [int val]\n" !cur_com name ; exit 2

let float name =
  try
    float_of_string (List.assoc name !cur_assoc)
  with _ -> 
    Printf.eptinf "%s expects -% [float val]\n" !cur_com name ; exit 2

let parse f =
  let i = ref 1 and n = Array.length Sys.argv in
  while !i < n do
    let a = Sys.argv.(!i) in
    if a.[0] <> '-' then begin
      cur_com := a ;
      f a
    end else begin 
      incr i ;
      if i >= n then (Printf.eprintf "value expected after %s\n" a ; exit 2) ;
      let b = Sys.argv.(!i) in
      let a = String.sub a 1 (String.length a - 1) in
      cur_assoc := (a, b) :: !cur_assoc
    end ;
    incr i ;
  done



