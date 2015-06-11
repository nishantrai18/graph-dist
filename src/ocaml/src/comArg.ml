(** Read command arguments as a sequence of commands with possible options. 
    Example: myprog read -format xml -file f.xml check -dtd f.dtd
*)

type args = string * string list

type com = 
    string (* command name *)
    * string (* help message *)
    * 
