
let () = 
  Printexc.record_backtrace true ;
  try
    Debug.set_verbosity "info" ;

    E.unit () ;
    BoolIntInt.unit () ;
    Bitarray.unit () ;
    NatHeap.unit () ;
    UnionFind.unit () ;
    StaticGraph.unit () ;

    Debug.the_end () ;
    ()
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    flush stderr ;
    exit 2
