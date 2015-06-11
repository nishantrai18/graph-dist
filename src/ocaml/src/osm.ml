(* Read .osm xml files from Open Street Map *)

type attr = string * string
type tag = string * attr list
type tree = E of tag * tree list | D of string

let tag_of_xmlm_tag ((_,name), attr) =
  let attr = List.map (fun ((_,k),v) -> k,v) attr in
  name, attr


let parse ic root_tags parse_root =
  let ic = Xmlm.make_input (`Channel ic) in 
  let el tag childs = E (tag_of_xmlm_tag tag, childs)  in
  let data d = D d in
  let rec pull depth = 
    match Xmlm.peek ic with
      | `El_start ((_,tag),_) when List.mem tag root_tags ->
        let tree = Xmlm.input_tree ~el ~data ic in
        parse_root tree ;
        if depth = 0 then () else pull depth
      | _ ->
        match Xmlm.input ic with 
        | `El_start _ -> pull (depth + 1)
        | `El_end -> if depth = 1 then () else pull (depth - 1)
        | `Data _ -> pull depth 
        | `Dtd _ -> pull depth 
  in
  pull 0 ;
  flush stdout ;
  if not (Xmlm.eoi ic) then invalid_arg "document not well-formed" ;
  ()

(*
let parse ic root_tags parse_root =
  let ic = Xmlm.make_input (`Channel ic) in 
  let el tag childs = E (tag_of_xmlm_tag tag, childs)  in
  let data d = D d in
  let nb =ref 0 in
  let rec pull depth =
    incr nb ; Debug.progress !nb (-1) "parse xml" ;
    match Xmlm.peek ic with
      | `El_start ((_,tag),_) when List.mem tag root_tags ->
        let tree = Xmlm.input_tree ~el ~data ic in
        ignore tree ; (* parse_root tree ; *)
        if depth = 0 then () else pull depth
      | _ ->
    match Xmlm.input ic with 
      | `El_start _ -> pull (depth + 1)
      | `El_end -> if depth = 1 then () else pull (depth - 1)
      | `Data _ -> pull depth 
      | `Dtd _ -> pull depth 
  in
  pull 0 ;
  flush stdout ;
  if not (Xmlm.eoi ic) then invalid_arg "document not well-formed" ;
  ()
*)


let coord = Coord.make (1024 * 1024) 

let dist_id u v =
  Grid.dist (Coord.get_loc coord u) (Coord.get_loc coord v)



(* Routing information from OSM, see :
     http://wiki.openstreetmap.org/wiki/OSM_tags_for_routing 
   Speeds :
     http://wiki.openstreetmap.org/wiki/OSM_tags_for_routing/Maxspeed#France
*)

 let is_road highway = match highway with
    | "motorway" -> true
    | "trunk" -> true
    | "primary" -> true
    | "secondary" -> true
    | "tertiary" -> true
    | "unclassified" -> true
    | "residential" -> true
    | "living_street" -> true
    | "service" -> false (* not public *)
    | "road" -> true
    | "motorway_link" -> true
    | "trunk_link" -> true 
    | "primary_link" -> true 
    | "secondary_link" -> true
    | "tertiary_link" -> true
    | _ -> (* Printf.eprintf "%s\n" highway ; flush stderr ;*) false

let time highway maxspeed urban u v =
  let s = 
    try 
      float_of_string maxspeed 
    with _ ->
      let maxspeed = E.String.trim maxspeed in
      let mph = 1.609 and knots = 1.852 in
      let unit_kmh = ref 1. in
      let maxspeed = 
        if E.String.has_suffix maxspeed "mph"
        then (unit_kmh := mph ; 
              E.String.trim (E.String.del_suffix maxspeed "mph"))
        else if E.String.has_suffix maxspeed "knots"
        then (unit_kmh := knots ; 
              E.String.trim (E.String.del_suffix maxspeed "knots"))
        else maxspeed
      in
      let max_split sep =
        let l = E.String.split_all ~separator:sep maxspeed in
        let l = List.rev_map (fun s -> float_of_string (E.String.trim s)) l in
        let s = List.fold_left max 0. l in
        s *. !unit_kmh
      in
      try max_split ' ' with _ ->
      try max_split ',' with _ ->
      try max_split ';' with _ ->
      try max_split '|' with _ ->
      match maxspeed with
        | "FR:motorway" -> 130.
        | "FR:rural" -> 90. 
        | "FR:urban" -> 50. 
        | "FR:walk" -> 20.
        | "DE:motorway" -> 200.
        | "DE:rural" -> 100.
        | "DE:urban" -> 50.
        | "IT:rural" -> 90.
        | "IT:motorway" -> 130.
        | "IT:urban" -> 50.
        | "RO:motorway" -> 130.
        | "RO:rural" -> 90.
        | "RO:trunk" -> 100.
        | "RO:urban" -> 50.
        | "RU:rural" -> 90.
        | "RU:urban" -> 60.
        | "GB:motorway" -> 70. *. mph
        | "GB:nsl_dual" -> 70. *. mph
        | "GB:nsl_single" -> 60. *. mph
        | "UK:motorway" -> 70. *. mph
        | "UK:nsl_dual" -> 70. *. mph
        | "UK:nsl_single" -> 60. *. mph 
        | _ -> 
      if maxspeed <> "" then Debug.info "buggy maxspeed : %s" maxspeed ;
      match highway with
        | "motorway" -> if urban then 110. else 130.
        | "trunk" -> if urban then 110. else 110.
        | "primary" -> if urban then 50. else 90.
        | "secondary" -> if urban then 50. else 85.
        | "tertiary" -> if urban then 50. else 80.
        | "unclassified" -> if urban then 50. else 70.
        | "residential" -> 50.
        | "living_street" -> 20.
        | "service" -> 20.
        | "road" -> 90.
        | "motorway_link" -> 90.
        | "trunk_link" -> 70. 
        | "primary_link" -> 65. 
        | "secondary_link" -> 60.
        | "tertiary_link" -> 50.
        | s -> assert false 
  in
  let s = s *. 1000. /. 3600. in
  let d = dist_id u v in
  d /. s

let () =
  Printexc.record_backtrace true ;
  try
    Debug.set_verbosity "info"  ;

    let roots = ["node"; "way"] in
    let tags sons =
      let attrs = ref [] in
      List.iter (fun s -> match s with
        | E ((tag,attr), sons) when tag = "tag" ->
          let k = List.assoc "k" attr in
          let v = List.assoc "v" attr in
          attrs := (k, v) :: !attrs
        | _ -> () 
      ) sons ;
      let tag_val k = try List.assoc k !attrs with Not_found -> "" in
      tag_val
    in

    let node attr sons =
      let id = List.assoc "id" attr in
      let id = int_of_string id in
      let lon = float_of_string (List.assoc "lon" attr) in
      let lat = float_of_string (List.assoc "lat" attr) in
      if Coord.mem coord id then begin
        Coord.set_loc coord id (lon, lat) ;
        if Coord.mult coord id > 1 then
          Printf.printf "v %d %.6f %.6f\n" id lon lat ;
      end ;
      let attr = tags sons in
      let place = attr "place" and name = attr "name" in
      if Grid.place_is_city place then begin
        Printf.printf "c %s %s %.6f %.6f\n" place name lon lat ;
        Grid.place_add place name (lon, lat) ;
      end
    in

    let way sons =
      let nodes = ref [] in
      List.iter (fun s -> match s with
        | E ((tag,attr), sons) when tag = "nd" -> 
          nodes := (List.assoc "ref" attr) :: !nodes
        | _ -> () 
      ) sons ;
      let attr = tags sons in
      let highway = attr "highway" and junction = attr "junction" in
      if is_road highway || is_road junction then begin
        let oneway = attr "oneway" in
        let reverse = oneway = "-1" in
        let oneway =
          (oneway = "yes" || oneway = "-1" || junction = "roundabout"
              || highway = "motorway" || highway = "motorway_link")
          && (oneway <> "no")
        in
        if oneway then Printf.printf "oneway %s\n" (attr "oneway") ;
        let urban = attr "is_in" <> "" in
        let maxspeed = attr "maxspeed" in
        let rec iter acc = function
          | [] | [_] -> acc
          | u :: v :: nodes ->
            let urban = urban
              || try
                   let lon, lat = Coord.get_loc coord u in
                   let lon', lat' = Coord.get_loc coord v in
                   let loc = (lon +. lon') /. 2., (lat +. lat') /. 2. in
                   let _ (* city, (name, loc', nb) *) = Grid.place_close loc in
                   true
                with Not_found -> false in
            let arc = u, v, (dist_id u v), (time highway maxspeed urban u v) in
            iter (arc :: acc) (v :: nodes)
        in
        let arcs = iter [] (List.rev_map int_of_string !nodes) in
        let rec reduc acc = function
          | [] -> acc
          | [arc] -> arc :: acc
          | (u,v,d,t) :: (u',v',d',t') :: arcs ->
            assert (v = u') ;
            let acc, arcs = 
              if Coord.mult coord v > 1 (* two roads at least cross at v *)
              then (u,v,d,t) :: acc, (u',v',d',t') :: arcs
              else acc, (u,v',d+.d',t+.t') :: arcs
            in reduc acc arcs
        in
        let arcs = reduc [] (List.rev arcs) in
        List.iter (fun (u,v,d,t) ->
          let print_link u v =
            Printf.printf "a %d %d %d %d\n" 
              u v (int_of_float (10. *. d)) (int_of_float (1000. *. t)) ;
          in
          if not (oneway && reverse) then print_link u v ;
          if (not oneway) || reverse then print_link v u ;
        ) (List.rev arcs)
      end
    in

    let way_nodes sons =
      let nodes = ref [] in
      List.iter (fun s -> match s with
        | E ((tag,attr), sons) when tag = "nd" -> 
          nodes := (List.assoc "ref" attr) :: !nodes
        | _ -> () 
      ) sons ;
      let attr = tags sons in
      let highway = attr "highway" and junction = attr "junction" in
      if is_road highway || is_road junction then begin
        List.iter (fun u -> Coord.add coord (int_of_string u)) !nodes ;
        (* Add again the first and last nodes to be sure they are considered
           as cross points : *)
        let rec iter first = function
          | [] -> ()
          | [u] -> Coord.add coord (int_of_string u)
          | u :: l ->
            if first then Coord.add coord (int_of_string u) ;
            iter false l
        in iter true !nodes
      end
    in

    let fname = Sys.argv.(1) in
    let nread = ref 0 and ntotal = ref (Debug.file_nlines fname) in

    let parse_first r = 
      incr nread ; 
      Debug.progress !nread !ntotal "Parsing xml" ;
      match r with
      | E ((tag,attr), sons) when tag = "node" -> ()
      | E ((tag,attr), sons) when tag = "way" -> way_nodes sons
      | _ -> assert false 
    in

    let parse_tree r = 
      incr nread ; 
      Debug.progress !nread !ntotal "Parsing xml" ;
      match r with
      | E ((tag,attr), sons) when tag = "node" -> node attr sons
      | E ((tag,attr), sons) when tag = "way" -> way sons
      | _ -> assert false 
    in

    let com = Printf.sprintf "bzcat %s" fname in
    let ci = Unix.open_process_in com in

    parse ci roots parse_first ;
    close_in ci ;
    Debug.store_nlines fname !nread ;
    Debug.progress_lap () ;

    ntotal := !nread ;
    nread := 0 ;
    let ci = Unix.open_process_in com in
    parse ci roots parse_tree ;
    close_in ci ;
    Debug.progress_lap () ;

    Debug.the_end () ;
    ()
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    flush stderr ;
    exit 2
