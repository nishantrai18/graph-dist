(** A fairly compact graph implementation intended to manipulate big graphs.
    Nodes and edges are manipulated through indices. String nodes are
    also supported, a hashtable is then used to convert name to index.
    
    Each node can be associated to an integer label. By default, node indices
    and node labels are both stored in 31 bits unsigned integers. The maximum
    number of nodes in a graph is then [2^31-2] (the index [V.null = 2^31-1] is
    considered as invalid and serves as a null reference).

    Each edge can also be associated to an integer label. By default, edge
    indices and edges labels are stored in 35 bits and 27 bits resp. unsigned
    integers. The maximum number of edges in a graph is then [2^35-2] (again,
    the index [E.null = 2^35-1] is considered as invalid).

    Both nodes and edges can be marked or unmarked. Most operations can be
    restricted to marked nodes and edges through the optional argument
    [~mrk_restrict:Mrk_nodes_edges]. (With [~mrk_restrict:Mark_nodes], edge
    marks are ignored but nodes are considered only if marked.)  The complexity
    of algorithms remains a priori the same as on the full graph. To get faster
    execution on small subgraphs, consider [subgraph
    ~mrk_restrict:Mrk_nodes_edges].  By default, all nodes and edges are
    unmarked and operations ignore marks.

    The memory usage of a graph with [n] nodes and [m] edges is basically
    [n+m] 64 bits words which is [8(n+m)] bytes usually.
*)

module W = struct
  let compare w w' = w - w'
  let zero = 0
  let one = 1
  let bit_size = 31
  let infinity = -1 lsr (Sys.word_size - 1 - bit_size)
  let check w =
    if w < zero || w >= infinity then
      failwith "weight overflow"
  let sum w w' = 
    let s = w + w' in check s ; s
  let long_infinity = -1 lsr 1 
  let long_check w =
    if w < zero || w >= long_infinity then
      failwith "weight long_overflow"
  let long_sum w w' = 
    let s = w + w' in long_check s ; s
  let diff w w' = 
    let s = w - w' in check s ; s
  let of_string s =
    let w = int_of_string s in check w ; w
  let to_string = string_of_int
  let min w w' =
    if compare w w' <= 0 then w else w'
  let max w w' =
    if compare w w' >= 0 then w else w'
end

module VecLabIndex (S : BoolIntInt.Size) = struct
  module V = BoolIntInt.Vector(S)
  open V
  type t = V.t
  let make = make
  let size = size
  let set_size = set_size
  let get = get
  let set = set
  let push = push
  let pop = pop
  let peek = peek
  let get = get
  let index = high
  let index_lab v i = let _, ind, lab = get v i in ind, lab
  let set_index = set_high
  let set_index_lab v i (ind, lab) = set v i (bool v i, ind, lab)
  let label = low
  let set_label = set_low
  let marked = bool
  let set_marked = set_bool
  let null = max_high (* null node : not a valid index *)
  let max_label = max_low
  let fprintf = fprintf
  let compare_label l l' = l - l'
end

(** Vector of node indices. *)
module V = struct
  let bit_size = 
    let s = (Sys.word_size - 2) / 2 in assert (W.bit_size <= s) ; s
  (* Labels are also used to store indices in graph algorithms. *)
  module Vec = VecLabIndex (struct let low = bit_size let high = bit_size end) 
  include Vec
  let compare_label = W.compare
end

(** Vector of edges indices. *)
module E = VecLabIndex (struct let low = 27 let high = 35 end)

type t = {
  n : int ;
  m : int ;
  nodes : E.t ;
  edges : V.t ;
  (* If nodes are given by strings : *)
  name : int -> string ; 
  index : string -> int ;
  mutable sym : symmetric ;
} and symmetric = 
    | SYM (* graph is symmetric *)
    | GEN (* general oriented graph *)
    | REV of t (* general, reversed graph has been computed *)

(* ----------- nodes and edges ------------ *)

type mrk_restrict = 
  | Mrk_ignore (* all nodes and edges are considered (default) *)
  | Mrk_nodes (* only marked nodes are considered *)
  | Mrk_edges (* only marked edges are considered *)
  | Mrk_nodes_edges (* only marked nodes and marked edges are considered *)
  | Mrk_custom of (int -> bool) * (int -> bool)

let marked g u = E.marked g.nodes u

let mrk_node ?mrk_restrict:(mr=Mrk_ignore) g u =
  match mr with
    | Mrk_nodes | Mrk_nodes_edges -> E.marked g.nodes u
    | Mrk_custom (marked, _) -> marked u
    | Mrk_ignore | Mrk_edges -> true

let set_marked g u mrk = E.set_marked g.nodes u mrk

let edge_marked g e = V.marked g.edges e


let deg_sum g u = E.index g.nodes u

let name g u = g.name u

let index g s = g.index s

let label g u = E.label g.nodes u

let degree g u = 
  (deg_sum g (u+1)) - (deg_sum g u)

let index_max g = g.n - 1


let neighbor g u i = V.index g.edges (deg_sum g u + i)

let edge g e = V.index_lab g.edges e 

let mrk_edge ?mrk_restrict:(mr=Mrk_ignore) g e =
  let m, v, w = V.get g.edges e in
  let m = match mr with
    | Mrk_edges | Mrk_nodes_edges -> m
    | Mrk_custom (_, marked) -> marked e
    | Mrk_ignore | Mrk_nodes -> true
  in
  m, v, w

let edge_dest g e = V.index g.edges e

let edge_label g e = V.label g.edges e


(* ----------- scan nodes and edges ------------ *)

let n g = g.n

let m g = g.m


let iter_nodes ?mrk_restrict:(mr=Mrk_ignore) f g =
  for u = 0 to index_max g do
    if mrk_node ~mrk_restrict:mr g u then f u
  done

(* Assumes [u] is considered as marked. *)
let iter_neighb ?mrk_restrict:(mr=Mrk_ignore) f g u =
  for e = deg_sum g u to deg_sum g (u + 1) - 1 do
    let mrk, v, w = mrk_edge ~mrk_restrict:mr g e in 
    if mrk && mrk_node ~mrk_restrict:mr g v then f v w
  done

let iter_neighb_rev ?mrk_restrict:(mr=Mrk_ignore) f g u =
  for e = deg_sum g (u + 1) - 1 downto deg_sum g u do
    let mrk, v, w = mrk_edge ~mrk_restrict:mr g e in 
    if mrk && mrk_node ~mrk_restrict:mr g v then f v w
  done

let edge_iter ?mrk_restrict:(mr=Mrk_ignore) f g =
  iter_nodes ~mrk_restrict:mr (fun u ->
    iter_neighb ~mrk_restrict:mr (f u) g u
  ) g

let fold_nodes ?mrk_restrict:(mr=Mrk_ignore) f g a0 =
  let rec fold u a =
    if u < 0 then a else begin
      let a = if mrk_node ~mrk_restrict:mr g u then f u a else a in
      fold (pred u) a
    end
  in
  fold (index_max g) a0

let fold_edges ?mrk_restrict:(mr=Mrk_ignore) f g a0 =
  fold_nodes ~mrk_restrict:mr (fun u a ->
    let e0 = deg_sum g u in
    let rec fold e a =
      if e < e0 then a else 
        let mrk, v, w = mrk_edge ~mrk_restrict:mr g e in
        let a = if mrk then f u v w a else a in
        fold (pred e) a
    in
    fold (deg_sum g (u+1) - 1) a
  ) g a0


let set_marked_all g mrk = 
  iter_nodes (fun u -> set_marked g u mrk) g



(* ----------- edge sorting ------------ *)

let sort t l r = (* quick sort part of an array of edges *)
  let edge_compare (m,v,w) (m',v',w') =
    let cv = v - v' in
    if cv <> 0 then cv else 
    let cw = V.compare_label w w' in
    if cw <> 0 then cw else compare m m'
  in
  let swap i j =
    assert (not (V.marked t i || V.marked t j)) ;
    let tmp = V.get t i in
    V.set t i (V.get t j) ; V.set t j tmp ;
  in
  let edge_compare i j = edge_compare (V.get t i) (V.get t j) in
  let partition l r =
    let l0 = l and r0 = r in
    let ipiv = (l + r) / 2 in
    swap ipiv r0 ;
    let ipiv = r0 in
    let l = ref l and r = ref (pred r) in
    while !l < !r do
      while !l < r0 && edge_compare !l ipiv < 0 do incr l done ;
      while !r > l0 && edge_compare !r ipiv > 0 do decr r done ;
      if !l < !r then begin
        swap !l !r ;
        decr r ;
      end
    done ;
    if edge_compare !l ipiv < 0 then incr l ;
    swap !l ipiv ;
    !l
  in
  let rec sort l r =
    if l < r then begin
      let j = partition l r in
      sort l (pred j) ;
      sort (succ j) r ;
    end
  in
  sort l r


(* Returns the minimum index of an edge u -> v. *)
let edge_index ?mrk_restrict:(mr=Mrk_ignore) g u v = 
  let rec dicho l r =
    if l > r then raise Not_found else
    if l = r then begin
      let mrk, v', _ = mrk_edge ~mrk_restrict:mr g l in 
      if mrk && v' = v then l else raise Not_found
    end else begin
      let m = (l + r) / 2 in
      let l', r' = 
        let v', _ = edge g m in
        if v' < v then m + 1, r 
        else if v' > v then l, m
        else (* v' = v *) begin
          let m = ref m in
          while 
            !m > 0 && let v', _ = edge g (!m - 1) in 
                      v' = v
          do decr m done ;
          while 
            !m < g.m - 1 && let mrk, v', w = mrk_edge ~mrk_restrict:mr g !m in 
                            v' = v && not mrk 
          do incr m done ;
          !m, !m
        end
      in
      dicho l' r'
    end 
  in
  if not (mrk_node ~mrk_restrict:mr g u) then raise Not_found ;
  dicho (deg_sum g u) (deg_sum g (u+1) - 1)

let edge_weight ?mrk_restrict:(mr=Mrk_ignore) ?infinity:(infty=false) g u v =
  try
    edge_label g (edge_index ~mrk_restrict:mr g u v)
  with Not_found ->
    if infty then W.infinity else raise Not_found

let has_edge ?mrk_restrict:(mr=Mrk_ignore) g u v =
  try ignore (edge_weight ~mrk_restrict:mr g u v) ; true
  with Not_found -> false


(* ----------- graph creation ------------ *)

(* [make false op_n _ edge_iter] builds the graph in two passes on edges :
   first compute degrees and then store edges in memory. [edge_iter f] should
   call [f u v w_uv] for each edge [u -> v] with weight [w_uv] where [u], [v]
   and [w_uv] are strings. If [op_n] is [Some en], then it used as an
   estimation of [n] to pre-allocate data-structures. Otherwise a value of [10]
   is assumed.

   [make true ind_max edge_iter sit] is used when nodes are small integers
   (integers in the range [0..O(n)]). [edge_iter f] should call
   [f u v w_uv] for each edge [u -> v] with weight [w_uv] where [u], [v] and 
   [w_uv] are ints. If [ind_max] is [Some im], then the structure will have
   size to cope with node indices up to [im]. If [ind_max] is [None], then
   an additional preliminary pass on edges is maded to get the highest index 
   [im] of a node.
*)
let make sym from_ints ind_max edge_iter_int edge_iter_string =

  (* First pass : compute degrees. *)
  let index_max, index, name, nodes =  

    if from_ints then begin

      let n = match ind_max with None -> 10 | Some im -> im in
      let degrees = Hashtbl.create n in
      let index_max = ref 0 in
      let count_edge u v _ =
        let deg = try Hashtbl.find degrees u with Not_found -> 0 in
        Hashtbl.replace degrees u (deg+1) ;
        if not (Hashtbl.mem degrees v) then Hashtbl.add degrees v 0 ;
        if u > !index_max then index_max := u ;
        if v > !index_max then index_max := v ;
      in

      edge_iter_int count_edge ;

      let nodes = E.make (!index_max + 2) (false, 0, 0) in
      Hashtbl.iter (fun u deg ->
        E.set_index nodes u deg
      ) degrees ;

      !index_max, int_of_string, string_of_int, nodes

    end else begin (* from strings *)

      let n = match ind_max with None -> 10 | Some im -> im in
      let degrees = Hashtbl.create n in
      let names_are_int = ref true in
      let name_min = ref max_int and name_max = ref min_int in 
      let check_int u =
        if !names_are_int then
          try 
            let i = int_of_string u in
            if i < 0 || i >= E.null then failwith "index_overflow" ;
            if i < !name_min then name_min := i ;
            if i > !name_max then name_max := i ;
          with Failure _ ->
            names_are_int := false
      in
      let count_edge u v _ =
        let deg = try Hashtbl.find degrees u with Not_found -> 0 in
        Hashtbl.replace degrees u (deg+1) ;
        if not (Hashtbl.mem degrees v) then Hashtbl.add degrees v 0 ;
        check_int u ; check_int v ;
      in

      edge_iter_string count_edge ;

      let n = Hashtbl.length degrees in
      if n = 0 then failwith "no edge read !!! empty graph ???" ;
      let from_ints = !names_are_int && !name_min >= 0 && !name_max <= 2*n in
      let index_max = if from_ints then !name_max else n in

      let nodes = E.make (index_max + 2) (false, 0, 0) in
      let next_index = ref 1 in
      Hashtbl.iter (fun u deg ->
        if from_ints then
          E.set_index nodes (int_of_string u) deg
        else begin
          E.set_index nodes !next_index deg ;
          Hashtbl.replace degrees u !next_index ; (* reuse table for index *)
          incr next_index ;
        end
      ) degrees ;
      
      if from_ints
      then index_max, int_of_string, string_of_int, nodes
      else begin
        let indexes = degrees in
        let names = Array.make (index_max + 1) "" in
        Hashtbl.iter (fun u i -> names.(i) <- u) indexes ;
        for i=0 to index_max do
          if names.(i) = "" then names.(i) <- Printf.sprintf "node_%d" i ;
        done ;
        index_max, Hashtbl.find indexes, Array.get names, nodes
      end

    end

  in

  assert (index_max < V.null) ;
  let n = index_max + 1 in

  (* sum degrees *)
  let sum = ref 0 in
  for u = 0 to n do
    let d = E.index nodes u in
    (* node labels are used temporarily to store the number of edges read *)
    E.set_index nodes u !sum ;
    if d > E.max_label then failwith "degree overflow: do not fit in a label" ;
    sum := !sum + d ;
  done ;
  let m = !sum in
  assert (m <= E.null) ;

  (* Second pass : read edges. *)
  let edges = V.make m (false, V.null, W.infinity) in
  let add_int u v w_uv =
    let i = E.index nodes u and di = E.label nodes u in
    V.set edges (i + di) (false, v, w_uv) ;
    E.set_label nodes u (di + 1) ;
  in
  let add_string u v w_uv =
    let u = index u and v = index v in
    let w_uv = int_of_string w_uv in
    add_int u v w_uv
  in
  if from_ints then edge_iter_int add_int else edge_iter_string add_string ;

  (* Check and sort. *)
  for u = 0 to index_max do
    let i = E.index nodes u  and di = E.label nodes u in
    if i + di <> E.index nodes (u+1) then 
      failwith "Degree changed compared to first pass." ;
    sort edges i (i + di - 1) ;
    E.set_label nodes u 0 ;
  done ;

  { n = n ;
    m = m ;
    nodes = nodes ;
    edges = edges ;
    index = index ;
    name = name ;
    sym = sym ;
  }

let make_int ?symmetric:(sym=GEN) ?index_max:(ind_max=None) edge_iter =
  make sym true ind_max edge_iter (fun _ -> assert false)

let make_string ?symmetric:(sym=GEN) ?index_max:(ind_max=None) edge_iter =
  make sym false ind_max (fun _ -> assert false) edge_iter



(* ----------- output graph ------------ *)

let fprintf ?mrk_restrict:(mr=Mrk_ignore) f g =
  iter_nodes ~mrk_restrict:mr (fun u ->
    if degree g u > 0 then begin
      Printf.fprintf f "%s ->" (name g u) ;
      iter_neighb ~mrk_restrict:mr (fun v w ->
        Printf.fprintf f " %s,%s" (name g v) (W.to_string w) ;
      ) g u ;
      Printf.fprintf f "\n" ;
    end
  ) g

let print ?mrk_restrict:(mr=Mrk_ignore) = 
  fprintf ~mrk_restrict:mr stdout

let edge_compare (u,v,w) (u',v',w') =
  let cu = u - u' in
  if cu <> 0 then cu else
  let cv = v - v' in
  if cv <> 0 then cv else  
  W.compare w w' 

let to_list_int g =
  fold_edges (fun u v w l -> (u,v,w) :: l) g []

let edge_compare_string g (u,v,w) (u',v',w') =
  let index_compare u u' = (index g u) - (index g u') in
  let cu = index_compare u u' in
  if cu <> 0 then cu else
  let cv = index_compare v v' in
  if cv <> 0 then cv else  
  W.compare (W.of_string w) (W.of_string w') 

let to_list_string g =
  fold_edges (fun u v w l -> (name g u, name g v, W.to_string w) :: l) g []



(* ------------ Stats ---------------- *)

let symmetric_edges g =
  fold_edges (fun u v w nb ->
    try
      if w = edge_weight g v u then nb + 1 else nb
    with Not_found -> nb
  ) g 0 

let symmetric_edges_unweighted g =
  fold_edges (fun u v w nb ->
    if has_edge g v u then nb + 1 else nb
  ) g 0 

(* ------------ Graph algorithms ---------------- *)

(* Beware that indexes are usually reassigned. *)
let subgraph ?mrk_restrict:(mr=Mrk_ignore) g =
  let iter f = edge_iter ~mrk_restrict:mr  (fun u v w -> f u v w) g in
  let new_index = Hashtbl.create 16 in
  let index_max = ref (-1) in
  let add u = 
    if not (Hashtbl.mem new_index u) then begin
      incr index_max ;
      Hashtbl.add new_index u !index_max
    end 
  in
  iter (fun u v w -> add u ; add v) ;
  let index u = Hashtbl.find new_index u in
  let iter f = iter (fun u v w -> f (index u) (index v) w) in
  let g' = make GEN true (Some !index_max) iter (fun _->assert false) in
  let index_orig = Array.make g'.n V.null in
  Hashtbl.iter (fun u nu -> index_orig.(nu) <- u) new_index ;
  let name u = g.name index_orig.(u) in
  let index s = index (g.index s) in
  { n = g'.n ;
    m = g'.m ;
    nodes = g'.nodes ;
    edges = g'.edges ;
    name = name ;
    index = index ;
    sym = match g.sym with SYM -> SYM | _ -> GEN ;
  }

let induced_subgraph g = subgraph ~mrk_restrict:Mrk_nodes g 

(* Same vertex set with same indices. *)
let edge_subgraph g = subgraph ~mrk_restrict:Mrk_edges g 


let reverse g =
  match g.sym with
    | SYM -> g
    | REV g' -> g'
    | GEN ->
      let iter f = edge_iter (fun u v w -> f v u w) g in
      let g' = make GEN true (Some (index_max g)) iter (fun _->assert false) in
      let g' = { n = g'.n ;
                 m = g'.m ;
                 nodes = g'.nodes ;
                 edges = g'.edges ;
                 name = g.name ;
                 index = g.index ;
                 sym = REV g ;
               } in
      g.sym <- REV g' ;
      g'


let symmetrize g =
  match g.sym with
    | SYM -> g
    | _ ->
      let iter f = 
        edge_iter (fun u v w -> f u v w) g ;
        edge_iter (fun u v w -> 
          if not (edge_weight ~infinity:true g v u = w) then f v u w
        ) g ;
      in
      let g' = make GEN true (Some (index_max g)) iter (fun _->assert false) in
      { n = g'.n ;
        m = g'.m ;
        nodes = g'.nodes ;
        edges = g'.edges ;
        name = g.name ;
        index = g.index ;
        sym = SYM ;
      }

(* Asserts no multiple edges. *)
let check_symmetric g =
  let sym = fold_edges (fun u v w sym ->
    sym && edge_weight ~infinity:true g v u = w
  ) g true in
  if sym then g.sym <- SYM ;
  sym


let unweighted g =
  let iter f = edge_iter (fun u v w -> f u v W.one) g in
  let g' = make GEN true (Some (index_max g)) iter (fun _->assert false) in
  { n = g'.n ;
    m = g'.m ;
    nodes = g'.nodes ;
    edges = g'.edges ;
    name = g.name ;
    index = g.index ;
    sym = match g.sym with SYM -> SYM | _ -> GEN ;
  }


(* ------- Connectivity --------- *)


let connected_components g =

  let g' = reverse g in
  let comp = V.make g.n (false, V.null, V.max_label) in
  let cnb = ref 0 in

  let fifo = Queue.create () in
  let nedges = ref 0 in

  let bfs u =
    Queue.add (u, V.null) fifo ;
    while Queue.length fifo > 0 do
      let u, p = Queue.take fifo in
      V.set comp u (true, p, !cnb) ;
      let bfs v _ = 
        incr nedges; Debug.progress !nedges (g.m+g'.m) "Connected_components" ;
        if not (V.marked comp v) 
        then (V.set_marked comp v true ; Queue.add (v, u) fifo)
      in
      iter_neighb bfs g u ;
      iter_neighb bfs g' u ;
    done
  in

  for u=0 to index_max g do
    if not (V.marked comp u) then (bfs u ; incr cnb)
  done ;

  Debug.progress_lap () ;
  comp, (* all marked, parent in BFS traversal of comp, comp_nb *)
  !cnb (* number of components *)


let component_number (comp, _) u = V.label comp u

let component_total_number (_, nb) = nb

let components_to_list (comp, nb) =
  let al = Array.make nb [] in
  for u = V.size comp - 1 downto 0 do
    let c = V.label comp u in
    al.(c) <- u :: al.(c) ;
  done ;
  Array.to_list al

let mark_component g (comp, _) i_comp =
  iter_nodes (fun u ->
    set_marked g u (V.label comp u = i_comp)
  ) g

exception Break

let component_sizes (comp, nb) =
  let index_max = V.size comp - 1 in
  let size = Array.make nb 0 in
  for u = 0 to index_max do
    let c = V.label comp u in
    size.(c) <- size.(c) + 1 ;
  done ;
  size

let node_in_largest_component (comp, nb) =
  let size = component_sizes (comp, nb) in
  let cmax = ref 0 and smax = ref size.(0) in
  for c = 0 to nb-1 do
    Debug.distr "comp_size" size.(c) ; 
    if size.(c) > !smax then (cmax := c ; smax := size.(c))
  done ;
  let index_max = V.size comp - 1 in
  let umax = ref 0 in
  (try for u = 1 to index_max do
    if V.label comp u = !cmax then (umax := u ; raise Break) 
    done with Break -> ()) ;
  !umax

let component_iter f (comp, nb) =
  let size = component_sizes (comp, nb) in
  let visited = Array.make nb false in
  let index_max = V.size comp - 1 in
  for u = 0 to index_max do
    let c = V.label comp u in
    if not visited.(c) then begin
      f c u size.(c) ;
      visited.(c) <- true ;
    end
  done

(***** TODO may be
let induced_subgraph g member =
  let n = ref 0 in
  for u=0 to index_max g do
    if V.label comp u = nb_comp then begin
      incr n ;
    end 
  done ;
  let n = m = ref 0 in
  for u=0 to index_max g do
    if V.label comp u = nb_comp then begin
      incr n ;
      iter_neighb (fun v _ ->
        if V.label comp v = nb_comp then 
          incr m ;
      ) g u ;
    end 
  done ;
  ()

let subgraph_component g (comp, _) nb_comp =
  induced_subgraph g (fun u -> V.label comp u = nb_comp)
*****)

let strongly_connected_components g =

  (* number in dfs visit and lowlink (i.e. minimal number of a node 
     reachable through a sequence of forward edges plus a back edge) *)
  let vis_low = V.make g.n (false, V.null, V.max_label) and nb = ref 0 in
  let unvisited u = V.index vis_low u = V.null in
  let number u = V.index vis_low u in
  let dfs_todo = V.make 0 (false, V.null, V.max_label) in

  let stack = V.make 0 (false, V.null, V.max_label) in
  let in_stack u = V.marked vis_low u in (* in stack and visit not finished *)
  let comp_nb = ref 0 in
  let pop_scc u =
    (try while true do
      let _, v, _ = V.pop stack in
      V.set_marked vis_low v false ; (* not in stack anymore *)
      V.set_label vis_low v !comp_nb ; (* reuse label for commp nb *)
      if v = u then raise Break ;
    done with Break -> ()) ;
    incr comp_nb ;
  in
  let nedges = ref 0 in

  let dfs u =
    V.push dfs_todo (true, u, 0) ; (* first time, u, label is next neighbor *)
    let lowlink = ref V.null in (* lowlink of node visited previously *)
    while V.size dfs_todo > 0 do
      let start_visit, u, i = V.pop dfs_todo in
      (* begin visit of [u] *)
      if start_visit then begin 
        lowlink := !nb ;
        V.set vis_low u (true, !nb, !nb) ; (* marked, number, lowlink *)
        incr nb ;
        V.push stack (false, u, V.max_label) ;
      end else begin
        (* !lowlink is lowlink of son whose visit has just finished *)
        lowlink := min !lowlink (V.label vis_low u) ;
      end ;
      try
        (* continue visit of [u] *)
        for i = i to degree g u - 1 do
          let v = neighbor g u i in
          incr nedges ; 
          Debug.progress !nedges g.m "Strongly_connected_components" ; 
          if unvisited v then begin (* forward edge *)
            V.push dfs_todo (false, u, i+1) ; (* keep until visit terminated *)
            V.set_label vis_low u !lowlink ;
            V.push dfs_todo (true, v, 0) ; 
            raise Break
          end else (* back edge *) 
            if in_stack v then lowlink := min !lowlink (number v)
        done ;
        (* end of visit of [u] *)
        if !lowlink = number u then pop_scc u
      with Break -> ()
    done
  in

  for u=0 to index_max g do
    if unvisited u then ignore (dfs u)
  done ;

  Debug.progress_lap () ;
  vis_low, (* all marked, number in DFS traversal, comp_nb *)
  !comp_nb


(* ------- Distances --------- *)


module Heap = NatHeap.Make (V)

let dijkstra_list ?marked:(mrk=None) g src_list =
  let marked = match mrk with Some v -> V.marked v | _-> fun _ -> true in
  List.iter (fun s ->
    if not (marked s) then 
      Debug.info "WARNING!!!! dijkstra: source %s is not marked" (name g s)
  ) src_list ;
  let parent = V.make g.n (false, V.null, W.infinity) in
  List.iter (fun s -> V.set_marked parent s true) src_list ;
  (* let ordering = V.make ~capacity:g.n 0 (false, V.null, W.infinity) in *)
  let h = Heap.create g.n in
  List.iter (fun s -> if marked s then Heap.add h s W.zero) src_list ;
  let dist = Heap.label_vec h in (* reuse heap vector *)
  let nedges = ref 0 and nb = ref 0 and last = ref V.null in

  while not (Heap.is_empty h) do
    let u, d = Heap.pop_min h in

    V.set_index dist u !nb ; (* label is already d *)
    last := u ;
    incr nb ;

    iter_neighb (fun v w ->
      incr nedges ; Debug.progress !nedges g.m "Dijkstra" ;
      if marked v && not (Heap.visited h v) then begin 
        let dv' = W.sum d w in
        if Heap.member h v then begin
          let dv = Heap.label h v in
          if dv' < dv then begin
            Heap.decr_label h v dv' ;
            V.set parent v (true, u, w) ;
          end
        end else begin
          Heap.add h v dv' ;
          V.set parent v (true, u, w) ;
        end ;
      end
    ) g u ;

  done ;

  Debug.progress_lap () ;
  Debug.info "Last: %s at dist %d" (name g !last) (V.label dist !last) ; 
  dist, (* marked if reachable from [src_list], nb of visit, dist *)
  parent, (* mrk. if reach., parent [p] in traversal, weight of edge from [p] *)
  !last

let dijkstra ?marked:(mrk=None) g s = dijkstra_list ~marked:mrk g [s]

let path g parent s t =
  let rec path acc t =
    if t = s then acc
    else begin
      let p, w = V.index_lab parent t in
      path ((p, t, w) :: acc) p
    end
  in
  path [] t 

let path_length p =
  List.fold_left (fun len (_,_,w) -> W.sum len w) W.zero p

let visit_order (dist, _, last) =
  let nlast = V.index dist last in
  let visit = Array.make (nlast+1) V.null in
  for u=0 to V.size dist - 1 do
    if V.marked dist u then begin
      assert (visit.(V.index dist u) = V.null) ;
      visit.(V.index dist u) <- u
    end
  done ;
  visit

(* SumSweep heristic from :
   Michele Borassi, Pierluigi Crescenzi, Michel Habib, Walter Kosters, 
   Andrea Marino, and Frank Takes [FUN 2014]. *)

type sweep_summary = 
{
  node : int ;
  ecc : int ; last : int ;
  ecc' : int ; last' : int ;
}

let diameter_scc ?diam_only:(diam_only=false)
                 g u_scc = 
                                   (* of the strgly con. c. of [u_scc] *)
  let g' = reverse g in
  (* lower, upper out excentricity bounds, marked if lower < upper : *)
  let ecc = V.make g.n (true, W.zero, W.infinity) in 
  (* lower, upper in excentricity bounds, marked if lower < upper : *)
  let ecc' = V.make g.n (true, W.zero, W.infinity) in
  let sum = Array.make g.n W.zero in (* sum of dist to sweep sources *)
  let sum' = Array.make g.n W.zero in (* sum of dist from sweep sources *)
  let diamLB = ref W.zero and diamUB = ref W.infinity in
  let radLB = ref W.zero and radUB = ref W.infinity in
  let rad'LB = ref W.zero and rad'UB = ref W.infinity in

  (* Recall best sweeps : *)
  let dum = { node=u_scc; ecc=0; last=u_scc; ecc'=0; last'=u_scc; } in
  let sw_diam = ref dum and sw_diam' = ref dum in
  let dum = { node=u_scc; ecc=W.infinity; last=u_scc; 
              ecc'=W.infinity; last'=u_scc; } in
  let sw_rad = ref dum and sw_rad' = ref dum in

  (* Consider only the strongly connected component of u. *)
  let first_sweep = ref true and scc_size = ref g.n in

  let update s dist dist' =
    let s_ecc = ref W.zero and last = ref s in
    let s_ecc' = ref W.zero and last' = ref s in

    for u=0 to index_max g do
      let dist_mrk, _, dist_s_u = V.get dist u in (* dist s --> u *)
      let dist'_mrk, _, dist'_u_s = V.get dist' u in (* dist u --> s *)
      if dist_mrk && dist'_mrk then begin (*otherwise not in scc *)
        (* Excentricity of s in scc *)
        if W.compare dist_s_u !s_ecc > 0 then 
          (s_ecc := dist_s_u ; last := u) ;
        if W.compare dist'_u_s !s_ecc' > 0 then 
          (s_ecc' := dist'_u_s ; last' := u) ;
      end else if !first_sweep then begin
        V.set_marked ecc u false ;
        V.set_marked ecc' u false ;
        decr scc_size
      end
    done ;
    first_sweep := false ;

    let s_ecc = !s_ecc and last = !last in
    let s_ecc' = !s_ecc' and last' = !last' in
    diamLB := max !diamLB (max s_ecc s_ecc') ;
    radUB := min !radUB s_ecc ;
    rad'UB := min !rad'UB s_ecc' ;
    let sw_sum = { node=s ; ecc=s_ecc; last=last; ecc'=s_ecc'; last'=last'; } in
    if s_ecc > !sw_diam.ecc then sw_diam := sw_sum ;
    if s_ecc' > !sw_diam'.ecc' then sw_diam' := sw_sum ;
    if s_ecc < !sw_rad.ecc then sw_rad := sw_sum ;
    if s_ecc' < !sw_rad'.ecc' then sw_rad' := sw_sum ;

    for u=0 to index_max g do
      let dist_mrk, _, dist_s_u = V.get dist u in (* dist s --> u *)
      let dist'_mrk, _, dist'_u_s = V.get dist' u in (* dist u --> s *)
      if dist_mrk && dist'_mrk then begin (*otherwise not in scc *)
        (* Bound outward eccentricity of u *)
        let mrk, ecc_u_lb, ecc_u_ub = V.get ecc u in
        let lb = W.max ecc_u_lb dist'_u_s in
        let ub = W.min ecc_u_ub (W.sum dist'_u_s s_ecc) in
        if W.compare lb ecc_u_lb > 0 || W.compare ub ecc_u_ub < 0 then 
          V.set ecc u (mrk, lb, ub)  ;
        (* Bound inward eccentricity of u *)
        let mrk', ecc'_u_lb, ecc'_u_ub = V.get ecc' u in
        let lb' = W.max ecc'_u_lb dist_s_u in
        let ub' = W.min ecc'_u_ub (W.sum s_ecc' dist_s_u) in
        if W.compare lb' ecc'_u_lb > 0 || W.compare ub' ecc'_u_ub < 0 then 
          V.set ecc' u (mrk', lb', ub')  ;
        (* Sums of distances to source nodes of all sweeps so far : *)
        let s, s' = sum.(u), sum'.(u) in
        let s, s' = W.long_sum s dist'_u_s, W.long_sum s' dist_s_u in
        sum.(u) <- s ; sum'.(u) <- s' ;
      end ;
    done ;

    (* longest u' --> s --> u gives an upper bound of the diameter,
       consider only u' s.t. eccUB > diamLB and u s.t. ecc'UB > diamLB *)
    (* min_u ecc_LB(u) for u marked is a lower bound of radius, idem for rad' *)
    let eccM_ub = ref W.zero and ecc'M_ub = ref W.zero in
    let eccM_lb = ref W.infinity and ecc'M_lb = ref W.infinity in
    let mrk = ref 0 and mrk' = ref 0 in
    let mrkD = ref 0 and mrk'D = ref 0 in
    let bnd_diff = ref 0 and bnd_diff' = ref 0 in

    for u=0 to index_max g do
      let m', ecc'_u_lb, ecc'_u_ub = V.get ecc' u in
      if m' then begin
        if W.compare ecc'_u_lb ecc'_u_ub < 0 
        then (incr bnd_diff'; if W.compare ecc'_u_lb !rad'UB < 0 then incr mrk')
        (* Nasty heurisitic : else V.set_marked ecc' u false *) ;
        ecc'M_lb := W.min !ecc'M_lb ecc'_u_lb ; 
        if W.compare ecc'_u_ub !diamLB > 0 then 
          (incr mrk'D ; 
           eccM_ub := W.max !eccM_ub (V.label dist u)) ; (* dist s --> u *)
      end ;
    done ;
    for u=0 to index_max g do
      let m, ecc_u_lb, ecc_u_ub = V.get ecc u in
      if m then begin
        if W.compare ecc_u_lb ecc_u_ub < 0 
        then (incr bnd_diff; if W.compare ecc_u_lb !radUB < 0 then incr mrk)
        (* Nasty heuristic : else V.set_marked ecc u false *) ;
        eccM_lb := W.min !eccM_lb ecc_u_lb ; 
        if W.compare ecc_u_ub !diamLB > 0 then 
          (incr mrkD ; 
           ecc'M_ub := W.max !ecc'M_ub (V.label dist' u)) ; (* dist u' --> s *)
      end ;
    done ;
    let d = 
      if !ecc'M_ub = W.infinity || !eccM_ub = W.infinity then W.infinity
      else W.sum !ecc'M_ub !eccM_ub
    in
    diamUB := W.min !diamUB d ;
    radLB := W.max !radLB !eccM_lb ;
    rad'LB := W.max !rad'LB !ecc'M_lb ;
    Debug.info 
      "s_ecc'=%d(from %s) s_ecc=%d(to %s) ecm'=%d ecm=%d ecmLB'=%d ecmLB=%d    \
       mrk'=%d mrk=%d mrk'D=%d mrkD=%d diff'=%d diff=%d scc:%d"
      s_ecc' (name g last') s_ecc (name g last) 
      !ecc'M_ub !eccM_ub  !ecc'M_lb !eccM_lb 
      !mrk' !mrk !mrk'D !mrkD !bnd_diff !bnd_diff' !scc_size ;
    (* mrk... correspond to number of candidates for center', center, diam_src,
       diam_dst. *)
    
  in
  
  let extremEcc maxUB forward backward =
    (* if not maxUB, then look for node with minimum lower bound *)
    let compare w w' = if maxUB then W.compare w w' else W.compare w' w in
    let zero = if maxUB then W.zero else W.infinity in
    let m = ref u_scc and s = ref zero in
    let bnd = ref zero in
    for u=0 to index_max g do
      let mrk, lb, ub = V.get ecc u in
      let mrk', lb', ub' = V.get ecc' u in
      let mrk, u_bnd, u_sum = match forward, backward with
        | true, false -> mrk, (if maxUB then ub else lb), sum.(u)
        | false, true -> mrk', (if maxUB then ub' else lb'), sum'.(u)
        | true, true -> 
          let s, s' = sum.(u), sum'.(u) in
          if mrk && mrk' then 
            true, (if maxUB then W.sum ub' ub else W.sum lb' lb), 
            W.long_sum s' s
          else
            false, zero, zero
        | _ -> invalid_arg "extremEcc"
      in
      let cmp = compare u_bnd !bnd in
      if mrk && cmp >= 0
        && (cmp > 0 || compare u_sum !s > 0)
      then (m := u ; bnd := u_bnd ; s := u_sum)
    done ;
    !m
  in

  let nsweeps = ref 0 in
  let sweep ?sweep_name:(sname="") s =
    incr nsweeps ;
    Debug.info "\nsweep %d %s from %s" !nsweeps sname (name g s) ;
    if not (V.marked ecc s || V.marked ecc' s) 
    then Debug.info "skip marked !!!!!!!!" else begin
      let mrk, lb, ub = V.get ecc s in
      let mrk', lb', ub' = V.get ecc' s in
      Debug.info "Bounds %s : ecc':%b,%d,%d ecc:%b,%d,%d" 
        (name g s) mrk' lb' ub' mrk lb ub ; 
      V.set_marked ecc s false ;
      V.set_marked ecc' s false ;
      let dist, prt, last = dijkstra g s in
      let dist', _, _ = 
        if g.sym = SYM then dist, prt, last else dijkstra g' s in
      update s dist dist' ;
      Debug.info "diameter %s: %d <= diam <= %d  \
                    %d <= rad <= %d  %d <= rad' <= %d" 
        sname !diamLB !diamUB !radLB !radUB !rad'LB !rad'UB ;
      if !diamLB >= !diamUB && diam_only then raise Break ;
      if !diamLB >= !diamUB && !radLB >= !radUB && !rad'LB >= !rad'UB then 
        raise Break ; 
(* if !nsweeps >= 2 then begin
  for u=0 to index_max g do
      let m, ecc_u_lb, ecc_u_ub = V.get ecc u in
      if m then begin
        let diff = ecc_u_ub - ecc_u_lb in
        Debug.distr "diff" (100*diff/ecc_u_ub) ;
        Debug.distr "deg" (degree g u)
      end ;
    done ;
  raise Break ;
end *) 
    end
  in

  let first = ref true in
  (try while !diamLB < !diamUB || !radLB < !radUB || !rad'LB < !rad'UB do
    let s = 
      if !first then (first := false ; u_scc) 
      else extremEcc false true false in
    sweep ~sweep_name:"min ecc" s ;
    let s = extremEcc true false true in
    sweep ~sweep_name:"max ecc'" s ;
    let s = extremEcc false true true in
    sweep ~sweep_name:"min ecc'+ecc" s ;
    let s = extremEcc true true false in
    sweep ~sweep_name:"max ecc" s ;
    let s = extremEcc false false true in
    sweep ~sweep_name:"min ecc'" s ;
    let s = extremEcc true true true in
    sweep ~sweep_name:"max ecc'+ecc" s ;
  done with Break -> ()) ;
  Debug.info "diameter: %d sweeps  (%d = %d)" !nsweeps !diamLB !diamUB ;
  Debug.info "diam : ecc(%s)=%d ecc'(%s)=%d"
    (name g !sw_diam.node) !sw_diam.ecc (name g !sw_diam'.node) !sw_diam'.ecc' ;
  Debug.info "rad : ecc(%s)=%d,%d ecc'(%s)=%d,%d"
    (name g !sw_rad.node) !sw_rad.ecc (!sw_rad.ecc + !sw_rad.ecc')
    (name g !sw_rad'.node) !sw_rad'.ecc' (!sw_rad'.ecc + !sw_rad'.ecc') ;

  !diamLB, !radUB, !rad'UB, (!sw_diam, !sw_diam', !sw_rad, !sw_rad')


let unit () =
  let l = 
    [1,2,1; 1,3,10; 2,4,10; 1,2,0; 3,4,100; 1,2,1; 1,5,1; 2,4,1; 2,1,1;] in
  let iter l f = List.iter (fun (u,v,w_uv) -> f u v w_uv) l in
  let g = make_int (iter l) in
  (* print g ; *) 
  assert (to_list_int g = List.sort edge_compare l) ;
  assert (edge_weight g 1 2 = 0) ;
  assert (edge_weight g 1 3 = 10) ;
  assert (edge_weight g 1 5 = 1) ;
  assert (edge_weight g 2 4 = 1) ;
  assert (edge_weight g 3 4 = 100) ;
  let l = List.map (fun (u,v,w_uv) -> 
    Printf.sprintf "n%d" u, Printf.sprintf "n%d" v, string_of_int w_uv
  ) l in
  let iter' l f = List.iter (fun (u,v,w_uv) -> f u v w_uv) l in
  let g' = make_string (iter' l) in
  (* Printf.printf "----\n" ;
  print (reverse g') ; *)
  assert (to_list_string g' = List.sort (edge_compare_string g') l) ;

  let neighb u l = List.map (fun v -> u,v,1) l in
  let l = [
    neighb 1 [2] ;
    neighb 2 [23] ;
    neighb 3 [18] ;
    neighb 4 [] ;
    neighb 5 [1; 12] ;
    neighb 6 [21] ;
    neighb 7 [13] ;
    neighb 8 [11] ;
    neighb 9 [7] ;
    neighb 10 [6] ;
    neighb 11 [9; 13] ;
    neighb 12 [5] ;
    neighb 13 [15] ;
    neighb 14 [2] ;
    neighb 15 [1; 8] ;
    neighb 16 [4; 14] ;
    neighb 17 [7] ;
    neighb 18 [19] ;
    neighb 19 [3] ;
    neighb 20 [9; 13] ;
    neighb 21 [] ;
    neighb 22 [20] ;
    neighb 23 [7; 12; 17] ;
    neighb 24 [] ;
    neighb 25 [17; 20; 22; 23] ;
  ] in
  let l = List.flatten l in
  let g = make_int (iter l) in

  let ccs = connected_components g in
  (* print g ; 
   let lccs = components_to_list ccs in 
    List.iter (fun l ->
    List.iter (fun u -> Printf.printf "%d " u) l ;
    Printf.printf "\n------------\n" ;
  ) lccs ; *)
  assert (component_number ccs 7 = component_number ccs 13) ;
  assert (component_number ccs 7 = component_number ccs 25) ;
  assert (component_number ccs 7 = component_number ccs 22) ;
  assert (component_number ccs 7 <> component_number ccs 19) ;
  assert (component_number ccs 7 <> component_number ccs 21) ;
  assert (component_number ccs 7 <> component_number ccs 24) ;
  assert (component_number ccs 3 = component_number ccs 19) ;
  assert (component_number ccs 10 = component_number ccs 21) ;

  let ccs = strongly_connected_components g in
  (* print g ; let lccs = components_to_list ccs in 
    List.iter (fun l ->
    List.iter (fun u -> Printf.printf "%d " u) l ;
    Printf.printf "\n----------------\n" ;
  ) lccs ; *)
  assert (component_number ccs 7 = component_number ccs 8) ;
  assert (component_number ccs 7 = component_number ccs 12) ;
  assert (component_number ccs 7 = component_number ccs 9) ;
  assert (component_number ccs 7 <> component_number ccs 14) ;
  assert (component_number ccs 7 <> component_number ccs 22) ;
  assert (component_number ccs 7 <> component_number ccs 25) ;
  assert (component_number ccs 12 = component_number ccs 5) ;
  assert (component_number ccs 3 = component_number ccs 19) ;

  mark_component g ccs (component_number ccs 7) ;
  (* print g ; Printf.printf "-----------\n" ; 
     print ~all_marked:(false, true) g ; Printf.printf "-----------\n" ; *)
  let g' = subgraph ~mrk_restrict:Mrk_nodes g in
  (* print g' ; Printf.printf "-----------\n" ; *)
  let g_index u = g.index (g'.name u) in
  edge_iter (fun u v _ -> 
    assert (has_edge ~mrk_restrict:Mrk_nodes g (g_index u) (g_index v))
  ) g' ;
  let g'_index u = g'.index (g.name u) in
  edge_iter ~mrk_restrict:Mrk_nodes
    (fun u v _ -> assert (has_edge g' (g'_index u) (g'_index v))) g ;

  let diam, rad, rad', _ = diameter_scc g 1 in
  (* Printf.printf "diameter: %d  radius: %d   radius': %d\n" diam rad rad' ; 
  flush stdout ; *)
  assert ((diam, rad, rad') = (10, 5, 4)) ;

  let diam, rad, rad', _ = diameter_scc g' (g'_index 1) in
  assert ((diam, rad, rad') = (10, 5, 4)) ;

  flush stdout ;
  ()
    
