
module B = Bigarray

type t = {
  index : (int, int) Hashtbl.t ;
  mutable loc : (float, B.float64_elt, B.c_layout) B.Array1.t ;
  mutable mult : (int, B.int16_unsigned_elt, B.c_layout) B.Array1.t ;
  mutable size : int ;
}

let make size = {
  index = Hashtbl.create size ;
  loc = B.Array1.create B.float64 B.c_layout (2 * size) ;
  mult = B.Array1.create B.int16_unsigned B.c_layout size ;
  size = size ;
}

let check_size t i =
  if i >= t.size then begin
    let size = max (i+1) (t.size + t.size/4) in
    let loc = B.Array1.create B.float64 B.c_layout (2 * size) in
    for j=0 to 2*t.size-1 do 
      loc.{j} <- t.loc.{j} ;
    done ;
    t.loc <- loc ;
    let mult = B.Array1.create B.int16_unsigned B.c_layout size in
    for j=0 to t.size-1 do 
      mult.{j} <- t.mult.{j} ;
    done ;
    t.mult <- mult ;
    t.size <- size ;
  end
  
let index_nb = ref (-1)

let index t id =
  try
    Hashtbl.find t.index id
  with e -> Printf.eprintf "%d\n" id ; raise e

let iter f t =
  Hashtbl.iter (fun id _ -> f id) t.index

let add t id =
  if not (Hashtbl.mem t.index id) then begin 
    incr index_nb ;
    Hashtbl.add t.index id !index_nb ;
    check_size t !index_nb ;
    t.mult.{!index_nb} <- 1 ;
  end else begin
    let ind = index t id in
    t.mult.{ind} <- t.mult.{ind} + 1 ;
  end

let mem t id =
  Hashtbl.mem t.index id

let mult t id =
  try
    let ind = index t id in
    t.mult.{ind}
  with Not_found -> 0


let set_loc t id (lon, lat) =
  let i = index t id in
  check_size t i ;
  t.loc.{2*i} <- lon ;
  t.loc.{2*i+1} <- lat ;
  ()

let get_loc t id =
  let i = index t id in
  t.loc.{2*i}, t.loc.{2*i+1}
    

