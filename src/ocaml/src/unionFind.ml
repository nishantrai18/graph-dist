
module B = Bigarray

type t = int * (int, B.int32_elt, B.c_layout) B.Array1.t

let make n =
  let t = B.Array1.create B.int32 B.c_layout n in
  for i=0 to n-1 do
    t.{i} <- Int32.of_int i ;
  done ;
  t

let get t u = Int32.to_int t.{u}

let set t u p = t.{u} <- Int32.of_int p

let find t u =
  let rec find u =
    let p = get t u in
    if p = u then u else find p
  in
  let c = find u in
  let rec write u =
    let p = get t u in
    if p = u then () else (set t u c ; write p)
  in
  write u ;
  c

let union t u v =
  let cu = find t u and cv = find t v in
  set t cu cv



let unit () =
  let t = make 12 in
  union t 2 3 ;
  union t 4 5 ;
  union t 6 7 ;
  union t 1 2 ;
  union t 1 5 ;
  assert (find t 3 = find t 4) ;
  assert (find t 6 = find t 7) ;
  assert (find t 5 <> find t 6) ;
  assert (find t 10 = 10) ;
  ()

