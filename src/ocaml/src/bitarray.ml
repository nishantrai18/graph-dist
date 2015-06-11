
module B = Bigarray

type t = int * (int, B.int8_unsigned_elt, B.c_layout) B.Array1.t

let make n b =
  let n' = (n+7) / 8 in
  let t = B.Array1.create B.int8_unsigned B.c_layout n' in
  let b = if b then 1 else 0 in
  let b8 = ref 0 in
  for i=1 to 8 do b8 := (!b8 lsl 1) lor b done ;
  let b8 = !b8 in
  for i=0 to n'-1 do t.{i} <- b8 done ;
  n, t

let set (n,t) i b' =
  assert (i < n) ;
  let ic = i / 8 and ib = i mod 8 in
  let c = t.{ic} in
  let m = 1 lsl ib in
  let b = c land m <> 0 in
  if b' <> b then t.{ic} <- c lxor m

let get (n,t) i =
  assert (i < n) ;
  let ic = i / 8 and ib = i mod 8 in
  let c = t.{ic} in
  let m = 1 lsl ib in
  c land m <> 0


let unit () =
  let t = make 12 false in
  set t 7 true ;
  set t 9 true ;
  assert (get t 7) ;
  assert (not (get t 8)) ;
  assert (get t 9) ;
  assert (not (get t 10)) ;
  assert (try ignore (get t 13) ; false with _ -> true) ;
  assert (try ignore (set t 13 false) ; false with _ -> true) ;
  ()
