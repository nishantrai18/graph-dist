
(* int, int hashtable with open addressing *)

type t = {
  mutable tab : int Array.t ;
  mutable size : int ;
  mutable length : int ;
}

let null = min_int
let deleted = min_int + 1

let prime n =
  if n mod 2 = 0 then false else
    let rec iter d =
      if d * d > n then true else
        if n mod d = 0 then false else
          iter (d+2)
    in
    iter 3

let next_prime n =
  let n = if n mod 2 = 0 then n+1 else n in
  let rec iter n =
    if prime n then n 
    else iter (n+2)
  in 
  iter n

let make size = 
  let size = next_prime size in {
    tab = Array.make (2*size) null ;
    size = size ;
    length = 0 ;
  }


exception No_binding of int

let index_find t k =
  let rec iter j =
    let i = k + j * j mod t.size in
    let k' = t.tab.(2*i) in
    if k' = k then i
    else if k' = null then raise (No_binding i)
    else iter (j+1)
  in iter k 0

let index_free t k =
  try
    let i = index_find t k in
    let err = Printf.spritnf "key %d already binded with %d" k tab.(2*i+1) in
    invalid_arg err
  with No_binding i -> i


let add t k v =
  if k <= deleted then 
    invalid_arg "min_int and min_int+1 are not allowed as keys" ;
  let i = index_free t k in
  t.tab.(2*i) <- k ;
  t.tab.(2*i+1) <- v ;
  t.length <- t.length + 1 ;
  ()

let iter f t =
  for i = 0 to t.size - 1 do
    let k = t.tab.(2*i) in
    if k <> null then f k t.tab.(2*i+1)
  done 

let check_size t size =
  if t.length >= 3 * t.size / 4 then begin
    let size = next_prime (t.size + t.size/2) in
    let t' = make size in
    iter (add t') t ;
    Array.blit t.keys 0 keys 0 t.size ;
    t.keys <- keys ;
    let vals = Array.make size null in
    Array.blit t.vals 0 vals 0 t.size ;
    t.vals <- vals ;
  end
