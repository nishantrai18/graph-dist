(** Compact storage of 2 small natural integers and a boolean in an int.
*)

module type Size = sig
  val low : int
  val high : int
end

module Make(S : Size) = struct

  let () = assert (S.low >= 0 && S.high >= 0 
                   && S.low + S.high <= Sys.word_size - 2) 

  type t = int

  let max_low = -1 lsr (Sys.word_size - 1 - S.low)
  let check_low i =
    if (i < 0 || i > max_low) then invalid_arg "low"

  let max_high = -1 lsr (Sys.word_size - 1 - S.high)
  let check_high i =
    if (i < 0 || i > max_high) then invalid_arg "high"

  let of_bii b h l =
    check_high h ; check_low l ;
    let b = if b then 1 else 0 in
    (b lsl (Sys.word_size - 2)) + (h lsl S.low) + l

  let bool t = (t lsr (Sys.word_size - 2)) = 1

  let high t = (t lsr S.low) land max_high

  let low t = t land max_low

  let to_bii t =
    bool t, high t, low t


  let mask_bool = (max_high lsl S.low) + max_low

  let set_bool t b =
    let b = if b then 1 else 0 in
    (t land mask_bool) + (b lsl (Sys.word_size - 2))

  let mask_high = (1 lsl (Sys.word_size - 2)) + max_low

  let set_high t h =
    check_high h ;
    (t land mask_high) + (h lsl S.low)

  let mask_low = (1 lsl (Sys.word_size - 2)) + (max_high lsl S.low)

  let set_low t l =
    check_low l ;
    (t land mask_low) + l

end      


module Vector (S : Size) = struct

  module BII = Make (S)
  open BII
  type t = {
    b0 : bool ; h0 : int ; l0 : int ;
    mutable data : BII.t array ;
    mutable size : int ;
  }

  let make ?capacity:(cap=16) n (b, h, l) = 
    if n < 0 || cap < 1 then invalid_arg "create" ;
    let capacity = max n cap in
    {
      b0 = b ; h0 = h ; l0 = l ;
      data = Array.make capacity (of_bii b h l)  ;
      size = n ;
    }

  let size v = v.size

  let set_size v n =
    if n > Array.length v.data then begin
      let n =
        if n <= 8192 then 2 * n else
          if n <= 314928 then (3 * n) / 2 else
            (6 * n) / 5 
      in
      let a = Array.make n (of_bii v.b0 v.h0 v.l0) in
      Array.blit v.data 0 a 0 v.size ;
      v.data <- a ;
    end ;
    v.size <- n

  let push v (b, h, l) =
    let n = v.size in
    set_size v (n + 1) ;
    v.data.(n) <- of_bii b h l

  let pop v =
    let n = v.size - 1 in
    if n < 0 then invalid_arg "pop: empty" ;
    let b, h, l = to_bii v.data.(n) in
    v.size <- n ;
    b, h, l

  let peek v =
    let n = v.size - 1 in
    if n < 0 then invalid_arg "peek: empty" ;
    to_bii v.data.(n)


  let check_index v i =
    if i < 0 || i >= v.size then
      invalid_arg "index out of bounds in vector"

  let get v i = check_index v i ; to_bii v.data.(i)
  let set v i (b, h, l) = check_index v i ; v.data.(i) <- of_bii b h l

  let bool v i = check_index v i ; bool v.data.(i)
  let high v i = check_index v i ; high v.data.(i)
  let low v i = check_index v i ; low v.data.(i)

  let set_bool v i b = check_index v i ; v.data.(i) <- set_bool v.data.(i) b
  let set_high v i h = check_index v i ; v.data.(i) <- set_high v.data.(i) h
  let set_low v i l = check_index v i ; v.data.(i) <- set_low v.data.(i) l

  let max_low = max_low
  let max_high = max_high

  let fprintf f v =
    for i=0 to v.size - 1 do
      Printf.fprintf f "%d,%d%s " (high v i) (low v i)  
        (if bool v i then "*" else "")
    done ;
    Printf.fprintf f "\n" ;
    ()

end


module BII = Make (struct let high = 20 let low = 9 end)
open BII


let unit () =
  let t = of_bii false 0 0 in
  let t = set_high t max_high in
  let t = set_bool t true in
  let t = set_low t 456 in
  assert (bool t = true) ;
  assert (high t = max_high) ;
  assert (low t = 456) ;
  assert (to_bii t = (true, max_high, 456)) ;
  ()
