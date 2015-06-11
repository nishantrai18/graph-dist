
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
  mutable n : int ;
  mutable m : int ;
  mutable nodes : 
  mutable out_adj : V.t array ;
  mutable in_adj : V.t array ;
  mutable name : string array ;
  index : (string, int) Hashtbl.t ;
} and symmetric = 
    | SYM (* graph is symmetric *)
    | GEN (* general oriented graph *)
    | REV of t (* general, reversed graph has been computed *)
