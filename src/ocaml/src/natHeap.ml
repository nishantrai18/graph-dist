(** A heap structure suited for Dijskstra algorithm.  Elements are indices
    (natural numbers) labelled with weights.  Elements are first said
    "unvisited". They are added to the heap with a given weight, this weight may
    decrease while elements are in the heap. While in the heap, they are said
    "members". The heap allows to remove in logarithmic time the element with
    smallest weight.  When an element is removed, its weight upon removal is
    stored. It is then said "visited".
*)

module type Vec = sig
  type t
  val make : ?capacity:int -> int -> bool * int * int -> t
  val size : t -> int
  val set_size : t -> int -> unit
  val get : t -> int -> bool * int * int
  val set : t -> int -> bool * int * int -> unit
  val index : t -> int -> int
  val set_index : t -> int -> int -> unit
  val label : t -> int -> int
  val set_label : t -> int -> int -> unit
  val marked : t -> int -> bool
  val set_marked : t -> int -> bool -> unit
  val null : int
  val max_label : int
  val fprintf : out_channel -> t -> unit
  val compare_label : int -> int -> int
end

module type S = sig
  type v
  type t
  val create : int -> t
  val is_empty : t -> bool
  val size : t -> int
  val unvisited : t -> int -> bool
  val member : t -> int -> bool
  val visited : t -> int -> bool
  val label : t -> int -> int
  val add : t -> int -> int -> unit
  val decr_label : t -> int -> int -> unit
  val update : t -> int -> int -> unit
  val min : t -> int * int
  val rem_min : t -> unit
  val pop_min : t -> int * int
  val label_vec : t -> v
end

module Make (V : Vec) : S with type v = V.t
  =
struct

  type v = V.t

  type t = { 
    data : v ; (* Elements of the heap with current weight as label. *)
    index : v ; (* Index in [data] and current weight as label, marked if in.*)
    visit : Bitarray.t ; (* Visited when removed. *)
  }

(* The heap is encoded in the array [data] as a binary tree: the sons
   of the element with index [i] have indices [2*i+1] and [2*i+2] resp.
   For each element, [index] stores if the element is member or visited,
   its index in [data] (when it is member), and its current weight.
   The index of an element in [data] is stored in [index].
   [n] is a strict upper bound on elements stored in the heap and is
   also the maximal number of elements that can be inserted in the heap.
   [size] is the number of elements in the heap.
   The memory usage of the structure is roughly [n + 2*max_size] words.
*)

  let create n =
    { 
      data = V.make 0 (false, V.null, V.max_label) ;
      index = V.make n (false, V.null, V.max_label) ;
      visit = Bitarray.make n false ;
    }

  let label_vec h = h.index

  let is_empty h = V.size h.data <= 0

  let size h = V.size h.data

  let unvisited h e = not (V.marked h.index e)

  let member h e = V.marked h.index e && not (Bitarray.get h.visit e)

  let visited h e = Bitarray.get h.visit e

  let label h e = V.label h.index e

  (* debug use *)
  let printf f ?index:(ind=false) h =
    V.fprintf f h.data ;
    if ind then V.fprintf f h.index ;
    flush f


  let unsafe_set h i e w =
    assert (not (visited h e)) ;
    V.set h.data i (true, e, w) ;
    V.set h.index e (true, i, w)

  let rec moveup h i e w =
    let pi = (i-1)/2 in
    if i <= 0 then unsafe_set h i e w else begin
      let _, pe, pw = V.get h.data pi in
      if i > 0 && V.compare_label w pw < 0 then begin
        unsafe_set h i pe pw ;
        moveup h pi e w ;
      end else 
        unsafe_set h i e w
    end

  let add h e w = 
    if not (unvisited h e) then 
      invalid_arg "add: element is already member or visited" ;
    let size = V.size h.data in
    V.set_size h.data (size+1) ;
    moveup h size e w

  let decr_label h e w =
    if not (member h e) then 
      invalid_arg "decr_label: element not in heap" ;
    if V.compare_label w (label h e) > 0 then
      invalid_arg "decr_label: greater label" ;
    moveup h (V.index h.index e) e w

  let update h e w =
    if unvisited h e then add h e w
    else begin
      if V.compare_label w (label h e) < 0 then
        moveup h (V.index h.data e) e w
    end


  let min h =
    if V.size h.data <= 0 then raise Not_found ;
    let _, e, w = V.get h.data 0 in
    e, w

  let rem_min h =
    if V.size h.data <= 0 then raise Not_found ;
    let _, m, w = V.get h.data 0 in
    Bitarray.set h.visit m true ; (* Element is now visited. *)
    let size = V.size h.data - 1 in
    let _, e, w = V.get h.data size in
    V.set_size h.data size ;

    let rec movedown i =
      let son = 2*i + 1 in
      if son < size then begin
        let son =
          let son' = son + 1 in
          if son' < size 
            && V.compare_label (V.label h.data son') (V.label h.data son) < 0 
          then son' else son
        in
        let sw = V.label h.data son in
        if V.compare_label sw w < 0 then begin
          unsafe_set h i (V.index h.data son) sw ;
          movedown son ;
        end else
          unsafe_set h i e w
      end else 
        unsafe_set h i e w
    in

    if size > 0 then movedown 0


  let pop_min h =
    let m = min h in
    rem_min h ;
    m

end

module V = struct
  module V = BoolIntInt.Vector(struct let low = 31 let high = 31 end)
  open V
  type t = V.t
  let make = make
  let size = size
  let set_size = set_size
  let get = get
  let set = set
  let index = high
  let set_index = set_high
  let label = low
  let set_label = set_low
  let marked = bool
  let set_marked = set_bool
  let null = max_low (* null node : not a valid index *)
  let max_label = max_high
  let fprintf = fprintf
  let compare_label l l' = l - l'
end


module H = Make (V)

let unit () =
  let h = H.create 15 in
  let l_orig = [2,5; 3,1; 4,15; 5,12; 6,7; 7,20; 8,16;  9,25; 10,30] in
  List.iter (fun (e,w) -> H.add h e w) ((1,50) :: l_orig) ;
  H.decr_label h 1 3 ;
  let l = ref [] in
  while not (H.is_empty h) do
    let e, w = H.pop_min h in
    (* Printf.printf "%d,%d\n" e w ; flush stdout ; *)
    l := (e, w) :: !l ;
  done ;
  let compare (_, w) (_, w') = V.compare_label w' w in
  assert (List.sort compare ((1,3) :: l_orig) = !l)
