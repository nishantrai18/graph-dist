
let earth_radius = 6371000.

let pi = atan2 0. (-1.0)
let rad d = d /. 180. *. pi

let dist (lon1, lat1) (lon2, lat2) =
  let lon1, lat1, lon2, lat2 = rad lon1, rad lat1, rad lon2, rad lat2 in
  let dlon = lon2 -. lon1 in
  let dlat = lat2 -. lat1 in
  let a = (sin(dlat /. 2.))**2. +. 
    cos(lat1) *. cos(lat2) *. (sin(dlon /. 2.))**2. in
    (* let c = 2. *. atan2 (sqrt a) (sqrt (1. -. a)) in *)
  let c = 2. *. asin (sqrt a) in (* haversine formula *)
  earth_radius *. c

let microdeg_per_radian = 180. /. pi *. 1000000.
let microdeg_dist x r = int_of_float (microdeg_per_radian *. x /. r)
let microdeg lat = int_of_float (1000000. *. lat)

type place = string * (float * float) * int (* nb of cities around *)

type t = {
  cells : (int * int, place) Hashtbl.t ;
  step : float ; (* meters *)
  stepy : int ; 
}

let make step = { 
  cells = Hashtbl.create 1024 ; 
  step = step ; 
  stepy = microdeg_dist step earth_radius ;
}

  (* Buggy when lon around 180° or lat around 90°,-90° *)
let cell t (lon, lat) =
  let rx = abs_float (earth_radius *. cos (rad lat)) in
  let stepx = microdeg_dist t.step rx in
  let lon, lat = microdeg lon, microdeg lat in
  let lon = lon / stepx and lat = lat / t.stepy in
  lon, lat

let add t city_name loc =
  let loc' = cell t loc in
  let n = 
    try
      let _, _, n = Hashtbl.find t.cells loc' in n
    with _ -> 0 in
  Hashtbl.replace t.cells loc' (city_name, loc, n+1)


exception Break of place

let close t loc =
  let lon, lat = cell t loc in
  let deltas = [0; -1; 1;] in
  try
    List.iter (fun dx ->
      List.iter (fun dy ->
        try
          let p = Hashtbl.find t.cells (lon + dx, lat + dy) in
          raise (Break p)
        with Not_found -> ()
      ) deltas ;
    ) deltas ;
    raise Not_found
  with Break p -> p


let place_radii = [("city", 10000.); ("town", 5000.); ("village", 1000.);
                   ("hamlet", 300.); ("suburb", 300.)]

let place_is_city city = List.mem_assoc city place_radii

let places = 
  List.map (fun (city, radius) -> (city, make (radius /. 2.))) place_radii

let place_add city name loc =
  try
    let t = List.assoc city places in
    add t name loc
  with Not_found -> ()

let place_close loc =
  let rec iter = function
    | [] -> raise Not_found
    | (city, t) :: places ->
      try city, close t loc with Not_found -> iter places
  in
  iter places

