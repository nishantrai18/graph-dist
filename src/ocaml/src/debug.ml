type t = NOTHING | PROG | INFO

let level = ref PROG

let to_string d = 
  match d with NOTHING -> "no" | PROG -> "prog" | INFO -> "info"

let of_string s =
  match s with
    | "no" -> NOTHING | "none" -> NOTHING | "nothing" -> NOTHING 
    | "yes" -> PROG | "info" -> INFO
    | _ -> failwith (Printf.sprintf "Debug: unknown verbosity '%s'" s)

let set_verbosity s = 
  level := of_string s

let verbosity () = 
  to_string !level

let start_t = Sys.time ()
let lap_t = ref start_t
let progress_t = ref start_t

let mem_usage () =
  let mem =
    8 * let s = Gc.quick_stat() in s.Gc.heap_words + s.Gc.stack_size in
  let mem, unit = 
    if mem < 10000 then mem, "b"
    else if mem < 10000000 then mem/1000, "k"
    else if mem < 10000000000 then mem/1000000, "m"
    else mem/1000000000, "g" 
  in
  Printf.sprintf "%d%s" mem unit

let progress ndone ntotal fmt =
  let after str =
    if !level <> NOTHING then
      let now = Sys.time () in
      if (ntotal > 0 && ndone = ntotal) || now >= !progress_t +. 10.
        || (now >= !progress_t +. 1. && now <= !lap_t +. 30.)
        || (now >= !progress_t +. 0.1 && now <= !lap_t +. 1.) then begin
          let t = now -. !lap_t in
          let rest = 
            if ntotal < 0 || ndone <= 0 then (Printf.sprintf " in %.0fs" t) else
              let todo = 
                (float_of_int (ntotal - ndone))/.(float_of_int ndone)*.t in
              if t >= 10. then
                Printf.sprintf " / %d : %d%% in %.0fs + %.0fs"
                  ntotal (ndone * 100 / ntotal) t todo 
              else
                Printf.sprintf " / %d : %d%% in %.1fs + %.1fs"
                  ntotal (ndone * 100 / ntotal) t todo 
          in
          Printf.eprintf "%s (%d%s, %s)\r" 
            str ndone rest (mem_usage ()) ; 
          flush stderr ;
          progress_t := now
        end
  in
  Printf.ksprintf after fmt

let progress_lap () =
  if !level <> NOTHING then begin
    if !progress_t >= !lap_t +. 0.1 then (Printf.eprintf "\n" ; flush stderr) ;
    let now = Sys.time () in
    lap_t := now ;
    progress_t := now ;
  end


let info fmt =
  if !level = INFO then begin
    let now = Sys.time () in
    let time_ellapsed =
      let total = now -. start_t in
      let progress = now -. !progress_t in
      if progress < 0.1 then Printf.sprintf "%.2fs / %.0fs" progress total
      else if progress < 1. then Printf.sprintf "%.1fs / %.0fs" progress total
      else Printf.sprintf "%.0fs / %.0fs" progress total
    in
    let after f =
      Printf.fprintf f " (%s, %s)\n" time_ellapsed (mem_usage ()) ;
      flush f ;
      progress_t := now ;
    in
    Printf.kfprintf after stderr fmt
  end else 
    Printf.ikfprintf (fun _ -> ()) stderr fmt

let file_nlines f =
  let fn = Printf.sprintf "%s.nlines" f in
  if Sys.file_exists fn then begin
    let i = open_in fn in
    let n = int_of_string (input_line i) in
    close_in i ;
    n
  end else -1

let store_nlines f n =
  let fn = Printf.sprintf "%s.nlines" f in
  let o = open_out fn in
  Printf.fprintf o "%d\n" n ;
  close_out o


module StringMap = Map.Make(struct type t = string let compare = compare end)

let counts = ref StringMap.empty

let count (id : string) occured =
  if !level <> NOTHING then begin
    let noc, ntot = try StringMap.find id !counts with Not_found -> (0,0) in 
    counts := StringMap.add id ((if occured then noc+1 else noc), ntot+1) !counts
  end

module IntMap = Map.Make(struct type t = int let compare = compare end)
let distrs = ref StringMap.empty

let distr (id : string) v =
  if !level <> NOTHING then begin
    let vals = 
      try StringMap.find id !distrs 
      with Not_found -> 
        let vals = ref IntMap.empty in
        distrs := StringMap.add id vals !distrs ;
        vals
    in
    let nbocc = try IntMap.find v !vals with Not_found -> 0 in
    vals := IntMap.add v (nbocc+1) !vals
  end

let distr_to_string vals n =
  let percents = 
    [0; 1; 2; 5; 10; 20; 30; 40; 50; 60; 70; 80; 90; 95; 98; 99; 100;] in
  let p_i = ref percents and p_v = ref percents in
  let p_to_int p min max = 
    match p with
      | [] -> max + 1
      | p :: _ -> min + (max - min) * p / 100
  in
  let p_next p = match p with [] -> [] | _ :: p -> p in
  let cum = ref [] and i = ref 0 in
  let iv = ref 0 and nv = IntMap.cardinal vals in
  let vmin, _ = IntMap.min_binding vals and vmax, _ = IntMap.max_binding vals in
  IntMap.iter (fun v nbocc -> 
    i := !i + nbocc ;
    incr iv ;
    if !i >= p_to_int !p_i 0 n  || v >= p_to_int !p_v vmin vmax
      || !iv = 1 || !iv = 2 || !iv = 3
      || !iv = nv || !iv = n-1 || !iv = n-2 then begin
        cum := (v,(float_of_int !i)*.100./.(float_of_int n)) :: !cum ;
        while !i >= p_to_int !p_i 0 n do p_i := p_next !p_i done ;
        while v >= p_to_int !p_v vmin vmax do p_v := p_next !p_v done ;
      end
  ) vals ;
  let buf = Buffer.create 50 in
  List.iter (fun (v,p) ->
    Printf.bprintf buf "%d,%.2f%% " v p
  ) (List.rev !cum) ;
  Buffer.contents buf

let the_end () =
  if !level <> NOTHING then begin
    flush stdout ;
    Printf.eprintf "\nStat distrs (x,p%% means p%% of vals are at most x) :\n" ;
    StringMap.iter (fun id vals ->
      let n = IntMap.fold (fun _ nbocc sum -> sum + nbocc) !vals 0 in
      let nd = IntMap.cardinal !vals in
      Printf.eprintf "   %s %d vals (%d distinct): %s\n" id n nd (distr_to_string !vals n)
    ) !distrs ;
    Printf.eprintf "\n" ; flush stderr ;

    Printf.eprintf "\nStat counts :\n" ;
    StringMap.iter (fun id (noc, ntot) ->
      Printf.eprintf "%s %d / %d (%d%%)\n" id noc ntot (100 * noc / ntot)
    ) !counts ;
    flush stderr ;

    Printf.eprintf "Total time : %.0fs\n" (Sys.time () -. start_t) ;
  end ;
  flush stderr


