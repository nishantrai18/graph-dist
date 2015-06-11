
module G = StaticGraph

let dimacs_graph fname f =
  let line s = 
    match E.String.split_n 4 s with
      | ["a"; u; v; d] -> 
        f u v d
        | _ -> ()
  in
  Input.line_read line fname

let dimacs_coordinates index fname =
  let coord = Coord.make 1024 in
  let line s = 
    match E.String.split_n 4 s with
      | ["v"; u; lon; lat] -> 
        let f s = (float_of_string s) /. 1000000. in 
        let lon, lat = f lon, f lat in
        let u = index u in
        Coord.add coord u ;
        Coord.set_loc coord u (lon, lat) ;
      | _ -> ()
  in
  Input.line_read line fname ;
  coord
  
let edges_graph ?default_weight:(dft_wgt="1") fname f =
  let line s = 
    let s = E.String.trim s in
    match E.String.split_space_all s with
      | "#" :: _ -> (* comment *)
        () 
      | [u; v;] -> 
        f u v dft_wgt
      | [u; v; d;] -> 
        f u v d
      | l -> 
        ()
  in
  Input.line_read line fname    


let snap_graph ?comment:(com='#') ?default_weight:(dft_wgt="1") fname f =
  let line s = 
    if s <> "" && s.[0] <> com then
    match E.String.split_all ~separator:'\t' ~separator':'\r' s with
      | [u; v;] -> 
        f u v dft_wgt
      | [u; v; d;] -> 
        f u v d
      | l -> 
        ()
  in
  Input.line_read line fname    


let the_graph = ref (G.make_int (fun _ -> ()))

let index g u_name = 
  if u_name = "random" then Random.int (G.n g)
  else G.index g u_name 

let coms = [
  "random", "[seed] : initialize random generator with seed [seed]",
  (fun () ->
    Random.init (TrivialArg.int "seed")
  );

  "read", "[format] [file] : read a graph from a file\
           (formats : edges, snap or dimacs)",
  (fun () ->
    let format = TrivialArg.string "format" in
    let fname = TrivialArg.string "file" in
    the_graph := begin match format with
      | "edges" -> G.make_string (edges_graph fname) ;
      | "snap" -> G.make_string (snap_graph fname) ;
      | "dimacs" -> G.make_string (dimacs_graph fname) ;
      | _ -> invalid_arg "format" ;
    end ;
    Debug.info "Read_graph n=%d m=%d m_sym=%d,%d" 
      (G.n !the_graph) (G.m !the_graph) 
      (G.symmetric_edges_unweighted !the_graph)
      (G.symmetric_edges !the_graph)
  );

  "symmetrize", ": make the graph symmetric",
  (fun () ->
    let g = !the_graph in
    the_graph := G.symmetrize g ;
    Debug.info "Symmetrize n=%d m=%d" (G.n !the_graph) (G.m !the_graph)
  );

  "unweighted", ": set edges weights to 1",
  (fun () ->
    let g = !the_graph in
    the_graph := G.unweighted g ;
    Debug.info "Unweighted n=%d m=%d" (G.n !the_graph) (G.m !the_graph)
  );

  "subgraph_scc", "[node_name] : restrict the graph to the strongly \
                   connected component of [node_name]",
  (fun () ->
    let u_name = TrivialArg.string "node_name" in
    let g = !the_graph in
    let scc = G.strongly_connected_components g in
    let u = index g u_name in
    G.mark_component g scc (G.component_number scc u) ;
    the_graph := G.induced_subgraph g ;
    Debug.info "Subgraph_scc_of %s n=%d m=%d"
      u_name (G.n !the_graph) (G.m !the_graph)
  ) ;    

  "diameter_scc", "[node_name] : compute the diameter of the strongly \
                   connected component of [node_name]",
  (fun () ->
    let u_name = TrivialArg.string "node_name" in
    let g = !the_graph in
    let u = index g u_name in
    Debug.info "Diameter_scc of %s(%s)" u_name (G.name g u) ;
    let diam, rad, rad', _ = G.diameter_scc g u in
    Printf.printf "%d %d %d\n" diam rad rad' ; flush stdout ;
    Debug.info "Diameter_scc of %s(%s) : %d" u_name (G.name g u) diam
  ) ;    

  "dijkstra", "[node_name] : make a Dijkstra traversal from node [node_name]",
  (fun () ->
    let u_name = TrivialArg.string "node_name" in
    let g = !the_graph in
    let u = index g u_name in
    let dist, _, last = G.dijkstra g u in
    let dlast = G.V.label dist last in
    Printf.printf "%d %d %d\n" u last dlast ; flush stdout ;
    (* for u=0 to G.index_max g do
      Debug.distr "dist" (G.V.label dist u) ;
    done ; *)
    Debug.info "Dijkstra from %s : eccentricity=%d" u_name dlast
  );
    
  "dijkstra_bulk", "[start_index] [end_index] [i_core] [nb_core] : \
                    make Dijkstra traversals in parallel",
  (fun () ->
    let start_index = TrivialArg.int "start_index" in
    let end_index = TrivialArg.int "end_index" in
    let i_core = TrivialArg.int "i_core" in
    let nb_core = TrivialArg.int "nb_core" in
    let g = !the_graph in
    if i_core < 0 || i_core >= nb_core then 
      invalid_arg "Dijkstra_bulk" ;
    let end_index = min end_index (G.index_max g) in
    let todo = ((end_index + 1 - start_index) + nb_core - 1) / nb_core in
    let s_start = start_index + i_core * todo in
    let s_end = min end_index (s_start + todo - 1) in
    let m = ref s_start and l = ref s_start and dm = ref G.W.zero in
    for s=s_start to min s_end (G.index_max g) do
      let dist, _, last = G.dijkstra g s in
      let dlast = G.V.label dist last in
      Printf.printf "%d %d %d\n" s last dlast ;
      flush stdout ;
      if dlast > !dm then (m := s ; l := last ; dm := dlast) ;
      Debug.info "Dijkstra_bulk : %d / %d" 
        (s + 1 - s_start) (s_end + 1 - s_start) ;
    done ;
    Debug.info "Dijkstra_bulk : max for %d %d %d" !m !l !dm
  );

  "diameter", ": compute the diameter of the largest strongly connected \
               component",
  (fun () ->
        let g = !the_graph in
        let ccs = G.strongly_connected_components g in
        let u = G.node_in_largest_component ccs in
        let diam, rad, rad', _ = G.diameter_scc g u in
        Printf.printf "%d %d %d\n" diam rad rad' ; flush stdout ;
        Debug.info "Misc diameter of scc fo %s : %d" (G.name g u) diam
  );

  "dijkstra_histo", ": count long branches in a Dijkstra traversal from \
                     a node in the largest strongly connected component",
  (fun () ->
    let g = !the_graph in
    let ccs = G.strongly_connected_components g in
    let u = G.node_in_largest_component ccs in
        (*        let u = Random.int (G.n g) in *)
    let d,p,l = G.dijkstra g u in
    let visit = G.visit_order (d,p,l) in
    let n = Array.length visit in
    let nsons = Array.make (G.n g) 0 in
    for i = 1 to n - 1 do
      let v = visit.(i) in
      let p = G.V.index p v in
      nsons.(p) <- nsons.(p) + 1
    done ;
    let desc = Array.make (G.n g) 0 in
    for i = n - 1 downto 1 do
      let v = visit.(i) in
      if nsons.(v) = 0 then desc.(v) <- G.V.label d v ;
      let p = G.V.index p v in
      desc.(p) <- max desc.(p) desc.(v)
    done ;
    let long_node v =
      let dist_v = G.V.label d v in
      desc.(v) >= 2 * dist_v
    in
    let nsons_long = Array.make (G.n g) 0 in
    for i = 1 to n - 1 do
      let v = visit.(i) in
      if long_node v then
        let p = G.V.index p v in
        nsons_long.(p) <- nsons_long.(p) + 1
    done ;
        (* Scan verticies by increasing distance : *)
    let nbranches = ref 1 and nbranches_long = ref 1 in
    for i = 0 to n - 1 do
      let v = visit.(i) in
      Printf.printf "%d\t%d\t%d\t%d\n" 
        v (G.V.label d v) !nbranches !nbranches_long ;
      nbranches := !nbranches - 1 + nsons.(v) ;
      if long_node v then decr nbranches_long ;
      if long_node v && nsons_long.(v) = 0 then
        Printf.printf "long leaf %d dist=%d\n" v (G.V.label d v) ;
      nbranches_long := !nbranches_long + nsons_long.(v) ;
    done
  );

  "center_tree", "[node_name] [coord_file] : compute the main branches \
                  of the shortest path tree \
                  from node [node_name] with coordinates in [coord_file] \
                  (each line has format [v node_name long*10^6 lat*10^6])",
  (fun () ->
    let g = !the_graph in
    let u = TrivialArg.string "node_name" in
    let coord = TrivialArg.string "coord_file" in
    let u = G.index g u in
    let d,p,l = G.dijkstra g u in
    let visit = G.visit_order (d,p,l) in
    let n = Array.length visit in
    let nsons = Array.make (G.n g) 0 in
    for i = 1 to n - 1 do
      let v = visit.(i) in
      let p = G.V.index p v in
      nsons.(p) <- nsons.(p) + 1
    done ;
    let desc = Array.make (G.n g) 0 in
    let desc_node = Array.make (G.n g) G.V.null in
    for i = n - 1 downto 1 do
      let v = visit.(i) in
      if nsons.(v) = 0 then begin
        desc.(v) <- G.V.label d v ;
        desc_node.(v) <- v ;
      end ;
      let p = G.V.index p v in
      if desc.(v) > desc.(p) then begin
        desc.(p) <- desc.(v) ;
        desc_node.(p) <- desc_node.(v) ;
      end
    done ;
    let long_node v =
      let dist_v = G.V.label d v in
      desc.(v) >= 2 * dist_v
    in
    let nsons_long = Array.make (G.n g) 0 in
    for i = 1 to n - 1 do
      let v = visit.(i) in
      if long_node v then
        let p = G.V.index p v in
        nsons_long.(p) <- nsons_long.(p) + 1
    done ;
    let coord = dimacs_coordinates (G.index g) coord in
    let print u = 
      let lon, lat = Coord.get_loc coord u in
      Printf.printf "%d,%f,%f,%d,%d\n" u lon lat (G.V.label d u) desc.(u)
    in
        (* Scan verticies by increasing distance : *)
    let nbranches = ref 1 and nbranches_long = ref 1 in
    for i = 0 to n - 1 do
      let v = visit.(i) in
      if long_node v then begin
        print v ;
        if nsons_long.(v) = 0 then begin
          let path = G.path g p v desc_node.(v) in
          List.iter (fun (_,u,_) -> print u) path
        end
      end ;
      nbranches := !nbranches - 1 + nsons.(v) ;
      if long_node v then decr nbranches_long ;
      nbranches_long := !nbranches_long + nsons_long.(v) ;
    done
  );

  "tree", "[node_name] [coord_file] : compute the shortest path tree \
                  from node [node_name] with coordinates in [coord_file] \
                  (each line has format [v node_name long*10^6 lat*10^6])",
  (fun () ->
    let g = !the_graph in
    let u = TrivialArg.string "node_name" in
    let coord = TrivialArg.string "coord_file" in
    let u = G.index g u in
    let d,p,l = G.dijkstra g u in
    let u_root = u in
    let visit = G.visit_order (d,p,l) in
    let n = Array.length visit in
    let nsons = Array.make (G.n g) 0 in
    for i = 1 to n - 1 do
      let v = visit.(i) in
      let p = G.V.index p v in
      nsons.(p) <- nsons.(p) + 1
    done ;
    let desc = Array.make (G.n g) 0 in
    let desc_node = Array.make (G.n g) G.V.null in
    for i = n - 1 downto 1 do
      let v = visit.(i) in
      if nsons.(v) = 0 then begin
        desc.(v) <- G.V.label d v ;
        desc_node.(v) <- v ;
      end ;
      let p = G.V.index p v in
      if desc.(v) > desc.(p) then begin
        desc.(p) <- desc.(v) ;
        desc_node.(p) <- desc_node.(v) ;
      end
    done ;
    let long_node v =
      let dist_v = G.V.label d v in
      desc.(v) >= 2 * dist_v
    in
    let nsons_long = Array.make (G.n g) 0 in
    for i = 1 to n - 1 do
      let v = visit.(i) in
      if long_node v then
        let p = G.V.index p v in
        nsons_long.(p) <- nsons_long.(p) + 1
    done ;
    let coord = dimacs_coordinates (G.index g) coord in
    let printed = Array.make (G.n g) false in
    let print u = 
      let lon, lat = Coord.get_loc coord u in
      Printf.printf "[%f,%f]" lat lon ;
      printed.(u) <- true ;
    in
    let first_branch = ref true in
    let print_branch u =
      if !first_branch then first_branch := false 
      else Printf.printf ",\n" ;
      let rec iter acc u = 
        if printed.(u) || u = u_root then begin
          Printf.printf "[" ;
          print u ;
          List.iter (fun u -> Printf.printf "," ; print u) acc ;
          Printf.printf "]" ;
        end else begin
          let p = G.V.index p u in
          iter (u :: acc) p
        end
      in
      iter [] u ;
    in
        (* Scan verticies by increasing distance : *)
    let nbranches = ref 1 and nbranches_long = ref 1 in
    for i = 0 to n - 1 do
      let v = visit.(i) in
      if long_node v then begin
        if nsons_long.(v) >= 2 then print_branch v ;
        if nsons_long.(v) = 0 then print_branch desc_node.(v) ;
      end ;
      nbranches := !nbranches - 1 + nsons.(v) ;
      if long_node v then decr nbranches_long ;
      nbranches_long := !nbranches_long + nsons_long.(v) ;
    done
  );

  "diam_path", "[node_name] [coord_file] : compute the longest path from \
                node [node_name] with coordinates in [coord_file]",
  (fun () ->
    let g = !the_graph in
    let s = TrivialArg.string "node_name" in
    let coord = TrivialArg.string "coord_file" in
    let s = G.index g s in
    let dist, parent, last = G.dijkstra g s in
    let coord = dimacs_coordinates (G.index g) coord in
    let print_branch u root =
      let print u = 
        let lon, lat = Coord.get_loc coord u in
        Printf.printf "[%f,%f]" lat lon
      in
      let rec iter acc u = 
        if u = root then begin
          Printf.printf "[" ;
          print u ;
          List.iter (fun u -> Printf.printf "," ; print u) acc ;
          Printf.printf "]" ;
        end else begin
          let p = G.V.index parent u in
          iter (u :: acc) p
        end
      in
      iter [] u ;
    in
    print_branch last s
  );

  "closest", "[lat] [lon] [coord_file] : return the index of the closest \
              node from the given position.",
  (fun () ->
    let lat = TrivialArg.float "lattitude" in
    let lon = TrivialArg.float "longitude" in
    let pos = lon, lat in
    let coord = TrivialArg.string "coord_file" in
    let coord = dimacs_coordinates int_of_string coord in
    let close = ref 0 and dist = ref max_float in
    Coord.iter (fun u ->
      let d = Grid.dist pos (Coord.get_loc coord u) in
      if d < !dist then (close := u ; dist := d)
    ) coord ;
    let lon,lat = Coord.get_loc coord !close in
    Debug.info "Closest : %d %f,%f (at %f m)" !close lat lon !dist ;
    Printf.printf "%d\n" !close
  );

]


(* 
USA-road-d.NY.gr.gz :
diam : ecc(90644)=1652818 ecc'(90644)=1652818 (0.00s / 49s, 43m)
rad : ecc(28160)=828773,1657546 ecc'(28160)=828773,1657546 (0.00s / 49s, 43m)

USA-road-d.USA.gr.gz :
diameter: 62 sweeps  (55859802 = 27766) (0.00s / 22334s, 4884m)
diam : ecc(9159208)=55859802 ecc'(9159208)=55859802 (0.00s / 22334s, 4884m)
rad : ecc(5567034)=27995792,55991584 ecc'(5567034)=27995792,55991584 (0.00s / 22334s, 4884m)

USA-road-t.USA.gr.gz :
diameter: 9 sweeps  (66339681 = 0) (0.00s / 2369s, 4552m)
diam : ecc(9159208)=66339681 ecc'(9159208)=66339681 (0.00s / 2369s, 4552m)
rad : ecc(13938409)=33170689,66341378 ecc'(13938409)=33170689,66341378 (0.00s / 2369s, 4552m)

france-latest.osm-t.gr.gz
diameter: 11 sweeps  (49080052 = 48842190) (0.00s / 307s, 1432m)
diam : ecc(268690564)=49080052 ecc'(1247150638)=49080052 (0.00s / 307s, 1432m)
rad : ecc(239563337)=24872331,49831164 ecc'(343441717)=24906148,51194282 (0.00s / 307s, 1432m)

france-latest.osm-d.gr.gz
diameter: 13 sweeps  (13458991 = 0) (0.00s / 2652s, 5304m)
diam : ecc(1056090)=13458991 ecc'(9857562)=13458991 (0.00s / 2652s, 5304m)
rad : ecc(22229981)=6727013,13459140 ecc'(2214912)=6728583,13461366 (0.00s / 2652s, 5304m)

corse-latest.osm-t.gr.gz :
diameter: 105 sweeps  (12334557 = 11430100) (0.00s / 110s, 76m)
diam : ecc(2990023579)=12334557 ecc'(2990023579)=12330955 (0.00s / 110s, 76m)
rad : ecc(51303544)=6944137,13893347 ecc'(51303545)=6945508,13893396 (0.00s / 110s, 76m)

corse-latest.osm-d.gr.gz :
diameter: 96 sweeps  (2567918 = 1610150) (0.00s / 100s, 91m)
diam : ecc(2990023579)=2567918 ecc'(1061717830)=2567918 (0.00s / 100s, 91m)
rad : ecc(21103286)=1398623,2806794 ecc'(1249118726)=1403619,2807019 (0.00s / 100s, 91m)


corse-asym.osm-d.gr.gz :
diameter: 1501 sweeps  (12299918 = 0) (0.00s / 622s, 225m)
diam : ecc(1104877)=12299918 ecc'(1524196)=12299918 (0.00s / 622s, 225m)
rad : ecc(28224)=7484071,15506219 ecc'(1518963)=7121375,15506219 (0.00s / 622s, 225m)

*)




let () =
  Printexc.record_backtrace true ;
  try
    Debug.set_verbosity "info"  ;

    let usage = "[com list] : apply some operations on a weighted graph" in 
    TrivialArg.parse usage coms ;

    Debug.the_end () ;
    ()
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    flush stderr ;
    exit 2

  
