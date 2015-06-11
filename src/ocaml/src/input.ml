module Bytes = String

type filgegz = FILENORM of in_channel * string ref * int ref 
               | FILEGZ of Gzip.in_channel * string ref * int ref

let file_open_in fname =
  let buf = Bytes.create 1024 and pos = 0 in
  if Filename.check_suffix fname ".gz" then
    FILEGZ ((Gzip.open_in fname), ref buf, ref pos)
  else
    FILENORM ((open_in fname), ref buf, ref pos)

let file_input_line file =
  let input, buf, pos =
    match file with
      | FILENORM (f, buf, pos) -> input f, buf, pos
      | FILEGZ (f, buf, pos) -> Gzip.input f, buf, pos
  in
  let rec index n =
    let n = try min !pos (String.index_from !buf n '\n') with Not_found -> !pos in
    if n < !pos then n 
    else begin
      if !pos = String.length !buf then begin
        let newbuf = Bytes.create (2 * !pos) in
        String.blit !buf 0 newbuf 0 !pos ;
        buf := newbuf ;
      end ;
      let nread = input !buf !pos (String.length !buf - !pos) in
      if nread = 0 
      then (if !pos = 0 then raise End_of_file else incr pos ; !pos-1)
      else begin
        let n = !pos in
        pos := !pos + nread ;
        index n
      end
    end
  in
  let n = index 0 in
  let res = Bytes.create n in
  String.blit !buf 0 res 0 n ;
  pos := !pos - (n+1) ;
  String.blit !buf (n+1) !buf 0 !pos ;
  res

let file_close_in file =
  match file with
    | FILENORM (f,_,_) -> close_in f
    | FILEGZ (f,_,_) -> Gzip.close_in f


let file_wc fname =
  let file = file_open_in fname in
  let nread = ref 0 and ntotal = -1 in
  (try while true do
    ignore (file_input_line file) ;
    incr nread ;
    Debug.progress !nread ntotal
      "Counting lines of %s" fname ; 
   done with End_of_file -> ()) ;
  Debug.progress_lap () ;
  file_close_in file ;
  !nread


let line_read f fname =
  let file = file_open_in fname in
  let nread = ref 0 and ntotal = Debug.file_nlines fname in
  (try while true do
      f (file_input_line file) ;
      incr nread ;
      Debug.progress !nread ntotal "Reading %s" fname ; 
   done with End_of_file -> ()) ;
  Debug.store_nlines fname !nread ;
  Debug.progress_lap () ;
  file_close_in file

