let nums =
  Array.of_list ((List.map
    (fun n -> 1. /. (10. ** ((float_of_int n) /. 20.)))
    [0; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 22; 24; 26; 28])
  @ [0.0])

(* These are the log volume levels used by BeebEm/SDL.  No idea how they were
   derived.  *)
let nums' =
  Array.of_list (List.map
    (fun n -> (float_of_int n) /. 128.)
    [120; 102; 87; 74; 63; 54; 46; 39; 33; 28; 24; 20; 17; 14; 11; 0])

let get_sample inbuf n =
  let samp =
    (Char.code inbuf.[n * 2]) + 256 * (Char.code inbuf.[n * 2 + 1]) in
  (float_of_int samp) /. 65535.0

let convert_file infile outfile =
  let inh = open_in_bin infile
  and outh = open_out_bin outfile in
  let inlen = in_channel_length inh in
  let outlen = inlen / 2 in
  let inbuf = String.create inlen
  and outbuf = String.create outlen in
  really_input inh inbuf 0 inlen;
  let closest_to x =
    let (_, best, _) = Array.fold_left
      (fun (cur_idx, best_idx, least) n ->
        let cur = abs_float (x -. n) in
	if cur < least then
	  (succ cur_idx, cur_idx, cur)
	else
	  (succ cur_idx, best_idx, least))
      (0, -1, 100000.0)
      nums in
    best in
  for i = 0 to outlen - 1 do
    let sample = get_sample inbuf i in
    let best = closest_to sample in
    outbuf.[i] <- Char.chr best
  done;
  output outh outbuf 0 outlen;
  close_in inh;
  close_out outh;
  inbuf, outbuf

let get_palette buf entry_size =
  let palette = Hashtbl.create 1000 in
  for i = 0 to (String.length buf - entry_size) / entry_size - 1 do
    let entry = ref []
    and avg = ref 0 in
    for j = 0 to entry_size - 1 do
      let samp = (Char.code buf.[i * entry_size + j]) in
      avg := samp + !avg;
    done;
    avg := !avg / entry_size;
    for j = 0 to entry_size - 1 do
      let samp = Char.code buf.[i * entry_size + j] in
      (* entry := (samp - !avg) :: !entry*)
      entry := samp :: !entry
    done;
    if not (Hashtbl.mem palette !entry) then
      Hashtbl.add palette !entry true
  done;
  Hashtbl.fold (fun k _ ls -> k :: ls) palette []

let find_bbox palette entry_size =
  let mins = Array.create entry_size max_int
  and maxs = Array.create entry_size min_int in
  List.iter
    (fun palentry ->
      ignore (List.fold_left
        (fun n dim ->
	  mins.(n) <- min mins.(n) dim;
	  maxs.(n) <- max maxs.(n) dim;
	  succ n)
	0
	palentry))
    palette;
  mins, maxs

let longest_dimension mins maxs =
  let best_dim = ref (-1) in
  let best_dist = ref 0 in
  for i = 0 to Array.length mins - 1 do
    let dist = maxs.(i) - mins.(i) in
    if dist >= !best_dist then begin
      best_dist := dist;
      best_dim := i
    end
  done;
  !best_dim

let sort_and_partition palette dim =
  let sorted_pal = List.sort
    (fun p1 p2 -> compare (List.nth p1 dim) (List.nth p2 dim))
    palette in
  let length = (List.length sorted_pal) / 2 in
  let lhs, rhs, _ =
    List.fold_left
      (fun (lhs, rhs, num) palentry ->
        if num < length then
	  (palentry :: lhs, rhs, succ num)
	else
	  (lhs, palentry :: rhs, succ num))
      ([], [], 0)
      sorted_pal in
  lhs, rhs

(* Find the dimension-wise median of a palette subset.  *)

let rec palentry_median palette entry_size =
  Array.init entry_size
    (fun i ->
      let dim = List.fold_right
	(fun palentry acc -> (List.nth palentry i) :: acc)
	palette
	[] in
      List.nth (List.sort compare dim) (entry_size / 2))

let rec median_cut palette entry_size depth outlist =
  let bbox_min, bbox_max = find_bbox palette entry_size in
  if depth == 0 then
    (bbox_min, bbox_max, palentry_median palette entry_size) :: outlist
  else begin
    let longest_dim = longest_dimension bbox_min bbox_max in
    let palette_l, palette_r = sort_and_partition palette longest_dim in
    let outlist' = median_cut palette_l entry_size (pred depth) outlist in
    median_cut palette_r entry_size (pred depth) outlist'
  end

let numlist str =
  let lst = ref [] in
  for i = 0 to String.length str - 1 do
    lst := (string_of_int (Char.code str.[i])) :: !lst
  done;
  !lst

let square_dist p1 p2 =
  let acc = ref 0.0 in
  for i = 0 to Array.length p1 - 1 do
    acc := !acc +. (p1.(i) -. p2.(i)) *. (p1.(i) -. p2.(i))
  done;
  !acc

let closest_palette_entry buf offset entry_len palette =
  let _, best_idx, _ = List.fold_left
    (fun (idx, best_idx, best_dist) palentry ->
      let dist = square_dist
        (Array.init entry_len (fun i -> get_sample buf (offset + i)))
	(Array.map (fun n -> nums.(n)) palentry) in
      match best_dist with
        None -> (succ idx, idx, Some dist)
      | Some old_dist as od ->
          if dist < old_dist then
	    (succ idx, idx, Some dist)
	  else
	    (succ idx, best_idx, od))
    (0, -1, None)
    palette in
  best_idx

let pal_entries palette =
  List.map (fun (_, _, med) -> med) palette

let _ =
  if (Array.length Sys.argv) != 3 then
    failwith (Printf.sprintf "Usage: %s <infile> <outfile>" Sys.argv.(0));
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2) in
  let inbuf, outbuf = convert_file infile outfile in
  let entry_size = 8 in
  let palette = get_palette outbuf entry_size in
  Printf.printf "Number of unique palette entries: %d\n"
    (List.length palette);
  let pal_out = median_cut palette entry_size 8 [] in
  Printf.printf "output palette length: %d\n" (List.length pal_out);
  List.iter
    (fun (_, _, entry) ->
      Printf.printf "%s\n" (String.concat ","
			   (List.map string_of_int (Array.to_list entry))))
    pal_out;
  let palette = pal_entries pal_out in
  if false then begin
    let reconstruct = String.make (String.length outbuf) '\000' in
    for i = 0 to (String.length outbuf / entry_size) - 1 do
      let use_entry = closest_palette_entry inbuf (i * entry_size) entry_size
					    palette in
      let pal_entry = List.nth palette use_entry in
      for j = 0 to entry_size - 1 do
	reconstruct.[i * entry_size + j] <-
          Char.chr (int_of_float (nums.(pal_entry.(j)) *. 255.0))
      done
    done;
    let fo = open_out_bin outfile in
    output fo reconstruct 0 (String.length reconstruct);
    close_out fo
  end else begin
    assert (entry_size == 8);
    let pal_enc_buf = String.make 1024 '\000' in
    ignore (List.fold_left
      (fun idx entry ->
        for i = 0 to 3 do
	  let lo = entry.(i * 2)
	  and hi = entry.(i * 2 + 1) in
	  pal_enc_buf.[idx * 4 + i] <- Char.chr ((hi lsl 4) lor lo)
	done;
	succ idx)
      0
      palette);
    let indices = String.make (String.length outbuf / entry_size) '\000' in
    for i = 0 to (String.length outbuf / entry_size) - 1 do
      let use_entry = closest_palette_entry inbuf (i * entry_size) entry_size
					    palette in
      indices.[i] <- Char.chr use_entry
    done;
    let fo = open_out_bin outfile in
    output fo pal_enc_buf 0 1024;
    output fo indices 0 (String.length indices);
    close_out fo
  end
