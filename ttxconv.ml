let spaces fo =
  for i = 0 to 39 do
    Printf.fprintf fo "20"
  done;
  Printf.fprintf fo "\n"

let _ =
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2) in
  let fi = open_in_bin infile
  and fo = open_out outfile in
  let inlen = in_channel_length fi in
  let buf = String.create inlen in
  really_input fi buf 0 inlen;
  Printf.fprintf fo "---\n";
  spaces fo;
  for i = 0 to 22 do
    for j = 0 to 39 do
      let mychar = buf.[104 + 47 * i + j] in
      Printf.fprintf fo "%.2x" (Char.code mychar);
    done;
    Printf.fprintf fo "\n"
  done;
  spaces fo;
  close_in fi;
  close_out fo
