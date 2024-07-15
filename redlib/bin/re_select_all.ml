
open Core.Std
open Redlib
module Red = Red.F(struct end)
module R = Red.Sub

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let one_of words =
  R.alts (List.map words ~f:R.string)

let bar = "----------------------------------------------------------------------"

let _ =
  let file = Sys.argv.(1) in
  let pat = Sys.argv.(2) in
  let debug = (Array.length Sys.argv > 3) && Sys.argv.(3) = "-debug" in
  let red = Red.parse_exn ~pat in
  let s = In_channel.read_all file in
  let n = String.length s in
  if debug then
    logf "file: %s (size=%d)\n    '%s'\n--> '%s'\n%s" file n pat (Red.to_string red) bar;
  let rec loop i =
    if i>=n then () else
      match (Red.search red ~start_pos:i s) with
      | None -> ()
      | Some (p,q) ->
        logf "matched at (%d,%d) -- %s" p q (String.slice s p q);
        loop (max (p+1) q)
  in
  loop 0
