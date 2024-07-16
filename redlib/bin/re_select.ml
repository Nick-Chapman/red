open Printf
open Redlib
module Red = Red.F()
module R = Red.Sub

let logf fmt = ksprintf (fun s -> Printf.printf "%s\n%!" s) fmt

let contents ~file = In_channel.(with_open_text file input_all)


let bar = "----------------------------------------------------------------------"

let _ =
  let file = Sys.argv.(1) in
  let pat = Sys.argv.(2) in
  let debug = (Array.length Sys.argv > 3) && Sys.argv.(3) = "-debug" in
  let red = Red.parse_exn ~pat in
  let s = contents ~file in
  if debug then logf "file: %s (size=%d)\n    '%s'\n--> '%s'\n%s" file (String.length s)
    pat (Red.to_string red) bar;
  begin match (Red.search red ~strat:`leftmost_longest s) with
  | None -> logf "FAIL"
  | Some (p,q) -> logf "matched at (%d,%d)\n%s\n%s\n%s\n" p q bar (String.sub s p (q-p)) bar
  end
