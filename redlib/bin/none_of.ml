open Printf
open Redlib
module Red = Red.F()
module R = Red.Sub

let contents ~file = In_channel.(with_open_text file input_all)

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let none_of words =
  Red.toplevel ~anchor_start:true ~anchor_end:true (
    R.neg (
      R.cats [
        R.dotstar;
        R.alts (List.map R.string words);
        R.dotstar;
      ])
  )

let _ =
  let file = Sys.argv.(1) in
  let words = List.tl (List.tl (Array.to_list Sys.argv)) in
  let s = contents ~file in
  logf "none_of, file: %s(#=%d), words: (%s)..." file (String.length s)
    (String.concat "," words);
  match (Red.matches (none_of words) s) with
  | false -> logf "FAIL - something was found"
  | true -> logf "succ - none of the words found"
