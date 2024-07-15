
open Core.Std
open Redlib
module Red = Red.F(struct end)
module R = Red.Sub

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let none_of words =
  Red.toplevel ~anchor_start:true ~anchor_end:true (
    R.neg (
      R.cats [
        R.dotstar;
        R.alts (List.map words ~f:R.string);
        R.dotstar;
      ])
  )

let _ =
  let file = Sys.argv.(1) in
  let words = List.drop (Array.to_list Sys.argv) 2 in
  let s = In_channel.read_all file in
  logf "all_of, file: %s(#=%d), words: (%s)..." file (String.length s)
    (String.concat ~sep:"," words);
  match (Red.matches (none_of words) s) with
  | false -> logf "FAIL - something was found"
  | true -> logf "succ - none of teh words found"
