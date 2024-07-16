open Printf
open Redlib
module Red = Red.F()
module R = Red.Sub

let contents ~file = In_channel.(with_open_text file input_all)

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let all_of words =
  Red.toplevel ~anchor_start:false ~anchor_end:false (
    R.conjs (
      List.map (fun
        word -> R.cats [R.dotstar; R.string word; R.dotstar]
      ) words)
  )

let _ =
  let file = Sys.argv.(1) in
  let words = List.tl (List.tl (Array.to_list Sys.argv)) in
  let s = contents ~file in
  logf "all_of, file: %s(#=%d), words: (%s)..." file (String.length s)
    (String.concat "," words);
  match (Red.search (all_of words) s) with
  | None -> logf "FAIL"
  | Some (p,q) -> logf "succ (%d,%d) --> (%s)" p q (String.sub s p (q-p))
