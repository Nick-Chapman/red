
open Core.Std
open Redlib
module Red = Red.F(struct end)
module R = Red.Sub

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let all_of words =
  Red.toplevel ~anchor_start:false ~anchor_end:false (
    R.conjs (
      List.map words ~f:(fun
        word -> R.cats [R.dotstar; R.string word; R.dotstar]
      ))
  )

let _ =
  let file = Sys.argv.(1) in
  let words = List.drop (Array.to_list Sys.argv) 2 in
  let s = In_channel.read_all file in
  logf "all_of, file: %s(#=%d), words: (%s)..." file (String.length s)
    (String.concat ~sep:"," words);
  match (Red.search (all_of words) s) with
  | None -> logf "FAIL"
  | Some (p,q) -> logf "succ (%d,%d) --> (%s)" p q (String.slice s p q)
