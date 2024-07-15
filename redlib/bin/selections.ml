
open Core.Std
open Redlib
module Red = Red.F(struct end)
module R = Red.Sub

let search_multi ~pat s =
  let n = String.length s in
  let red = Red.parse_exn ~pat in
  let rec loop i acc =
    if i >= n then List.rev acc else
      match (Red.search red ~start_pos:i s) with
      | None -> List.rev acc
      | Some ((p,q) as range) ->
        let new_i = max (p+1) q in
        assert (new_i > i);
        loop new_i (range :: acc)
  in
  loop 0 []

let search_multi_negative ~pat s =
  let n = String.length s in
  let red = Red.parse_exn ~pat in
  let rec loop i acc =
    if i >= n then List.rev acc else
      match (Red.search red ~start_pos:i s) with
      | None -> List.rev ((i,n) :: acc)
      | Some (p,q) ->
        let new_i = max (p+1) q in
        assert (new_i > i);
        if p > i
        then
          let gap_range = (i,p) in
          loop new_i (gap_range :: acc)
        else
          loop new_i acc
  in
  loop 0 []


let selections ~pat s =
  List.map (search_multi ~pat s) ~f:fun (p,q) ->
    String.slice s p q

let split ~pat s =
  List.map (search_multi_negative ~pat s) ~f:fun (p,q) ->
    String.slice s p q


let message fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let progname = Array.get Sys.argv 0
let usage = Format.sprintf "%s [OPTION] PATTERN [FILE]" progname
let parse_command_line() =
  let v = ref false in
  let anons = ref [] in
  let () =
    Arg.parse [
      "-v", Arg.Set v, "select `negative' text between matches ('split')";
    ]
      (fun x -> anons := x :: !anons) usage
  in
  match List.rev !anons with
  | [pat] -> pat,None,!v (* stdin *)
  | [pat;filename] -> pat,Some filename,!v
  | _ -> eprintf "%s\n" usage; exit 1


let _ =
  let pat,file_opt,arg_v = parse_command_line () in
  let s =
    match file_opt with
    | Some file -> In_channel.read_all file
    | None -> In_channel.input_all In_channel.stdin
  in
  let xs =
    (if arg_v then search_multi_negative else search_multi) ~pat s
  in
  List.iter xs ~f:fun (p,q) ->
    (*message "%s(%d,%d)" (String.slice s p q) p q*)
    message "%s" (String.slice s p q)

