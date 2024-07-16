open Printf
open Redlib
module Red = Red.F()

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt

let progname = Array.get Sys.argv 0
let usage = Format.sprintf "%s [OPTIONS] PAT1 PAT2" progname
let parse_command_line() =
  let dfa = ref false in
  let anons = ref [] in
  let () =
    Arg.parse [
      "--debug", Arg.Set Redlib.Red.debug, "switch red debug messages on";
      "--dfa", Arg.Set dfa, "show the constructed DFA after grepping";
    ]
      (fun x -> anons := x :: !anons) usage
  in
  match List.rev !anons with
  | [pat1;pat2] -> pat1,pat2,!dfa
  | _ -> eprintf "%s\n" usage; exit 1


let _ =
  let pat1,pat2,_show_dfa = parse_command_line () in

  match (Red.Sub.parse ~pat:pat1) with
  | `Error (s,i) -> eprintf "%s -- pat1 '%s' -- %s -- char %d\n" progname pat1 s i; exit 1
  | `Ok red1 ->

    match (Red.Sub.parse ~pat:pat2) with
    | `Error (s,i) -> eprintf "%s -- pat2 '%s' -- %s -- char %d\n" progname pat2 s i; exit 1
    | `Ok red2 ->

      logf "pat1 = %s" pat1;
      logf "pat2 = %s" pat2;
      begin
        match (Red.test_for_equivalence red1 red2) with
        | `same -> logf "SAME"
        | `in_first_not_second s -> logf "in_first_not_second - '%s'" s
        | `in_second_not_first s -> logf "in_second_not_first - '%s'" s
      end(*;
      if show_dfa then  Red.debug_print_cache red1;
      if show_dfa then  Red.debug_print_cache red2*)
