
open Core.Std
open Redlib

module Red = Red.F(struct end)

let logf fmt = ksprintf (fun s -> printf "%s\n%!" s) fmt
let bar = "----------------------------------------------------------------------"

let progname = Array.get Sys.argv 0
let usage = Format.sprintf "%s [OPTIONS] PAT1" progname
let parse_command_line() =
  let dfa = ref false in
  let debug = ref false in
  let anons = ref [] in
  let () =
    Arg.parse [
      "--dfa", Arg.Set dfa, "show the constructed DFA after transforming";
      "--debug", Arg.Set debug, "switch red debug messages on";
    ]
      (fun x -> anons := x :: !anons) usage
  in
  match List.rev !anons with
  | [pat1] -> pat1,!dfa,!debug
  | _ -> eprintf "%s\n" usage; exit 1

let _ =
  let pat1,dfa,debug = parse_command_line () in
  Redlib.Red.debug := debug;
  match (Red.Sub.parse ~pat:pat1) with
  | `Error (s,i) -> eprintf "%s -- pat1 '%s' -- %s -- char %d\n" progname pat1 s i; exit 1
  | `Ok red1 ->
    logf "red(before) = %s" (Red.Sub.to_string red1);
    if !Redlib.Red.debug then logf "%s" bar;
    let red2 = Red.transform_via_and_forcing_dfa red1 in
    if dfa || debug then logf "%s" bar;
    if dfa then (
      Red.Sub.debug_print_cache red1;
      logf "%s" bar;
    );
    logf "red(after) = %s" (Red.Sub.to_string red2);
    begin
      match (Red.test_for_equivalence red1 red2) with
      | `same -> logf "SAME"
      | `in_first_not_second s -> logf "in_first_not_second - '%s'" s
      | `in_second_not_first s -> logf "in_second_not_first - '%s'" s
    end
