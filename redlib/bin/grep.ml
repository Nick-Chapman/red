open Printf
open Redlib
module Red = Red.F()

let red_matcher ~pat =
  match (Red.parse ~pat) with
  | `Error e -> `Error e
  | `Ok red ->
    `Ok (
      Red.matches red,
      (fun () -> Red.debug_print_cache red)
    )

let progname = Array.get Sys.argv 0
let usage = Format.sprintf "%s [OPTION] PATTERN [FILE]..." progname
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
  | [] -> eprintf "%s\n" usage; exit 1
  | pat::filenames -> pat,filenames,!dfa

let _ =
  let pat,filenames,show_dfa = parse_command_line () in
  let selected_matcher = red_matcher in
  match (selected_matcher ~pat) with
  | `Error (s,i) -> eprintf "%s -- %s -- char %d\n" progname s i; exit 1
  | `Ok (f_matches, dfa_shower) ->
    let grep1 ?tag channel =
      let print_line =
        match tag with
        | None -> (fun line -> printf "%s\n%!" line)
        | Some filename -> (fun line -> printf "%s:%s\n%!" filename line)
      in
      List.iter (fun line -> if (f_matches line) then print_line line)
        (In_channel.input_lines channel)
    in
    (
      match filenames with
      | [] -> grep1 In_channel.stdin
      | [filename] ->
        In_channel.with_open_text filename (fun channel ->
          grep1 channel)
      | filenames ->
        List.iter (fun filename ->
          In_channel.with_open_text filename (fun channel ->
              grep1 ~tag:filename channel))
          filenames
    );
    if show_dfa then dfa_shower ()
