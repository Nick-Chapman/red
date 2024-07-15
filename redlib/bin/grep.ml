open Core.Std
open Redlib

module Re2 = Re2.Regex

let re2_matcher ~pat =
  match (Re2.create pat) with
  | Error e -> `Error (Error.to_string_hum e, 0)
  | Ok re2 ->
    `Ok (
      Re2.matches re2,
      (fun () -> ())
    )

module Red = Red.F(struct end)

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
  let use_re2 = ref false in
  let anons = ref [] in
  let () =
    Arg.parse [
      "--debug", Arg.Set Redlib.Red.debug, "switch red debug messages on";
      "--dfa", Arg.Set dfa, "show the constructed DFA after grepping";
      "--re2", Arg.Set use_re2, "use re2-ocaml";
    ]
      (fun x -> anons := x :: !anons) usage
  in
  match List.rev !anons with
  | [] -> eprintf "%s\n" usage; exit 1
  | pat::filenames -> pat,filenames,!dfa,!use_re2



let _ =
  let pat,filenames,show_dfa,use_re2 = parse_command_line () in
  let selected_matcher =
    if use_re2
    then re2_matcher
    else red_matcher
  in
  match (selected_matcher ~pat) with
  | `Error (s,i) -> eprintf "%s -- %s -- char %d\n" progname s i; exit 1
  | `Ok (f_matches, dfa_shower) ->
    let grep1 ?tag channel =
      let print_line =
        match tag with
        | None -> (fun line -> printf "%s\n%!" line)
        | Some filename -> (fun line -> printf "%s:%s\n%!" filename line)
      in
      In_channel.iter_lines channel ~f:fun line ->
        if (f_matches line) then print_line line
    in
    (
      match filenames with
      | [] -> grep1 In_channel.stdin
      | [filename] ->
        In_channel.with_file filename ~f:fun channel ->
          grep1 channel
      | filenames ->
        List.iter filenames ~f:fun filename ->
          In_channel.with_file filename ~f:fun channel ->
            grep1 ~tag:filename channel
    );
    if show_dfa then dfa_shower ()
