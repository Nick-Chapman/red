(*
  REd -- in-house replacement regexp library.
  Based on technique of using RE derivatives for lazy DFA construction.

  Advantages of this technique:
  - Efficient non back-tracking search algorithm.
  - Tail recursive; no stack blowup as with PCRE.
  - No distinct pre-pass; DFA is generated lazily as input text is scanned.
  - Regexp language is extended with compliment operator (!) and conjuction (&)

  For more info, see:
    "Regular-expression derivatives reexamined", Owens, Reppy & Turon
*)

type strat =
[
| `soonest_shortest (* most efficient *)
| `soonest_leftmost
| `rightmost_longest
| `leftmost_longest  (* POSIX, least efficient *)
]

(* Lazily generated DFA cache lives for the dynamic extent of the functor application *)

module F (M:sig end) : sig

  type t

  val parse : pat:string -> [
  | `Ok of t
  | `Error of string * int (* message + position of bad char: 1.. *)
  ]

  val parse_exn : pat:string -> t
  val to_string : t -> string

  (* grammar accepted by the red parser...

    Red         = '^'? SubRed '$'?

    SubRed      = Disjunct ( '|' Disjunct )*
    Disjunct    = Conjunct ( '&' Conjunct )*
    Conjunct    = ( '!'? Sequence )*
    Sequence    = RepGroup *
    RepGroup    = Atom PostfixOp*
    PostfixOp   = '*' | '+' | '?' | '{' Counted '}'
    Atom        = '(' SubRed ')' | '.' | Char | Escaped | '[' '-'? CsetItem* ']'
    CsetItem    = CsetChar | CsetChar '-' CsetChar
    CsetChar    = CharX | Escaped
    Escaped     = '\' Escapable
    Counted     = Num | Num ',' | Num ',' Num
    Num         = Digit+

    Char        = not-one-of: |&!*+?{}().[]\
    CharX       = not-one-of: ]-\
    Escapable   = one-of: ^$|&!*+?{}().[]-\nt
    Digit       = one-of: 0123456789
  *)

  module Sub : sig

    type t

    val parse       : pat:string -> [ `Ok of t | `Error of string * int ]
    val parse_exn   : pat:string -> t
    val to_string   : t -> string
    val debug_print_cache : t -> unit

    val zero        : t                 (* []                   *)
    val eps         : t                 (* empty-string         *)
    val char        : char -> t         (* x                    *)
    val one_of      : char list -> t    (* [ab..]               *)
    val not_one_of  : char list -> t    (* [^ab..]              *)
    val dot         : t                 (* . or [^]             *)
    val star        : t -> t            (* r*                   *)
    val plus        : t -> t            (* r+                   *)
    val query       : t -> t            (* r?                   *)
    val dotstar     : t                 (* .*                   *)
    val cats        : t list -> t       (* sequencing, rs.. = R *)
    val neg         : t -> t            (* !R                   *)
    val alts        : t list -> t       (* (R|S|..)             *)
    val conjs       : t list -> t       (* (R&S&..)             *)
    val string      : string -> t       (* foo                  *)

    val repeat_count            : t -> int -> t         (* r{3}     *)
    val repeat_count_or_more    : t -> int -> t         (* r{3,}    *)
    val repeat_count_range      : t -> int -> int -> t  (* r(3,7}   *)

  end

  val toplevel : anchor_start:bool -> anchor_end:bool -> Sub.t -> t


  val search : t
    -> ?start_pos:int
    -> ?strat:strat (* default - `leftmost_longest *)
    -> string -> (int * int) option (* suitable for String.select *)

  val matches : t -> string -> bool

  val debug_print_cache : t -> unit

  val test_for_equivalence : Sub.t -> Sub.t
    -> [ `same | `in_first_not_second of string | `in_second_not_first of string ]

  val transform_via_and_forcing_dfa : Sub.t -> Sub.t

end

(* Wrappers to hide functor application *)
val search_exn : pat:string -> ?strat:strat -> string -> (int * int) option
val select_exn : pat:string -> ?strat:strat -> string -> string option
val matches_exn : pat:string -> string -> bool

val debug : bool ref (* see some messages *)
