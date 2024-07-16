
open Printf
let debug = ref false

let structural_compare = compare
let structural_equal = (=)
let phys_equal = (==)

let upto : int -> int -> int list = fun i j ->
  let rec loop acc i = if i > j then List.rev acc else loop (i::acc) (i+1) in
  loop [] i

let implode chars =
  let bytes = Bytes.create (List.length chars) in
  List.iteri (Bytes.set bytes) chars;
  String.of_bytes bytes

let explode s =
  List.map (fun i -> s.[i]) (upto 0 (String.length s - 1))

module Char = struct
  include Char
  let get_digit c : int option =
    let code0 = Char.code '0' in
    if c >= '0' && c <= '9' then Some (code c - code0) else None
end

module String = struct
  include String
  let slice s p q = String.sub s p (q-p)
end

module List = struct
  include List
  let group : break:('a -> 'a -> bool) -> 'a list -> 'a list list = fun ~break xs ->
    List.of_seq (Seq.map List.of_seq (Seq.group break (List.to_seq xs)))
end

module Hashtbl : sig
  include module type of Hashtbl
  val find_or_add : ('k,'v) t -> 'k -> default:(unit -> 'v) -> 'v
  val data : ('k,'v) t -> 'v list
end = struct
  include Hashtbl
  let find_or_add ht k ~default =
    match Hashtbl.find_opt ht k with
    | Some v -> v
    | None -> let v = default () in Hashtbl.add ht k v; v
  let data ht =
    let f _ v acc = v::acc in
    Hashtbl.fold f ht []
end

type strat =
[
| `soonest_shortest  (* searches: ---> then <--| *)
| `soonest_leftmost  (* searches: ---> then <==| *)
| `rightmost_longest (* searches: ===> then <==| *)
| `leftmost_longest  (* searches: <=== then |==> *)
]

let message fmt = ksprintf (fun s -> eprintf "%s\n%!" s) fmt

let char_range c1 c2 =
  List.map Char.chr (upto (Char.code c1) (Char.code c2))

let%test _ = String.equal (implode (char_range 'a' 'a')) "a"
let%test _ = String.equal (implode (char_range 'a' 'e')) "abcde"
let%test _ = String.equal (implode (char_range 'e' 'a')) ""
let%test _ = String.equal (implode (char_range 'b' 'a')) ""

let sort_and_remove_dups ~compare xs =
  List.sort_uniq compare xs

let filter_out xs ~f = List.filter (fun x -> not (f x)) xs

let rec iterate i f acc =
  if Int.equal i 0 then acc else iterate (i-1) f (f acc)

module Cset : sig

  type t
  val rep : t -> char list
  val create : Char.t list -> t
  val any : t
  val is_empty : t -> bool
  val pick_one_exn : t -> char
  val contains : t -> Char.t -> bool
  val intersect : t -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t

  val compare : t -> t -> int


end = struct

  type t = char list (* rep as ordered list with no dups *)

  let rep t = t

  let create xs =
    sort_and_remove_dups ~compare:Char.compare xs

  let any = create (List.map Char.chr (upto 0 255))

  let is_empty = function [] -> true | _::_ -> false

  let pick_one_exn = function [] -> failwith "pick_one_exn" | x::_ -> x

  let contains t char = List.mem char t

  let intersect =
    let rec loop acc xs1 xs2 =
      match xs1,xs2 with
      | [],_ -> List.rev acc
      | _,[] -> List.rev acc
      | (x1::xs1 as again1), (x2::xs2 as again2) ->
        if Char.equal x1 x2 then loop (x1::acc) xs1 xs2
        else
          if x1 < x2
          then loop acc xs1 again2
          else loop acc again1 xs2
    in
    loop []

  let union =
    let rec loop acc xs1 xs2 =
      match xs1,xs2 with
      | [],xs2 -> List.rev_append acc xs2
      | xs1,[] -> List.rev_append acc xs1
      | (x1::xs1 as again1), (x2::xs2 as again2) ->
        if Char.equal x1 x2 then loop (x1::acc) xs1 xs2
        else
          if x1 < x2
          then loop (x1::acc) xs1 again2
          else loop (x2::acc) again1 xs2
    in
    loop []

  let diff =
    let rec loop acc xs1 xs2 =
      match xs1,xs2 with
      | [],_ -> List.rev acc
      | xs1,[] -> List.rev_append acc xs1
      | (x1::xs1 as again1), (x2::xs2 as again2) ->
        if Char.equal x1 x2 then loop acc xs1 xs2
        else
          if x1 < x2
          then loop (x1::acc) xs1 again2
          else loop acc again1 xs2
    in
    loop []

  let mk string = create (explode string)
  let (=) = structural_equal
  let (!=) x y = not (x = y)
  let%test _ = mk "xyz" = mk "xzy"
  let%test _ = mk "xyz" != mk "xzw"
  let%test _ = intersect (mk "abcd") (mk "acxy") = mk "ac"
  let%test _ = union (mk "abcd") (mk "acxy") = mk "abcdxy"
  let%test _ = diff (mk "abcd") (mk "acxy") = mk "bd"

  let compare = structural_compare
end

module F() = struct

  type sense = Pos | Neg

  module Form = struct

    type 'a t =
    | Eps
    | Poslit of Cset.t
    | Neglit of Cset.t
    | Star of 'a
    | Neg of 'a
    | Cat2 of 'a * 'a
    | Alts of 'a list

    let nullable ~nullable form =
      match form with
      | Eps -> true
      | Poslit _ -> false
      | Neglit _ -> false
      | Star _ -> true
      | Neg t -> not (nullable t)
      | Cat2 (t1,t2) -> nullable t1 && nullable t2
      | Alts ts -> List.exists nullable ts

    let firsts ~firsts form =
      match form with
      | Eps -> []
      | Poslit cset -> [cset]
      | Neglit cset -> [cset]
      | Star t -> firsts t
      | Neg t -> firsts t
      | Cat2 (t1,t2) -> firsts t1 @ firsts t2
      | Alts ts -> List.concat_map firsts ts

  end

  let is_regexp_special_char = function
    | '(' | ')' | '[' | ']' | '&' | '|' | '!'
    | '*' | '+' | '?' | '.' | '-' | '^' | '$'
    | '{' | '}'
    | '\\'
      -> true | _ -> false

  module Display : sig (* in correct syntax for parsing *)

    val cset : Cset.t -> string
    val cspec : sense -> Cset.t -> string
    val form : unroll:('a -> 'a Form.t)  -> 'a -> string

  end = struct

    let of_char c =
      match c with
      | '\n' -> "\\n"
      | '\t' -> "\\t"
      | _ -> if is_regexp_special_char c then sprintf "\\%c" c else sprintf "%c" c

    let cset cset =
      match (Cset.rep cset) with
      | [x] -> of_char x
      | xs -> sprintf "[%s]" (String.concat "" (List.map of_char xs))

    let cspec sense cset =
      let xs = Cset.rep cset in
      match sense,xs with
      | Neg,[] -> "."
      | Pos,[c] -> of_char c
      | _,_ ->
        sprintf "[%s%s]"
          (match sense with Pos -> "" | Neg -> "^")
          (String.concat "" (List.map of_char xs))

    let form ~unroll =

      let rec to_string t =
        match (unroll t) with
        | Form.Eps -> ""
        | Form.Poslit x -> cspec Pos x
        | Form.Neglit x -> cspec Neg x
        | Form.Star t -> sprintf "%s*" (to_string' t)
        (*| Form.Neg t -> sprintf "(!%s)" (to_string t)*)
        | Form.Neg t -> to_string_neg t
        | Form.Cat2 (t1,t2) -> sprintf "%s%s" (to_string t1) (to_string t2)
        | Form.Alts [] -> "[]"
        | Form.Alts ts ->
          sprintf "(%s)" (String.concat "|" (List.map to_string ts))

      and to_string_neg t =
        match (unroll t) with
        | Form.Neg t -> to_string t
        | Form.Alts [] -> ".*"
        | Form.Alts ts ->
          sprintf "(%s)" (String.concat "&" (List.map to_string_neg ts))
        | _ -> sprintf "(!%s)" (to_string t)

      and to_string' t = (*to_string t*) (* temp while have fully parenthesized cat2 *)
        match (unroll t) with
        | Form.Eps | Form.Poslit _ | Form.Neglit _ | Form.Neg _ | Form.Alts _
        | Form.Cat2 _
          -> to_string t
        | Form.Star _
          -> sprintf "(4%s)" (to_string t)
      in
      to_string

  end

  let partition_firsts csets =
    (* critical for efficiency to remove dups *)
    let csets = sort_and_remove_dups ~compare:Cset.compare csets in
    let acc,cover =
      List.fold_left (fun (acc,cover) cset ->
        let acc =
          filter_out ~f:Cset.is_empty (
            Cset.diff cset cover ::
              List.concat_map (fun in1 -> [
                Cset.intersect in1 cset;
                Cset.diff in1 cset;
              ]) acc
          )
        in acc, Cset.union cover cset) ([],Cset.create[]) csets
    in
    (* sort result to get dfa display in order *)
    let acc = sort_and_remove_dups ~compare:Cset.compare acc in
    acc, (`cover  cover)

  (* canonicalizing; with hash-consing, and memoized deriv *)
  module Gold : sig

    type t

    val to_string : t -> string
    val to_tag_string : t -> string

    val unanchor : t -> t
    val reverse : t -> t
    val is_zero : t -> bool
    val nullable : t -> bool
    val deriv : char -> t -> t
    val debug_print_cache : t -> unit

    val zero : t
    val eps : t
    val dotstar : t
    val star : t -> t
    val neg : t -> t
    val cats : t list -> t
    val alts : t list -> t
    val conjs : t list -> t

    val lit : sense -> Cset.t -> t
    val one_of : char list -> t
    val not_one_of : char list -> t
    val dot : t
    val char : char -> t
    val string : string -> t
    val query : t -> t
    val plus : t -> t

    val repeat_count : t -> int -> t
    val repeat_count_or_more : t -> int -> t
    val repeat_count_range : t -> int -> int -> t

    val equal : t -> t -> bool
    val compare : t -> t -> int

    val representative_first_chars : t -> char list

    val unroll_one_step : t -> ((sense * Cset.t) * t) list

  end = struct

    type t = {
      rep : form;
      u : int;
      nullable : bool;
      firsts : Cset.t list;
      partition : Cset.t list;
      cover : Cset.t;
      d_map : (Cset.t * t option ref) list;
      mutable d_unspec : t option;
      d_array : t option Array.t;
      mutable is_state_of_dfa : bool; (* just for debug print *)
    }
    and form = t Form.t

    let unroll t = t.rep
    let to_string = Display.form ~unroll

    let tag t = t.u

    let equal = phys_equal
    let compare t1 t2 = if equal t1 t2 then 0 else Int.compare t1.u t2.u

    let to_tag_string t = sprintf "%03d" (tag t)

    module Cache = struct

      type cache = {
        mutable u : int;
        ht : (form, t) Hashtbl.t;
      }

      let the_cache = {
        u = 0;
        ht = Hashtbl.create 100;
      }

      let lookup_or_add form ~f =
        Hashtbl.find_or_add the_cache.ht form ~default:(fun () ->
          let u = 1 + the_cache.u in
          the_cache.u <- u;
          f u)

      let s_of_t_option opt =
        match opt with None -> "---" | Some t -> to_tag_string t

      let just_show_dfa_nodes = ref true

      let print t0 =
        let cmp_u t1 t2 = Int.compare (tag t1) (tag t2) in
        let all_sorted = List.sort cmp_u (Hashtbl.data the_cache.ht) in
        let just_dfa_nodes = List.filter (fun t -> t.is_state_of_dfa) all_sorted in
        let nodes =
          if !just_show_dfa_nodes then just_dfa_nodes else all_sorted
        in
        List.iter (fun t ->
          message "%-2s%-80s  =  %s"
            (if Int.equal (tag t) (tag t0) then "*" else (* start node *)
                if not !just_show_dfa_nodes && t.is_state_of_dfa then "x" else "")
            (sprintf "%03d  =  %s%s %s" (tag t)
               (String.concat ""
                  (List.map (fun (cset,r) ->
                       sprintf "%s %s | " (Display.cset cset) (s_of_t_option (!r)))
                   t.d_map))
               (Display.cspec Neg t.cover)
               (s_of_t_option t.d_unspec)
            )
            (to_string t)
        ) nodes;
        let n,a,v = (* count - nodes,arcs,visited-arcs *)
          List.fold_left (fun (n,a,v) t ->
            let n = n + 1 in
            let a = a + 1 + List.length t.d_map in
            let v =
              v + (match (t.d_unspec) with | Some _ -> 1 | None -> 0) +
                (List.fold_left
                   (fun n (_,r) -> match (!r) with None -> n | Some _ -> n + 1) 0 t.d_map)
            in
            n,a,v) (0,0,0) just_dfa_nodes
        in
        message "DFA: #nodes = %d, #arcs(fraction visited) = %d / %d" n v a

    end

    let debug_print_cache t = Cache.print t

    let nullable t = t.nullable
    let firsts t = t.firsts

    let create rep  =
      Cache.lookup_or_add rep ~f:(fun u ->
        let firsts = Form.firsts ~firsts rep in
        let partition,`cover cover = partition_firsts firsts in
        let d_map = List.map (fun x -> x,ref None) partition in
        let d_unspec = None in
        let d_array = Array.make 256 None in
        let t = {
          rep;
          u;
          nullable = Form.nullable ~nullable rep;
          firsts;
          partition;
          cover;
          d_map; d_unspec; d_array;
          is_state_of_dfa = false;
        } in
        t)

    let is_eps t =
      match t.rep with
      | Form.Eps -> true
      | _ -> false

    let is_zero t =
      match t.rep with
      | Form.Alts [] -> true
      | _ -> false

    let zero = create (Form.Alts [])

    let eps = create (Form.Eps)

    let poslit spec =
      if Cset.is_empty spec then zero else
        create (Form.Poslit spec)

    let neglit spec =
      create (Form.Neglit spec)

    let lit = function | Pos -> poslit | Neg -> neglit

    let neg t =
      match t.rep with
      | Form.Neg t -> t
      | _ -> create (Form.Neg t)

    let dotstar = neg zero

    let star t =
      match t.rep with
      | Form.Alts [] -> eps
      | Form.Star _ -> t
      | Form.Neglit cset ->
        if Cset.is_empty cset then dotstar else
          create (Form.Star t)
      | _ ->
        create (Form.Star t)

    let is_dotstar t =
      match t.rep with
      | Form.Neg t -> is_zero t
      | _ -> false

    let rec cat2 x1 x2 =
      match x1.rep with
      | Form.Cat2 (x0,x1) -> cat2 x0 (cat2 x1 x2) | _ -> (* reassoc *)
        if is_zero x1 then zero else
          if is_zero x2 then zero else
            if is_eps x1 then x2 else
              if is_eps x2 then x1 else
                if is_dotstar x1 && is_dotstar x2 then x1 else
                  create (Form.Cat2 (x1,x2))

    let merge2_cspec (sense1,cset1) (sense2,cset2) =
      match (sense1,sense2) with
      | (Pos,Pos) -> (Pos, Cset.union cset1 cset2)
      | (Pos,Neg) -> (Neg, Cset.diff cset2 cset1)
      | (Neg,Pos) -> (Neg, Cset.diff cset1 cset2)
      | (Neg,Neg) -> (Neg, Cset.intersect cset1 cset2)


    let filter_merge_csets xs =
      let yes,no =
        List.partition_map (fun x -> match x.rep with
        | Form.Poslit x -> Left (Pos,x)
        | Form.Neglit x -> Left (Neg,x)
        | _ -> Right x
        ) xs
      in
      match yes with
      | [] -> no
      | x::xs ->
        let (sense,cset) = List.fold_left merge2_cspec x xs in
        lit sense cset :: no


    let alts xs =
      let xs = List.concat_map
        (fun x -> match x.rep with Form.Alts xs -> xs | _ -> [x]) xs in
      let xs = filter_merge_csets xs in
      let xs = sort_and_remove_dups ~compare xs in
      let xs = filter_out xs ~f:is_zero in
      if List.exists is_dotstar xs then dotstar else
        match xs with
        | [x] -> x
        | xs -> create (Form.Alts xs)

    let deriv_form ~deriv char t0 =
      match (unroll t0) with
      | Form.Eps -> zero
      | Form.Poslit cset -> if Cset.contains cset char then eps else zero
      | Form.Neglit cset -> if Cset.contains cset char then zero else eps
      | Form.Star t -> cat2 (deriv char t) t0
      | Form.Neg t -> neg (deriv char t)
      | Form.Alts ts -> alts (List.map (deriv char) ts)
      | Form.Cat2 (t1,t2) ->
        if (nullable t1)
        then alts [cat2 (deriv char t1) t2 ; deriv char t2]
        else       cat2 (deriv char t1) t2

    (*let array_get = Array.get*)
    let array_get = Array.unsafe_get

    let rec deriv char t =
      match array_get t.d_array (Char.code char) with Some t_res -> t_res | None ->
        let rec loop = function
          | (cset,doptref)::d_map ->
            if (Cset.contains cset char) then
              match (!doptref) with
              | Some t_res -> t_res
              | None ->
                let t_res = deriv_form ~deriv char t in
                doptref := Some t_res;
                List.iter (fun x ->
                  Array.set t.d_array (Char.code x) (Some t_res);
                ) (Cset.rep cset);
                t_res
            else
              loop d_map
          | [] ->
            match (t.d_unspec) with
            | Some t_res -> t_res
            | None ->
              assert (not (Cset.contains t.cover char));
              let t_res = deriv_form ~deriv char t in
              t.d_unspec <- Some t_res;
              List.iter (fun x ->
                Array.set t.d_array (Char.code x) (Some t_res);
              ) (Cset.rep (Cset.diff Cset.any t.cover));
              t_res
        in
        loop t.d_map

    let deriv char t =
      t.is_state_of_dfa <- true;
      deriv char t

    let reverse t0 =
      let rec walk t0 =
        match (unroll t0) with
        | Form.Eps -> t0
        | Form.Poslit _ -> t0
        | Form.Neglit _ -> t0
        | Form.Star t -> star (walk t)
        | Form.Neg t -> neg (walk t)
        | Form.Cat2 (t1,t2) -> cat2 (walk t2) (walk t1)
        | Form.Alts ts -> alts (List.map walk ts)
      in
      walk t0

    (* non-primitive builders *)

    let conjs xs = neg (alts (List.map neg xs))
    let unanchor t = cat2 dotstar t
    let cats xs = List.fold_right cat2 xs eps
    let one_of xs = poslit (Cset.create xs)
    let not_one_of xs = neglit (Cset.create xs)
    let dot = not_one_of []
    let char x = one_of [x]
    let string string = cats (List.map char (explode string))
    let query x = alts [eps; x]
    let plus x = cats [x; star x]

    let repeat_count t n = assert(n >= 0);
      iterate n (cat2 t) eps

    let repeat_count_or_more t n = assert(n >= 0);
      iterate n (cat2 t) (star t)

    let repeat_count_range t n m = assert(n >= 0); assert(m >= n);
      let t_opt = query t in
      iterate (m-n) (cat2 t_opt) (repeat_count t n)

    (* [representative_first_chars t] returns a set of chars which if used to calc
       derivs of t we obtain a set of regexps which contains every regexp which
       can be obtained by forming a deriv of t for any char. *)

    let representative_first_chars t =
      assert (List.for_all (fun cset -> not (Cset.is_empty cset)) t.partition);
      let chars = List.map Cset.pick_one_exn t.partition in
      let rest = Cset.diff Cset.any t.cover in
      if (Cset.is_empty rest) then chars else
        Cset.pick_one_exn rest :: chars


    let unroll_one_step t =
      let xs =
        List.map
          (fun cset ->
            assert (not (Cset.is_empty cset));
            let char = Cset.pick_one_exn cset in
            let t' = deriv char t in
            let atom = (Pos,cset) in
            atom,t') t.partition
      in
      let ys =
        let rest = Cset.diff Cset.any t.cover in
        if (Cset.is_empty rest) then [] else
          let char = Cset.pick_one_exn rest in
          let t' = deriv char t in
          let atom = (Neg,t.cover) in
          [atom,t']
      in
      ys @ xs


  end


  module Parse : sig

    val parse : string -> [ `Ok of Gold.t | `Error of string * int ]

  end = struct

    exception Parse_error of string * int

    let err i s = raise (Parse_error (s,i))

    let after_escape i xs cont = let i = i+1 in match xs with
      | [] -> err i "unterminated escape"
      | x::xs -> match x with
        | 'n' -> cont i '\n' xs
        | 't' -> cont i '\t' xs
        | _ ->
          if not (is_regexp_special_char x) then err i "unknown escape" else
            cont i x xs

    let char_in_cspec i x xs cont =
      match x with
      | '\\' -> after_escape i xs cont
      | _ -> cont i x xs

    let cspec_items i x xs cont =
      let rec loop i acc x xs =
        match x with
        | ']' -> cont i acc xs
        | _ ->
          char_in_cspec i x xs (fun i c1 xs ->
            let i = i+1 in match xs with
            | [] -> err i "unclosed ["
            | '-'::xs ->
              begin let i = i+1 in match xs with
              | [] -> err i "unclosed [ after -"
              | x::xs ->
                match x with
                | ']' -> err i "unexpected ]"
                | _ ->
                  char_in_cspec i x xs (fun i c2 xs ->
                    let i = i+1 in match xs with
                    | [] -> err i "unclosed [.."
                    | x::xs ->
                      loop i (char_range c1 c2 @ acc) x xs
                  )
              end
            | x::xs -> loop i (c1::acc) x xs
          )
      in
      loop i [] x xs

    let after_open_square i xs cont =
      let i = i+1 in match xs with
      | [] -> err i "unclosed ["
      | '^'::xs ->
        begin
          let i = i+1 in match xs with
          | [] -> err i "unclosed [^"
          | x::xs ->
            cspec_items i x xs (fun i chars xs -> cont i (Gold.not_one_of chars) xs)
        end
      | x::xs ->
        cspec_items i x xs (fun i chars xs -> cont i (Gold.one_of chars) xs)

    let rec number i ~acc xs cont =
      let i = i+1 in match xs with | [] -> err i "unclosed {" | x::xs ->
        match Char.get_digit x with
        | None -> cont i ~num:acc x xs
        | Some d -> number i ~acc:(10*acc + d) xs cont

    let after_open_brace i xs cont =
      let i = i+1 in match xs with | [] -> err i "unclosed {" | x::xs ->
        match Char.get_digit x with
        | None -> err i "expected digit after {"
        | Some d ->
          number i ~acc:d xs (fun i ~num:n x xs ->
            match x with
            | '}' -> cont i ~rep_f:(fun t -> Gold.repeat_count t n) xs
            | ',' ->
              begin
                let i = i+1 in match xs with | [] -> err i "unclosed {" | x::xs ->
                  match x with
                  | '}' -> cont i ~rep_f:(fun t -> Gold.repeat_count_or_more t n) xs
                  | _ ->
                    match Char.get_digit x with
                    | None -> err i "unexpected char in {..}"
                    | Some d ->
                      number i ~acc:d xs (fun i ~num:m x xs ->
                        match x with
                        | '}' ->
                          if m < n then err i "{n,m} - m must be bigger than n" else
                            cont i ~rep_f:(fun t -> Gold.repeat_count_range t n m) xs
                        | _ -> err i "unexpected char in {..}"
                      )
              end
            | _ -> err i "unexpected char in {..}"
          )

    let build_sense sense gold = match sense with | Pos -> gold | Neg -> Gold.neg gold

    let rev_cats xs = Gold.cats (List.rev xs)

    let consopt opt xs = match opt with None -> xs | Some x -> x::xs

    let parse_exn =
      let rec loop i rp disjuncts conjuncts seq1 sense seq2 lastopt xs =
        let i = i+1 in match xs with
        | [] ->
          begin match rp with | Some _ -> err i "unclosed (" | None ->
            Gold.alts (
              Gold.conjs (
                rev_cats (
                  build_sense sense (
                    rev_cats (consopt lastopt seq2)
                  ) :: seq1
                ) :: conjuncts
              ) :: disjuncts
            )
          end
        | x::xs ->
          match x with
          | ')' ->
            begin match rp with None -> err i "unexpected )" | Some cont ->
              cont i (
                Gold.alts (
                  Gold.conjs (
                    rev_cats (
                      build_sense sense (
                        rev_cats (consopt lastopt seq2)
                      ) :: seq1
                    ) :: conjuncts
                  ) :: disjuncts
                )
              ) xs
            end
          | '|' ->
            loop i rp (
              Gold.conjs (
                rev_cats (
                  build_sense sense (
                    rev_cats (consopt lastopt seq2)
                  ) :: seq1
                ) :: conjuncts
              ) :: disjuncts
            ) [] [] Pos [] None xs
          | '&' ->
            loop i rp disjuncts (
              rev_cats (
                build_sense sense (
                  rev_cats (consopt lastopt seq2)
                ) :: seq1
              ) :: conjuncts
            ) [] Pos [] None xs
          | '!' ->
            loop i rp disjuncts conjuncts (
              build_sense sense (
                rev_cats (consopt lastopt seq2)
              ) :: seq1
            ) Neg [] None xs
          | '*' ->
            begin match lastopt with None -> err i "unexpected *" | Some last ->
              loop i rp disjuncts conjuncts seq1 sense seq2 (Some (Gold.star last)) xs
            end
          | '+' ->
            begin match lastopt with None -> err i "unexpected +" | Some last ->
              loop i rp disjuncts conjuncts seq1 sense seq2 (Some (Gold.plus last)) xs
            end
          | '?' ->
            begin match lastopt with None -> err i "unexected ?" | Some last ->
              loop i rp disjuncts conjuncts seq1 sense seq2 (Some (Gold.query last)) xs
            end
          | '{' ->
            begin match lastopt with None -> err i "unexected {" | Some last ->
              after_open_brace i xs (fun i ~rep_f xs ->
                loop i rp disjuncts conjuncts seq1 sense seq2 (Some (rep_f last)) xs
              )
            end
          | '}' ->
            err i "unexpected }"
          | '.' ->
            loop i rp disjuncts conjuncts seq1 sense (consopt lastopt seq2)
              (Some Gold.dot) xs
          | '\\' ->
            after_escape i xs (fun i c xs ->
              loop i rp disjuncts conjuncts seq1 sense (consopt lastopt seq2)
                (Some (Gold.char c)) xs)
          | ']' ->
            err i "unexpected ]"
          | '[' ->
            after_open_square i xs (fun i gold xs ->
              loop i rp disjuncts conjuncts seq1 sense (consopt lastopt seq2)
                (Some gold) xs)
          | '(' ->
            loop i (
              Some (
                fun i gold xs ->
                  loop i rp disjuncts conjuncts seq1 sense (consopt lastopt seq2)
                    (Some gold) xs
              )
            ) [] [] [] Pos [] None xs
          | c ->
            loop i rp disjuncts conjuncts seq1 sense (consopt lastopt seq2)
              (Some (Gold.char c)) xs
      in
      fun pat ->
        let xs = explode pat in
        loop 0 None [] [] [] Pos [] None xs

    let parse pat =
      try `Ok (parse_exn pat)
      with Parse_error (s,i) -> `Error (s,i)

  end

  module Sub = struct

    include Gold

    let parse ~pat = Parse.parse pat

    let parse_exn ~pat =
      match (parse ~pat) with
      | `Ok x -> x
      | `Error (s,i) -> failwith (sprintf "red parser -- %s -- char %d" s i)

  end

  module Toplevel = struct

    type t = {
      anchor_start : bool;
      gold : Gold.t;
      anchor_end : bool;
    }

    let create ~anchor_start ~anchor_end gold = {
      anchor_start;
      gold;
      anchor_end;
    }

    let parse ~pat =
      let n = String.length pat in
      let anchor_start =
        match pat with "" -> false | _ -> Char.equal (String.get pat 0) '^'
      in
      let anchor_end_dist =
        match pat with "" -> 0 | _ ->
          if Char.equal (String.get pat (n-1)) '$'
          then
            1 + (
              match pat with | "$" -> 0 | _ ->
                if Char.equal (String.get pat (n-2)) '\\' then 1 else 0
            )
          else 0
      in
      let anchor_end = not (Int.equal anchor_end_dist 0) in
      let pat_trimmed =
        if anchor_start || anchor_end then
          String.slice pat
            (if anchor_start then 1 else 0) (n - anchor_end_dist)
        else pat
      in
      match (Parse.parse pat_trimmed) with
      | `Error (s,i) -> `Error (s, if anchor_start then 1+i else i)
      | `Ok gold -> `Ok {
        anchor_start;
        gold;
        anchor_end;
      }

    let parse_exn ~pat =
      match (parse ~pat) with
      | `Ok x -> x
      | `Error (s,i) -> failwith (sprintf "red parser -- %s -- char %d" s i)

    let to_string t =
      sprintf "%s%s%s"
        (if t.anchor_start then "^" else "")
        (Gold.to_string t.gold)
        (if t.anchor_end then "$" else "")

  end

  type t = Toplevel.t
  let toplevel = Toplevel.create
  let parse = Toplevel.parse
  let parse_exn = Toplevel.parse_exn
  let to_string = Toplevel.to_string

  let string_get = String.get

  let search_first t string ~start_pos ~last_pos ~step =
    let rec loop t i =
      if (Gold.nullable t) then Some i
      else
        if (Gold.is_zero t || Int.equal i last_pos) then None
        else
          let char = string_get string i in
          let t = Gold.deriv char t in
          loop t (i + step)
    in
    loop t start_pos

  let search_first_step1 t string ~start_pos ~last_pos =
    let rec loop t i =
      if (Gold.nullable t) then Some i
      else
        if (Gold.is_zero t || Int.equal i last_pos) then None
        else
          let char = string_get string i in
          let t = Gold.deriv char t in
          loop t (i + 1)
    in
    loop t start_pos

  let search_last t string ~start_pos ~last_pos ~step =
    let rec loop ~last t i =
      let last = (if (Gold.nullable t) then Some i else last) in
      if (Gold.is_zero t || Int.equal i last_pos) then last
      else
        let char = string_get string i in
        let t = Gold.deriv char t in
        loop ~last t (i + step)
    in
    loop ~last:None t start_pos

  let search_exact_end t string ~start_pos ~last_pos ~step =
    let rec loop t i =
      if Int.equal i last_pos then (if (Gold.nullable t) then Some i else None)
      else
        if Gold.is_zero t then None
        else
          let char = string_get string i in
          let t = Gold.deriv char t in
          loop t (i + step)
    in
    loop t start_pos

  let is_some = Option.is_some

  let matches t =
    let gold = t.Toplevel.gold in
    match t.Toplevel.anchor_start, t.Toplevel.anchor_end with

    | true,true ->
      if !debug then message "matches(^$)";
      fun s ->
      let n = String.length s in
      is_some (search_exact_end gold s ~start_pos:0 ~last_pos:n ~step:(1))

    | true,false ->
      if !debug then message "matches(^)";
      fun s ->
      let n = String.length s in
      is_some (search_first gold s ~start_pos:0 ~last_pos:n ~step:1)

    | false,true ->
      if !debug then message "matches($)";
      let r_gold = Gold.reverse gold in (* search backwards from end *)
      (* important that we build r_gold *before* take the string arg *)
      fun s ->
      let n = String.length s in
      is_some (search_first r_gold s ~start_pos:(n-1) ~last_pos:(-1) ~step:(-1))

    | false,false ->
      if !debug then message "matches(unanchored)";
      let u_gold = Gold.unanchor gold in
      fun s ->
        let n = String.length s in
        (*is_some (search_first u_gold s ~start_pos:0 ~last_pos:n ~step:1)*)
        is_some (search_first_step1 u_gold s ~start_pos:0 ~last_pos:n)


  let search_style style =
    match style with
    | `first -> search_first
    | `last -> search_last
    | `exact_end -> search_exact_end

  let search_variations t ~start_pos ~strat =
    let gold = t.Toplevel.gold in
    match t.Toplevel.anchor_start, t.Toplevel.anchor_end with

    | true,true ->
      if !debug then message "search(^$)";
      fun s ->
      let n = String.length s in
      begin
        match (
          search_exact_end gold s ~start_pos ~last_pos:n ~step:(1)
        ) with
        | None -> None
        | Some i -> assert (Int.equal i n); Some (start_pos,n)
      end

    | true,false ->
      if !debug then message "search(^)";
      fun s ->
      let n = String.length s in
      begin
        match (
          match strat with
          | `soonest_shortest -> search_style `first
          | `soonest_leftmost -> search_style `first
          | `rightmost_longest -> search_style `last
          | `leftmost_longest -> search_style `last
        )
          gold s ~start_pos ~last_pos:n ~step:1
        with
        | None -> None
        | Some i -> Some (start_pos,i)
      end

    | false,true ->
      if !debug then message "search($)";
      let r_gold = Gold.reverse gold in (* search backwards from end *)
      fun s ->
      let n = String.length s in
      begin
        match (
          match strat with
          | `soonest_shortest -> search_style `first
          | `soonest_leftmost -> search_style `last
          | `rightmost_longest -> search_style `last
          | `leftmost_longest -> search_style `last
        )
          r_gold s ~start_pos:(n-1) ~last_pos:(start_pos-1) ~step:(-1)
        with
        | None -> None
        | Some i -> Some (i+1,n)
      end

    | false,false -> match strat with

      | `soonest_shortest ->
        if !debug then message "search(unanchored) -- `soonest_shortest";
        let u_gold = Gold.unanchor gold in
        let r_gold = Gold.reverse gold in
        fun s ->
        let n = String.length s in
        begin
          match (
            search_style `first u_gold s ~start_pos ~last_pos:n ~step:1
          ) with
          | None -> None
          | Some j ->
            match j with 0 -> Some (1,1) (* (0,0) is bad for String.slice *) | _ ->
              match (
                search_style `first r_gold s ~start_pos:(j-1) ~last_pos:(-1) ~step:(-1)
              ) with
              | None -> failwith "impossible"
              | Some i -> Some (i+1,j)
        end

      | `soonest_leftmost ->
        if !debug then message "search(unanchored) -- `soonest_leftmost";
        let u_gold = Gold.unanchor gold in
        let r_gold = Gold.reverse gold in
        fun s ->
        let n = String.length s in
        begin
          match (
            search_style `first u_gold s ~start_pos ~last_pos:n ~step:1
          ) with
          | None -> None
          | Some j ->
            match j with 0 -> Some (1,1) (* (0,0) is bad for String.slice *) | _ ->
              match (
                search_style `last r_gold s ~start_pos:(j-1) ~last_pos:(-1) ~step:(-1)
              ) with
              | None -> failwith "impossible"
              | Some i -> Some (i+1,j)
        end

      | `rightmost_longest ->
        if !debug then message "search(unanchored) -- `rightmost_longest";
        let u_gold = Gold.unanchor gold in
        let r_gold = Gold.reverse gold in
        fun s ->
        let n = String.length s in
        begin
          match (
            search_style `last u_gold s ~start_pos ~last_pos:n ~step:1
          ) with
          | None -> None
          | Some j ->
            (*match j with 0 -> Some (1,1) (* (0,0) is bad for String.slice *) | _ ->*)
              match (
                search_style `last r_gold s ~start_pos:(j-1) ~last_pos:(-1) ~step:(-1)
              ) with
              | None -> failwith "impossible"
              | Some i -> Some (i+1,j)

        end

      | `leftmost_longest ->
        if !debug then message "search(unanchored) -- `leftmost_longest";
        let r_gold = Gold.reverse gold in
        let ur_gold = Gold.unanchor r_gold in
        fun s ->
        let n = String.length s in
        begin
          (* search backwards, then forward *)
          match (
            search_style `last ur_gold s ~start_pos:(n-1) ~last_pos:(start_pos-1) ~step:(-1)
          ) with
          | None -> None
          | Some i ->
            (*if Int.equal i n then Some (n,n) else*)
              match (
                search_style `last gold s ~start_pos:(i+1) ~last_pos:n ~step:1
              ) with
              | None -> failwith "impossible"
              | Some j -> Some (i+1,j)
        end


  (* user entry - choose defaults *)
  let search t ?(start_pos=0) ?(strat=`leftmost_longest) s =
    search_variations t ~start_pos ~strat s


  let debug_print_cache t =
    let gold = t.Toplevel.gold in
    let root_gold =
      if t.Toplevel.anchor_start then gold else Gold.unanchor gold
    in
    Gold.debug_print_cache root_gold


  (* witness calculation... *)
  (* witness - find a path to a nullable state if one exists...  *)

  module Set =
    Stdlib.Set.Make(struct
        type t = Gold.t
        let compare = Sub.compare
      end)

  module MutSet : sig
    type t
    val create : unit -> t
    val mem : t -> Gold.t -> bool
    val add : t -> Gold.t -> unit
  end = struct
    type t = { mutable set : Set.t }
    let create () = { set = Set.empty }
    let add t x = t.set <- Set.add x t.set
    let mem t x = Set.mem x t.set
  end

  (* fail-continuation version - avoid blowing stack *)
  let witness t =
    let visited = MutSet.create () in
    let rec wit t ~path ~fail =
      if Gold.is_zero t then fail() else
        if MutSet.mem visited t then fail() else
          if Gold.nullable t then Some (implode (List.rev path)) else
            let () = MutSet.add visited t in
            let rec loop = function
              | [] -> fail()
              | char::chars ->
                wit (Gold.deriv char t) ~path:(char::path) ~fail:(fun () -> loop chars)
            in
            loop (Gold.representative_first_chars t)
    in
    wit t ~path:[] ~fail:(fun () -> None)


  type res = [ `same | `in_first_not_second of string | `in_second_not_first of string ]
  let test_for_equivalence g1 g2 : res =
    match witness (Gold.conjs [g1; Gold.neg g2]) with
    | Some s -> `in_first_not_second s
    | None ->
      match witness (Gold.conjs [g2; Gold.neg g1]) with
      | Some s -> `in_second_not_first s
      | None -> `same


  module Routes : sig

    type t
    val to_string : t -> string

    val zero : t
    val empty : t
    val cycle : Gold.t -> t
    val alt : t list -> t
    val prefix : Gold.t -> t -> t

    val tie_knot : t -> Gold.t -> t
    val to_gold_after_cycles_are_removed : t -> Gold.t

  end = struct

    module Path : sig

      type t
      val to_gold : t -> Gold.t
      val empty : t
      val seq : t -> t -> t
      val atom : Gold.t -> t
      val alt : t list -> t
      val knot : t list -> t

    end = struct

      type t = Gold.t

      let to_gold t = t
      let empty = Gold.eps
      let seq t1 t2 = Gold.cats [t1;t2]
      let atom gold = gold
      let alt = Gold.alts
      let knot cycles = Gold.star (Gold.alts cycles)

    end

    type route = Path of Path.t | Cycle of Path.t * Gold.t

    type t = {
      routes : route list
    }

    let to_string =
      let route_to_string = function
        | Path path -> Gold.to_string (Path.to_gold path)
        | Cycle (path,t) ->
          sprintf "%s %s" (Gold.to_string (Path.to_gold path)) (Gold.to_tag_string t)
      in
      fun t ->
        sprintf "(%s)" (String.concat " + " (List.map route_to_string t.routes))

    let zero = { routes = [] }
    let empty = { routes = [Path Path.empty] }
    let cycle gold = { routes = [Cycle (Path.empty,gold)] }
    let alt ts = { routes = List.concat_map (fun t -> t.routes) ts }

    let cons_path_to_route x = function
      | Path p -> Path (Path.seq x p)
      | Cycle (p,gold) -> Cycle (Path.seq x p,gold)


    let collect_up_common xs =
      let non_cycles,cycles =
        List.partition_map (function
        | Path p -> Left p
        | Cycle (p,g) -> Right (p,g)) xs
      in
      let collected_cycles =
        let compare_gold_cycle (_,g1) (_,g2) = Gold.compare g1 g2 in
        let not_eq_gold_cycle (_,g1) (_,g2) = not (Gold.equal g1 g2) in
        let cycles = List.sort compare_gold_cycle cycles in
        let grouped = List.group cycles ~break:not_eq_gold_cycle in
        List.map (function [] -> assert false | (p,g)::xs ->
          Cycle (Path.alt (p ::
                              List.map (fun (p,g') ->
                                assert (Gold.equal g g');
                                p) xs)
                    , g)) grouped
      in
      Path (Path.alt non_cycles) :: collected_cycles


    let cons_path path routes = {
      routes = List.map (cons_path_to_route path) (collect_up_common routes)
    }

    let prefix gold t = cons_path (Path.atom gold) t.routes

    let tie_knot t gold =
      let rec loop yes no = function | [] -> yes,no | x::xs ->
        match x with
        | Path _ -> loop yes (x::no) xs
        | Cycle (path,gold') ->
          if (Gold.equal gold gold')
          then loop (path::yes) no xs
          else loop yes (x::no) xs
      in
      match (loop [] [] t.routes) with
      | ([],_) -> t
      | (cycles,routes) -> cons_path (Path.knot cycles) routes

    let to_gold_after_cycles_are_removed t =
      let paths =
        let rec loop acc = function | [] -> acc | x::xs ->
          match x with
          | Path p -> loop (p::acc) xs
          | Cycle _ -> assert false (* no cycles should remain *)
          (*| Cycle (_,g) ->
            message "to_gold_after_cycles_are_removed - assert false - %s"
              (Gold.to_tag_string g); loop acc xs*)
        in loop [] t.routes
      in
      let gold = Gold.alts (List.map Path.to_gold paths) in
      gold


  end


  let transform_via_and_forcing_dfa gold =
    let tab = ref "" in
    let rec wander visiting gold =
      let old_tab = !tab in
      tab := "  " ^ old_tab;
      let routes =
        if Gold.is_zero gold then (
          if !debug then message "%swander: %s -> zero" !tab (Gold.to_tag_string gold);
          Routes.zero
        )else
          match (Set.mem gold visiting) with
          | true ->
            if !debug then message "%swander: %s -> cycle" !tab (Gold.to_tag_string gold);
            Routes.cycle gold
          | false ->
            let visiting = Set.add gold visiting in
            let unrolled = Gold.unroll_one_step gold in
            if !debug then message "%swander(pre): %s = %s = %s" !tab
              (Gold.to_tag_string gold) (Gold.to_string gold)
              (String.concat " / "
                 (List.map (fun ((sense,cset),gold) ->
                   sprintf "%s %s"
                     (Gold.to_string (Gold.lit sense cset)) (Gold.to_tag_string gold)
                  ) unrolled));
            let routes =
              Routes.alt (
                List.map (fun ((sense,cset),gold) ->
                  Routes.prefix (Gold.lit sense cset)  (wander visiting gold)) unrolled
              )
            in
            let routes =
              if Gold.nullable gold
              then Routes.alt [Routes.empty; routes]
              else routes
            in
            if !debug then message "%swander(post): %s -> %s"
              !tab (Gold.to_tag_string gold) (Routes.to_string routes);
            let routes = Routes.tie_knot routes gold in
            if !debug then message "%swander(knot): %s -> %s"
              !tab (Gold.to_tag_string gold) (Routes.to_string routes);
            routes
      in
      tab := old_tab;
      routes
    in
    let visiting = Set.empty in
    let routes = wander visiting gold in
    let gold = Routes.to_gold_after_cycles_are_removed routes in
    gold


end


(* useful combinations of functor-app, parse & search *)

let search_exn ~pat =
  let module Red = F() in
  let red = Red.parse_exn ~pat in
  fun ?strat s -> (* note staging *)
    Red.search red ?strat s

let select_exn ~pat =
  let module Red = F() in
  let red = Red.parse_exn ~pat in
  fun ?strat s -> (* note staging *)
    match (Red.search red ?strat s) with
    | None -> None
    | Some (p,q) -> Some (String.slice s p q)

let matches_exn ~pat =
  let module Red = F() in
  let red = Red.parse_exn ~pat in
  fun s -> (* note staging *)
    Red.matches red s
