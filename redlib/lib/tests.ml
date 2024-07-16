open Red

(* test for position of parse errors / absence of parse errors *)

let err pos pat =
  let module Red = F() in
  match (Red.parse ~pat) with `Error (_,i) -> (pos=i) | `Ok _ -> (pos=0)

let%test _ = err 0 ""

let%test _ = err 0 "a"
let%test _ = err 0 "."
let%test _ = err 0 "\n"
let%test _ = err 0 "\t"
let%test _ = err 0 "-"

let%test _ = err 0 "^"
let%test _ = err 0 "$"
let%test _ = err 0 "^$"
let%test _ = err 0 ".^"
let%test _ = err 0 "$."

let%test _ = err 2 "\\" (* unterminated escape *)
let%test _ = err 0 "\\\\"
let%test _ = err 0 "\\("
let%test _ = err 0 "\\)"
let%test _ = err 0 "\\["
let%test _ = err 0 "\\]"
let%test _ = err 0 "\\&"
let%test _ = err 0 "\\|"
let%test _ = err 0 "\\!"
let%test _ = err 0 "\\*"
let%test _ = err 0 "\\+"
let%test _ = err 0 "\\?"
let%test _ = err 0 "\\{"
let%test _ = err 0 "\\}"
let%test _ = err 0 "\\."
let%test _ = err 0 "\\-"
let%test _ = err 0 "\\^"
let%test _ = err 0 "\\$"
let%test _ = err 0 "\\n"
let%test _ = err 0 "\\t"
let%test _ = err 0 "\\$."
let%test _ = err 2 "\\a" (* unknown escape *)
let%test _ = err 2 "\\b" (* unknown escape *)

let%test _ = err 0 "[]"
let%test _ = err 0 "[^]"
let%test _ = err 0 "[^^]"
let%test _ = err 0 "[[^]"
let%test _ = err 0 "[-]"
let%test _ = err 0 "[-a]"
let%test _ = err 0 "[a[]"
let%test _ = err 0 "[a\\]]"
let%test _ = err 0 "[a\\-]"
let%test _ = err 0 "[a\\[]"
let%test _ = err 2 "["
let%test _ = err 1 "]"
let%test _ = err 3 "[a"
let%test _ = err 4 "[a-"
let%test _ = err 4 "[a-]"
let%test _ = err 5 "[a-b"
let%test _ = err 4 "[a]]"

let%test _ = err 0 "a{3}"
let%test _ = err 0 "a{3,}"
let%test _ = err 0 "a{3,7}"
let%test _ = err 0 "a{03}"
let%test _ = err 0 "a{13,}"
let%test _ = err 0 "a{13,1009}"

let%test _ = err 1 "{3}"
let%test _ = err 3 "(|{3})"
let%test _ = err 3 "a{"
let%test _ = err 3 "a{}"
let%test _ = err 3 "a{a"
let%test _ = err 4 "a{3"
let%test _ = err 4 "a{3a"
let%test _ = err 5 "a{3,"
let%test _ = err 5 "a{3,a"
let%test _ = err 6 "a{3,7"
let%test _ = err 6 "a{3,7,"
let%test _ = err 6 "a{3,2}"
let%test _ = err 1 "}"

let%test _ = err 0 "()"
let%test _ = err 0 "(())"
let%test _ = err 0 "(()())"
let%test _ = err 2 "("
let%test _ = err 4 "(()"
let%test _ = err 3 "())"
let%test _ = err 4 "[]("
let%test _ = err 3 "[])"

let%test _ = err 0 "&"
let%test _ = err 0 "!"
let%test _ = err 0 "|"
let%test _ = err 0 "&&"
let%test _ = err 0 "||"
let%test _ = err 0 "!!"

let%test _ = err 0 "a*"
let%test _ = err 0 "()*"
let%test _ = err 0 "[]*"

let%test _ = err 1 "*"
let%test _ = err 1 "+"
let%test _ = err 1 "?"
let%test _ = err 2 "(*)"
let%test _ = err 2 "|*"
let%test _ = err 2 "^*"


(* test for basic forms - use exact matching  *)

let exact pat b inp  = matches_exn ~pat:("^" ^ pat ^ "$") inp = not(b=0)

let%test _ = exact ""             1 ""
let%test _ = exact ""             0 "a"

let%test _ = exact "a"            0 ""
let%test _ = exact "a"            1 "a"
let%test _ = exact "a"            0 "b"
let%test _ = exact "a"            0 "aa"

let%test _ = exact "."            0 ""
let%test _ = exact "."            1 "a"
let%test _ = exact "."            1 "b"
let%test _ = exact "."            1 "."
let%test _ = exact "."            0 "ab"

let%test _ = exact "!a"           1 ""
let%test _ = exact "!a"           0 "a"
let%test _ = exact "!a"           1 "b"
let%test _ = exact "!a"           1 "aa"

let%test _ = exact "a?"           1 ""
let%test _ = exact "a?"           1 "a"
let%test _ = exact "a?"           0 "b"
let%test _ = exact "a?"           0 "aa"

let%test _ = exact "a*"           1 ""
let%test _ = exact "a*"           1 "a"
let%test _ = exact "a*"           1 "aa"
let%test _ = exact "a*"           1 "aaa"
let%test _ = exact "a*"           0 "aba"

let%test _ = exact "a+"           0 ""
let%test _ = exact "a+"           1 "a"
let%test _ = exact "a+"           1 "aa"
let%test _ = exact "a+"           1 "aaa"
let%test _ = exact "a+"           0 "aba"

let%test _ = exact "[ab]"         0 ""
let%test _ = exact "[ab]"         1 "a"
let%test _ = exact "[ab]"         1 "b"
let%test _ = exact "[ab]"         0 "c"
let%test _ = exact "[ab]"         0 "ab"

let%test _ = exact "[^ab]"        0 ""
let%test _ = exact "[^ab]"        0 "a"
let%test _ = exact "[^ab]"        0 "b"
let%test _ = exact "[^ab]"        1 "c"
let%test _ = exact "[^ab]"        0 "cc"

let%test _ = exact "[a-cx-y]"     0 ""
let%test _ = exact "[a-cx-y]"     1 "a"
let%test _ = exact "[a-cx-y]"     1 "b"
let%test _ = exact "[a-cx-y]"     1 "c"
let%test _ = exact "[a-cx-y]"     0 "d"
let%test _ = exact "[a-cx-y]"     0 "w"
let%test _ = exact "[a-cx-y]"     1 "x"
let%test _ = exact "[a-cx-y]"     1 "y"
let%test _ = exact "[a-cx-y]"     0 "z"
let%test _ = exact "[a-cx-y]"     0 "ab"

let%test _ = exact "[a-a]"        0 ""
let%test _ = exact "[a-a]"        1 "a"
let%test _ = exact "[a-a]"        0 "b"
let%test _ = exact "[a-a]"        0 "aa"

let%test _ = exact "[b-a]"        0 ""
let%test _ = exact "[b-a]"        0 "a"
let%test _ = exact "[b-a]"        0 "b"
let%test _ = exact "[b-a]"        0 "ab"

let%test _ = exact "ab"           0 ""
let%test _ = exact "ab"           0 "a"
let%test _ = exact "ab"           0 "b"
let%test _ = exact "ab"           1 "ab"
let%test _ = exact "ab"           0 "ba"
let%test _ = exact "ab"           0 "aba"

let%test _ = exact "a|b"          0 ""
let%test _ = exact "a|b"          1 "a"
let%test _ = exact "a|b"          1 "b"
let%test _ = exact "a|b"          0 "c"
let%test _ = exact "a|b"          0 "ab"

let%test _ = exact "a&b"          0 ""
let%test _ = exact "a&b"          0 "a"
let%test _ = exact "a&b"          0 "b"
let%test _ = exact "a&b"          0 "c"
let%test _ = exact "a&b"          0 "ab"

let%test _ = exact "[ab]&[bc]"    0 ""
let%test _ = exact "[ab]&[bc]"    0 "a"
let%test _ = exact "[ab]&[bc]"    1 "b"
let%test _ = exact "[ab]&[bc]"    0 "c"
let%test _ = exact "[ab]&[bc]"    0 "bb"

let%test _ = exact "a?b?"         1 ""
let%test _ = exact "a?b?"         1 "a"
let%test _ = exact "a?b?"         1 "b"
let%test _ = exact "a?b?"         0 "c"
let%test _ = exact "a?b?"         0 "aa"
let%test _ = exact "a?b?"         1 "ab"
let%test _ = exact "a?b?"         0 "ba"
let%test _ = exact "a?b?"         0 "abb"

let%test _ = exact "a?a?"         1 ""
let%test _ = exact "a?a?"         1 "a"
let%test _ = exact "a?a?"         1 "aa"
let%test _ = exact "a?a?"         0 "aaa"
let%test _ = exact "a?a?"         0 "b"
let%test _ = exact "a?a?"         0 "ab"
let%test _ = exact "a?a?"         0 "aba"

let%test _ = exact "[ab]c"        0 ""
let%test _ = exact "[ab]c"        0 "a"
let%test _ = exact "[ab]c"        0 "b"
let%test _ = exact "[ab]c"        0 "c"
let%test _ = exact "[ab]c"        0 "aa"
let%test _ = exact "[ab]c"        0 "ab"
let%test _ = exact "[ab]c"        1 "ac"
let%test _ = exact "[ab]c"        0 "ba"
let%test _ = exact "[ab]c"        0 "bb"
let%test _ = exact "[ab]c"        1 "bc"
let%test _ = exact "[ab]c"        0 "ca"
let%test _ = exact "[ab]c"        0 "cb"
let%test _ = exact "[ab]c"        0 "cc"
let%test _ = exact "[ab]c"        0 "abc"

let%test _ = exact "(ab)?"        1 ""
let%test _ = exact "(ab)?"        0 "a"
let%test _ = exact "(ab)?"        0 "b"
let%test _ = exact "(ab)?"        1 "ab"
let%test _ = exact "(ab)?"        0 "ba"
let%test _ = exact "(ab)?"        0 "aba"

let%test _ = exact "(ab)*"        1 ""
let%test _ = exact "(ab)*"        0 "a"
let%test _ = exact "(ab)*"        0 "b"
let%test _ = exact "(ab)*"        1 "ab"
let%test _ = exact "(ab)*"        0 "ba"
let%test _ = exact "(ab)*"        0 "aba"
let%test _ = exact "(ab)*"        1 "abab"
let%test _ = exact "(ab)*"        0 "ababa"
let%test _ = exact "(ab)*"        1 "ababab"

let%test _ = exact "\\."          0 ""
let%test _ = exact "\\."          0 "a"
let%test _ = exact "\\."          1 "."
let%test _ = exact "\\."          0 "*"
let%test _ = exact "\\."          0 ".."

let%test _ = exact "\\*"          0 ""
let%test _ = exact "\\*"          0 "a"
let%test _ = exact "\\*"          0 "."
let%test _ = exact "\\*"          1 "*"
let%test _ = exact "\\*"          0 "**"

let%test _ = exact "\\n"          0 ""
let%test _ = exact "\\n"          1 "\n"
let%test _ = exact "\\n"          0 "a"
let%test _ = exact "\\n"          0 "\\n"

let%test _ = exact "\n"           0 ""
let%test _ = exact "\n"           1 "\n"
let%test _ = exact "\n"           0 "a"
let%test _ = exact "\n"           0 "\\n"

let%test _ = exact "a{2}"         0 ""
let%test _ = exact "a{2}"         0 "a"
let%test _ = exact "a{2}"         1 "aa"
let%test _ = exact "a{2}"         0 "aaa"
let%test _ = exact "a{2}"         0 "aaaa"
let%test _ = exact "a{2}"         0 "aaaaa"

let%test _ = exact "a{2,}"        0 ""
let%test _ = exact "a{2,}"        0 "a"
let%test _ = exact "a{2,}"        1 "aa"
let%test _ = exact "a{2,}"        1 "aaa"
let%test _ = exact "a{2,}"        1 "aaaa"
let%test _ = exact "a{2,}"        1 "aaaaa"

let%test _ = exact "a{2,4}"       0 ""
let%test _ = exact "a{2,4}"       0 "a"
let%test _ = exact "a{2,4}"       1 "aa"
let%test _ = exact "a{2,4}"       1 "aaa"
let%test _ = exact "a{2,4}"       1 "aaaa"
let%test _ = exact "a{2,4}"       0 "aaaaa"


let grep_in_hello b pat = (matches_exn ~pat "hello" = not(b=0))

(* floating *)
let%test _ = grep_in_hello 1 "hello"
let%test _ = grep_in_hello 1 "h"
let%test _ = grep_in_hello 1 "he"
let%test _ = grep_in_hello 1 "el"
let%test _ = grep_in_hello 1 "lo"
let%test _ = grep_in_hello 1 "o"

let%test _ = grep_in_hello 0 "hellox"
let%test _ = grep_in_hello 0 "x"
let%test _ = grep_in_hello 0 "hex"
let%test _ = grep_in_hello 0 "lll"
let%test _ = grep_in_hello 0 "lox"
let%test _ = grep_in_hello 0 "xh"

(* start-anchored *)

let%test _ = grep_in_hello 1 "^hello"
let%test _ = grep_in_hello 1 "^h"
let%test _ = grep_in_hello 1 "^he"
let%test _ = grep_in_hello 0 "^el"
let%test _ = grep_in_hello 0 "^lo"
let%test _ = grep_in_hello 0 "^o"

let%test _ = grep_in_hello 0 "^hellox"
let%test _ = grep_in_hello 0 "^x"
let%test _ = grep_in_hello 0 "^hex"
let%test _ = grep_in_hello 0 "^lll"
let%test _ = grep_in_hello 0 "^lox"
let%test _ = grep_in_hello 0 "^xh"

(* end-anchored *)

let%test _ = grep_in_hello 1 "hello$"
let%test _ = grep_in_hello 0 "h$"
let%test _ = grep_in_hello 0 "he$"
let%test _ = grep_in_hello 0 "el$"
let%test _ = grep_in_hello 1 "lo$"
let%test _ = grep_in_hello 1 "o$"

let%test _ = grep_in_hello 0 "hellox$"
let%test _ = grep_in_hello 0 "x$"
let%test _ = grep_in_hello 0 "hex$"
let%test _ = grep_in_hello 0 "lll$"
let%test _ = grep_in_hello 0 "lox$"
let%test _ = grep_in_hello 0 "xh$"

(* start&end-anchored *)

let%test _ = grep_in_hello 1 "^hello$"
let%test _ = grep_in_hello 0 "^h$"
let%test _ = grep_in_hello 0 "^he$"
let%test _ = grep_in_hello 0 "^el$"
let%test _ = grep_in_hello 0 "^lo$"
let%test _ = grep_in_hello 0 "^o$"

let%test _ = grep_in_hello 0 "^hellox$"
let%test _ = grep_in_hello 0 "^x$"
let%test _ = grep_in_hello 0 "^hex$"
let%test _ = grep_in_hello 0 "^lll$"
let%test _ = grep_in_hello 0 "^lox$"
let%test _ = grep_in_hello 0 "^xh$"


let%test _ = grep_in_hello 1 "h.l"
let%test _ = grep_in_hello 1 "h..l"
let%test _ = grep_in_hello 1 "h...o"

let%test _ = grep_in_hello 0 "h.e"
let%test _ = grep_in_hello 0 "h.o"


(* search strategy tests... *)

let sel inp pat strat = select_exn ~pat ~strat inp

let patF = "A.*Z|B.*X" (* floating pattern *)

let inp =                                     "--A--B--B--X--Z--X--"

let%test _ = sel inp patF `soonest_shortest  = Some         "B--X"
let%test _ = sel inp patF `soonest_leftmost  = Some      "B--B--X"
let%test _ = sel inp patF `rightmost_longest = Some      "B--B--X--Z--X"
let%test _ = sel inp patF `leftmost_longest  = Some   "A--B--B--X--Z"

let patA = "^.*[XZ]" (* start-anchored *)

let%test _ = sel inp patA `soonest_shortest  = Some "--A--B--B--X"
let%test _ = sel inp patA `soonest_leftmost  = Some "--A--B--B--X"
let%test _ = sel inp patA `rightmost_longest = Some "--A--B--B--X--Z--X"
let%test _ = sel inp patA `leftmost_longest  = Some "--A--B--B--X--Z--X"

let patE = "[AB].*$" (* end-anchored *)

let%test _ = sel inp patE `soonest_shortest  = Some         "B--X--Z--X--"
let%test _ = sel inp patE `soonest_leftmost  = Some   "A--B--B--X--Z--X--"
let%test _ = sel inp patE `rightmost_longest = Some   "A--B--B--X--Z--X--"
let%test _ = sel inp patE `leftmost_longest  = Some   "A--B--B--X--Z--X--"

let fooAE = "^foo$" (* anchored at start & end *)

let%test _ = sel "foo" fooAE `soonest_shortest  = Some "foo"
let%test _ = sel "foo" fooAE `soonest_leftmost  = Some "foo"
let%test _ = sel "foo" fooAE `rightmost_longest = Some "foo"
let%test _ = sel "foo" fooAE `leftmost_longest  = Some "foo"


let%test _ = search_exn ~pat:".*" ~strat:`soonest_shortest "hello" = Some (1,1)
let%test _ = search_exn ~pat:".*" ~strat:`soonest_leftmost "hello" = Some (1,1)
let%test _ = search_exn ~pat:".*" ~strat:`rightmost_longest "hello" = Some (0,5)
let%test _ = search_exn ~pat:".*" ~strat:`leftmost_longest "hello" = Some (0,5)

let%test _ = select_exn ~pat:".*" ~strat:`soonest_shortest "hello" = Some ""
let%test _ = select_exn ~pat:".*" ~strat:`soonest_leftmost "hello" = Some ""
let%test _ = select_exn ~pat:".*" ~strat:`rightmost_longest "hello" = Some "hello"
let%test _ = select_exn ~pat:".*" ~strat:`leftmost_longest "hello" = Some "hello"
