
open Red

(* test for position of parse errors / absence of parse errors *)

let err pos pat =
  let module Red = F(struct end) in
  match (Red.parse ~pat) with `Error (_,i) -> (pos=i) | `Ok _ -> (pos=0)

TEST = err 0 ""

TEST = err 0 "a"
TEST = err 0 "."
TEST = err 0 "\n"
TEST = err 0 "\t"
TEST = err 0 "-"

TEST = err 0 "^"
TEST = err 0 "$"
TEST = err 0 "^$"
TEST = err 0 ".^"
TEST = err 0 "$."

TEST = err 2 "\\" (* unterminated escape *)
TEST = err 0 "\\\\"
TEST = err 0 "\\("
TEST = err 0 "\\)"
TEST = err 0 "\\["
TEST = err 0 "\\]"
TEST = err 0 "\\&"
TEST = err 0 "\\|"
TEST = err 0 "\\!"
TEST = err 0 "\\*"
TEST = err 0 "\\+"
TEST = err 0 "\\?"
TEST = err 0 "\\{"
TEST = err 0 "\\}"
TEST = err 0 "\\."
TEST = err 0 "\\-"
TEST = err 0 "\\^"
TEST = err 0 "\\$"
TEST = err 0 "\\n"
TEST = err 0 "\\t"
TEST = err 0 "\\$."
TEST = err 2 "\\a" (* unknown escape *)
TEST = err 2 "\\b" (* unknown escape *)

TEST = err 0 "[]"
TEST = err 0 "[^]"
TEST = err 0 "[^^]"
TEST = err 0 "[[^]"
TEST = err 0 "[-]"
TEST = err 0 "[-a]"
TEST = err 0 "[a[]"
TEST = err 0 "[a\\]]"
TEST = err 0 "[a\\-]"
TEST = err 0 "[a\\[]"
TEST = err 2 "["
TEST = err 1 "]"
TEST = err 3 "[a"
TEST = err 4 "[a-"
TEST = err 4 "[a-]"
TEST = err 5 "[a-b"
TEST = err 4 "[a]]"

TEST = err 0 "a{3}"
TEST = err 0 "a{3,}"
TEST = err 0 "a{3,7}"
TEST = err 0 "a{03}"
TEST = err 0 "a{13,}"
TEST = err 0 "a{13,1009}"

TEST = err 1 "{3}"
TEST = err 3 "(|{3})"
TEST = err 3 "a{"
TEST = err 3 "a{}"
TEST = err 3 "a{a"
TEST = err 4 "a{3"
TEST = err 4 "a{3a"
TEST = err 5 "a{3,"
TEST = err 5 "a{3,a"
TEST = err 6 "a{3,7"
TEST = err 6 "a{3,7,"
TEST = err 6 "a{3,2}"
TEST = err 1 "}"

TEST = err 0 "()"
TEST = err 0 "(())"
TEST = err 0 "(()())"
TEST = err 2 "("
TEST = err 4 "(()"
TEST = err 3 "())"
TEST = err 4 "[]("
TEST = err 3 "[])"

TEST = err 0 "&"
TEST = err 0 "!"
TEST = err 0 "|"
TEST = err 0 "&&"
TEST = err 0 "||"
TEST = err 0 "!!"

TEST = err 0 "a*"
TEST = err 0 "()*"
TEST = err 0 "[]*"

TEST = err 1 "*"
TEST = err 1 "+"
TEST = err 1 "?"
TEST = err 2 "(*)"
TEST = err 2 "|*"
TEST = err 2 "^*"


(* test for basic forms - use exact matching  *)

let exact pat b inp  = matches_exn ~pat:("^" ^ pat ^ "$") inp = not(b=0)

TEST = exact ""             1 ""
TEST = exact ""             0 "a"

TEST = exact "a"            0 ""
TEST = exact "a"            1 "a"
TEST = exact "a"            0 "b"
TEST = exact "a"            0 "aa"

TEST = exact "."            0 ""
TEST = exact "."            1 "a"
TEST = exact "."            1 "b"
TEST = exact "."            1 "."
TEST = exact "."            0 "ab"

TEST = exact "!a"           1 ""
TEST = exact "!a"           0 "a"
TEST = exact "!a"           1 "b"
TEST = exact "!a"           1 "aa"

TEST = exact "a?"           1 ""
TEST = exact "a?"           1 "a"
TEST = exact "a?"           0 "b"
TEST = exact "a?"           0 "aa"

TEST = exact "a*"           1 ""
TEST = exact "a*"           1 "a"
TEST = exact "a*"           1 "aa"
TEST = exact "a*"           1 "aaa"
TEST = exact "a*"           0 "aba"

TEST = exact "a+"           0 ""
TEST = exact "a+"           1 "a"
TEST = exact "a+"           1 "aa"
TEST = exact "a+"           1 "aaa"
TEST = exact "a+"           0 "aba"

TEST = exact "[ab]"         0 ""
TEST = exact "[ab]"         1 "a"
TEST = exact "[ab]"         1 "b"
TEST = exact "[ab]"         0 "c"
TEST = exact "[ab]"         0 "ab"

TEST = exact "[^ab]"        0 ""
TEST = exact "[^ab]"        0 "a"
TEST = exact "[^ab]"        0 "b"
TEST = exact "[^ab]"        1 "c"
TEST = exact "[^ab]"        0 "cc"

TEST = exact "[a-cx-y]"     0 ""
TEST = exact "[a-cx-y]"     1 "a"
TEST = exact "[a-cx-y]"     1 "b"
TEST = exact "[a-cx-y]"     1 "c"
TEST = exact "[a-cx-y]"     0 "d"
TEST = exact "[a-cx-y]"     0 "w"
TEST = exact "[a-cx-y]"     1 "x"
TEST = exact "[a-cx-y]"     1 "y"
TEST = exact "[a-cx-y]"     0 "z"
TEST = exact "[a-cx-y]"     0 "ab"

TEST = exact "[a-a]"        0 ""
TEST = exact "[a-a]"        1 "a"
TEST = exact "[a-a]"        0 "b"
TEST = exact "[a-a]"        0 "aa"

TEST = exact "[b-a]"        0 ""
TEST = exact "[b-a]"        0 "a"
TEST = exact "[b-a]"        0 "b"
TEST = exact "[b-a]"        0 "ab"

TEST = exact "ab"           0 ""
TEST = exact "ab"           0 "a"
TEST = exact "ab"           0 "b"
TEST = exact "ab"           1 "ab"
TEST = exact "ab"           0 "ba"
TEST = exact "ab"           0 "aba"

TEST = exact "a|b"          0 ""
TEST = exact "a|b"          1 "a"
TEST = exact "a|b"          1 "b"
TEST = exact "a|b"          0 "c"
TEST = exact "a|b"          0 "ab"

TEST = exact "a&b"          0 ""
TEST = exact "a&b"          0 "a"
TEST = exact "a&b"          0 "b"
TEST = exact "a&b"          0 "c"
TEST = exact "a&b"          0 "ab"

TEST = exact "[ab]&[bc]"    0 ""
TEST = exact "[ab]&[bc]"    0 "a"
TEST = exact "[ab]&[bc]"    1 "b"
TEST = exact "[ab]&[bc]"    0 "c"
TEST = exact "[ab]&[bc]"    0 "bb"

TEST = exact "a?b?"         1 ""
TEST = exact "a?b?"         1 "a"
TEST = exact "a?b?"         1 "b"
TEST = exact "a?b?"         0 "c"
TEST = exact "a?b?"         0 "aa"
TEST = exact "a?b?"         1 "ab"
TEST = exact "a?b?"         0 "ba"
TEST = exact "a?b?"         0 "abb"

TEST = exact "a?a?"         1 ""
TEST = exact "a?a?"         1 "a"
TEST = exact "a?a?"         1 "aa"
TEST = exact "a?a?"         0 "aaa"
TEST = exact "a?a?"         0 "b"
TEST = exact "a?a?"         0 "ab"
TEST = exact "a?a?"         0 "aba"

TEST = exact "[ab]c"        0 ""
TEST = exact "[ab]c"        0 "a"
TEST = exact "[ab]c"        0 "b"
TEST = exact "[ab]c"        0 "c"
TEST = exact "[ab]c"        0 "aa"
TEST = exact "[ab]c"        0 "ab"
TEST = exact "[ab]c"        1 "ac"
TEST = exact "[ab]c"        0 "ba"
TEST = exact "[ab]c"        0 "bb"
TEST = exact "[ab]c"        1 "bc"
TEST = exact "[ab]c"        0 "ca"
TEST = exact "[ab]c"        0 "cb"
TEST = exact "[ab]c"        0 "cc"
TEST = exact "[ab]c"        0 "abc"

TEST = exact "(ab)?"        1 ""
TEST = exact "(ab)?"        0 "a"
TEST = exact "(ab)?"        0 "b"
TEST = exact "(ab)?"        1 "ab"
TEST = exact "(ab)?"        0 "ba"
TEST = exact "(ab)?"        0 "aba"

TEST = exact "(ab)*"        1 ""
TEST = exact "(ab)*"        0 "a"
TEST = exact "(ab)*"        0 "b"
TEST = exact "(ab)*"        1 "ab"
TEST = exact "(ab)*"        0 "ba"
TEST = exact "(ab)*"        0 "aba"
TEST = exact "(ab)*"        1 "abab"
TEST = exact "(ab)*"        0 "ababa"
TEST = exact "(ab)*"        1 "ababab"

TEST = exact "\\."          0 ""
TEST = exact "\\."          0 "a"
TEST = exact "\\."          1 "."
TEST = exact "\\."          0 "*"
TEST = exact "\\."          0 ".."

TEST = exact "\\*"          0 ""
TEST = exact "\\*"          0 "a"
TEST = exact "\\*"          0 "."
TEST = exact "\\*"          1 "*"
TEST = exact "\\*"          0 "**"

TEST = exact "\\n"          0 ""
TEST = exact "\\n"          1 "\n"
TEST = exact "\\n"          0 "a"
TEST = exact "\\n"          0 "\\n"

TEST = exact "\n"           0 ""
TEST = exact "\n"           1 "\n"
TEST = exact "\n"           0 "a"
TEST = exact "\n"           0 "\\n"

TEST = exact "a{2}"         0 ""
TEST = exact "a{2}"         0 "a"
TEST = exact "a{2}"         1 "aa"
TEST = exact "a{2}"         0 "aaa"
TEST = exact "a{2}"         0 "aaaa"
TEST = exact "a{2}"         0 "aaaaa"

TEST = exact "a{2,}"        0 ""
TEST = exact "a{2,}"        0 "a"
TEST = exact "a{2,}"        1 "aa"
TEST = exact "a{2,}"        1 "aaa"
TEST = exact "a{2,}"        1 "aaaa"
TEST = exact "a{2,}"        1 "aaaaa"

TEST = exact "a{2,4}"       0 ""
TEST = exact "a{2,4}"       0 "a"
TEST = exact "a{2,4}"       1 "aa"
TEST = exact "a{2,4}"       1 "aaa"
TEST = exact "a{2,4}"       1 "aaaa"
TEST = exact "a{2,4}"       0 "aaaaa"


let grep_in_hello b pat = (matches_exn ~pat "hello" = not(b=0))

(* floating *)
TEST = grep_in_hello 1 "hello"
TEST = grep_in_hello 1 "h"
TEST = grep_in_hello 1 "he"
TEST = grep_in_hello 1 "el"
TEST = grep_in_hello 1 "lo"
TEST = grep_in_hello 1 "o"

TEST = grep_in_hello 0 "hellox"
TEST = grep_in_hello 0 "x"
TEST = grep_in_hello 0 "hex"
TEST = grep_in_hello 0 "lll"
TEST = grep_in_hello 0 "lox"
TEST = grep_in_hello 0 "xh"

(* start-anchored *)

TEST = grep_in_hello 1 "^hello"
TEST = grep_in_hello 1 "^h"
TEST = grep_in_hello 1 "^he"
TEST = grep_in_hello 0 "^el"
TEST = grep_in_hello 0 "^lo"
TEST = grep_in_hello 0 "^o"

TEST = grep_in_hello 0 "^hellox"
TEST = grep_in_hello 0 "^x"
TEST = grep_in_hello 0 "^hex"
TEST = grep_in_hello 0 "^lll"
TEST = grep_in_hello 0 "^lox"
TEST = grep_in_hello 0 "^xh"

(* end-anchored *)

TEST = grep_in_hello 1 "hello$"
TEST = grep_in_hello 0 "h$"
TEST = grep_in_hello 0 "he$"
TEST = grep_in_hello 0 "el$"
TEST = grep_in_hello 1 "lo$"
TEST = grep_in_hello 1 "o$"

TEST = grep_in_hello 0 "hellox$"
TEST = grep_in_hello 0 "x$"
TEST = grep_in_hello 0 "hex$"
TEST = grep_in_hello 0 "lll$"
TEST = grep_in_hello 0 "lox$"
TEST = grep_in_hello 0 "xh$"

(* start&end-anchored *)

TEST = grep_in_hello 1 "^hello$"
TEST = grep_in_hello 0 "^h$"
TEST = grep_in_hello 0 "^he$"
TEST = grep_in_hello 0 "^el$"
TEST = grep_in_hello 0 "^lo$"
TEST = grep_in_hello 0 "^o$"

TEST = grep_in_hello 0 "^hellox$"
TEST = grep_in_hello 0 "^x$"
TEST = grep_in_hello 0 "^hex$"
TEST = grep_in_hello 0 "^lll$"
TEST = grep_in_hello 0 "^lox$"
TEST = grep_in_hello 0 "^xh$"


TEST = grep_in_hello 1 "h.l"
TEST = grep_in_hello 1 "h..l"
TEST = grep_in_hello 1 "h...o"

TEST = grep_in_hello 0 "h.e"
TEST = grep_in_hello 0 "h.o"


(* search strategy tests... *)

let sel inp pat strat = select_exn ~pat ~strat inp

let patF = "A.*Z|B.*X" (* floating pattern *)

let inp =                                     "--A--B--B--X--Z--X--"

TEST = sel inp patF `soonest_shortest  = Some         "B--X"
TEST = sel inp patF `soonest_leftmost  = Some      "B--B--X"
TEST = sel inp patF `rightmost_longest = Some      "B--B--X--Z--X"
TEST = sel inp patF `leftmost_longest  = Some   "A--B--B--X--Z"

let patA = "^.*[XZ]" (* start-anchored *)

TEST = sel inp patA `soonest_shortest  = Some "--A--B--B--X"
TEST = sel inp patA `soonest_leftmost  = Some "--A--B--B--X"
TEST = sel inp patA `rightmost_longest = Some "--A--B--B--X--Z--X"
TEST = sel inp patA `leftmost_longest  = Some "--A--B--B--X--Z--X"

let patE = "[AB].*$" (* end-anchored *)

TEST = sel inp patE `soonest_shortest  = Some         "B--X--Z--X--"
TEST = sel inp patE `soonest_leftmost  = Some   "A--B--B--X--Z--X--"
TEST = sel inp patE `rightmost_longest = Some   "A--B--B--X--Z--X--"
TEST = sel inp patE `leftmost_longest  = Some   "A--B--B--X--Z--X--"

let fooAE = "^foo$" (* anchored at start & end *)

TEST = sel "foo" fooAE `soonest_shortest  = Some "foo"
TEST = sel "foo" fooAE `soonest_leftmost  = Some "foo"
TEST = sel "foo" fooAE `rightmost_longest = Some "foo"
TEST = sel "foo" fooAE `leftmost_longest  = Some "foo"


TEST = search_exn ~pat:".*" ~strat:`soonest_shortest "hello" = Some (1,1)
TEST = search_exn ~pat:".*" ~strat:`soonest_leftmost "hello" = Some (1,1)
TEST = search_exn ~pat:".*" ~strat:`rightmost_longest "hello" = Some (0,5)
TEST = search_exn ~pat:".*" ~strat:`leftmost_longest "hello" = Some (0,5)

TEST = select_exn ~pat:".*" ~strat:`soonest_shortest "hello" = Some ""
TEST = select_exn ~pat:".*" ~strat:`soonest_leftmost "hello" = Some ""
TEST = select_exn ~pat:".*" ~strat:`rightmost_longest "hello" = Some "hello"
TEST = select_exn ~pat:".*" ~strat:`leftmost_longest "hello" = Some "hello"
