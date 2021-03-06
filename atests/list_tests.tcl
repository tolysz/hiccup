
test "list test" {
  checkthat [list] eq {}

  set bean [list 1 2 3 4 5 {6 7 8}]

  checkthat [llength $bean] == 6

  checkthat [llength "peanut"] == 1
  checkthat [llength "peanut ontology"] == 2
  checkthat [llength ""] == 0

  checkthat [llength {one [puts bean]}] == 3

  checkthat [llength {a b # c d}] == 5

  checkthat [llength [list [list 1 2 3] [list 3 4 5]]] == 2

  set boo [list {} {} {} {}]
  checkthat [llength $boo] == 4
}

test "elts whitespace delimited" {
    assert_err { llength { "a""b""c""d" } }
    # " end quote for bad stuff
}

test "list eval" {
  checkthat [eval [list * 3 5]] == 15
}

test "lindex" {
  checkthat [lindex 4] == 4
  checkthat [lindex 4 {}] == 4

  set bean [list 1 2 3 4 5 {6 7 8}]

  checkthat [lindex $bean 3] == 4
  checkthat [lindex $bean 5] eq {6 7 8}
  checkthat [lindex $bean 8] eq "" 

  checkthat [lindex $bean -3] eq {} 

  checkthat [lindex $bean end] eq {6 7 8}

  assert_err { lindex {1 2 3 4} cat }
}

test "lindex multi" {
    checkthat [lindex {{a b} {c d}} 1 1] eq "d"
}

test "lindex w/ hex" {
    set lst [list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
    checkthat [lindex $lst 0xa] == 11
    checkthat [lindex $lst 0xB] == 12
}

test "lappend" {
  set x {}
  lappend x 1
  lappend x 2 3 "entropy kills"
  checkthat [llength $x] == 4
  checkthat $x eq "1 2 3 {entropy kills}"
}

test "lreverse" {
    checkthat [lreverse [list 1 2 3]] eq [list 3 2 1]
}

test "lset no index" {
  checkthat [not [info exists boo]]
  assertErr  { lset boo 5 }
  set boo 2
  lset boo 5
  checkthat $boo == 5
  lset boo {} 6
  checkthat $boo == 6
}

test "lset single index" {
  set lst [list 1 2 3]
  lset lst 0 ONE
  checkthat $lst eq {ONE 2 3}
  
  assertErr { lset lst 40 TOO_HIGH }
  assertErr { lset lst -3 TOO_LOW }

  set lst2 [list 1 2 3]
  lset lst2 end WIN
  checkthat $lst2 eq [list 1 2 WIN]
}

test "lassign basic" {
  lassign { 1 2 3 } a b c
  checkthat $a == 1
  checkthat $b == 2
  checkthat $c == 3
}

test "lassign return" {
  checkthat [lassign {0 1 2 3} x] eq {1 2 3}
  checkthat [lassign {0 1 2} a b c] eq {}
}

test "lassign length difference" {
  lassign { 0 1 } a b c d
  checkthat $a == 0
  checkthat $b == 1
  checkthat $c eq ""
  checkthat $d eq ""
}

test "lsort basic" { 
  checkthat [lsort {}] eq {}
  checkthat [lsort {a b c}] eq {a b c}
  checkthat [lsort {c b a}] eq {a b c}

  checkthat [lsort -ascii {c b a}] eq {a b c}
}

test "lsort bad arg" {
  assert_err { lsort -badarg {a b c} }
}

test "lsort decreasing" {
  checkthat [lsort -increasing {c b a}] eq {a b c}
  checkthat [lsort -decreasing {c b a}] eq {c b a}
  checkthat [lsort -decreasing {a b c}] eq {c b a}

  assert_err { lsort -decreasing batman [list 1 2 3] }
}

test "lsort nocase" {
  set lst { A b a c B C }

  checkthat [lsort $lst] eq {A B C a b c}
  checkthat [lsort -nocase $lst] eq {A a b B c C}
}

test "lsort integer" {
  checkthat [lsort -integer [list 1 2 3]] eq [list 1 2 3]
  checkthat [lsort -integer [list 3 2 1]] eq [list 1 2 3]
  checkthat [lsort -integer -decreasing [list 1 2 3]] eq [list 3 2 1]
}

test "join" { 
  checkthat [join {}] eq {}
  checkthat [join {1 2 3}] eq "1 2 3"
  checkthat [join {1 2 3} ","] eq "1,2,3"
  checkthat [join {1 2 3} " | "] eq "1 | 2 | 3"
  checkthat [join {1} " | "] eq "1"
  assertErr { join " \{ " }
}

test "concat" {
  checkthat [concat a b {c d e} {f {g h}}] eq "a b c d e f {g h}"
  checkthat [concat "a   b   c" { d e f }] eq "a   b   c d e f"
  checkthat [concat "a b {c " d " e} f"] eq "a b {c d e} f"
}

test "lsearch" {
  set items [list A B C D E]
  checkthat [lsearch $items X] == -1
  checkthat [lsearch $items C] == 2

  checkthat [lsearch [list AAA BBB CCC] A??] == 0
}

test "lsearch -exact/-glob" {
  checkthat [lsearch -exact [list AAA BBB CCC] A??] == -1
  checkthat [lsearch -exact [list AAA BBB CCC] AAA] == 0
  checkthat [lsearch -glob [list AAA BBB CCC] C??] == 2
}

test "lsearch -start" {
    checkthat [lsearch -start 3 [list X Y Y Y X] X] == 4
    checkthat [lsearch -start 1000 [list X Y Y Y X] X] == -1
}

test "lrepeat" {
  checkthat [lrepeat 1 x] eq [list x]
  checkthat [lrepeat 3 x] eq [list x x x]
  checkthat [lrepeat 3 a b c] eq [list a b c a b c a b c]

  assert_err { lrepeat 0 x }
  assert_err { lrepeat -4 x }
  assert_err { lrepeat 1 }

  checkthat [llength [lrepeat 10 [lrepeat 10 a b]]] == 10
}

test "lrange" {
    checkthat [lrange {1 2 3} 0 0] == 1
    checkthat [lrange {1 2 3} 1 2] == [list 2 3]

    checkthat [lrange {1 5} e e] == 5

    assert_err { lrange {1 2} 1 "" }
    assert_err { lrange {1 2} 1 ends }
}

test "lmap" {
    set start [list 1 2 3 4]
    set plus1 {x {incr x}}
    checkthat [lmap $plus1 $start] eq [list 2 3 4 5]
    checkthat [lmap $plus1 [list]] eq {}
}

