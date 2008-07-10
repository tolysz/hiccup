
test "multi-statement subexpression" {
    set x "-[set y 4;incr y]-"
    checkthat $x eq {-5-}

    set v "[set v 0;incr v;incr v; incr v ;incr v]"
    checkthat $v == 4
}

test "apply" {
  set plus1 { x {+ $x 1}}
  checkthat [apply $plus1 3] == 4

  checkthat [apply {v {expr $v}} {3*3}] == 9

  checkthat [apply {{a b} { + $a $b}} 2 3] == 5
  assert_err { apply {{a b} { + $a $b}} 2 }
}

test "return code" {
    proc retthis args {
        return {*}$args
    }
    checkthat [retthis -errorcode] eq -errorcode
    checkthat [retthis -code] eq -code

    checkthat [retthis -code ok] eq {}

    assert_err { return -code fishbulb }

    checkthat [catch { return pants } msg1] == 2
    checkthat $msg1 eq pants

    checkthat [catch { set x 4 } msg2] == 0
    checkthat $msg2 eq 4

    checkthat [catch {return -code error EEP} msg3] == 2 { catch return -code error == 2 }
    checkthat $msg3 eq EEP

    finalize { proc retthis }
}

test "return break" {
    proc retbreak {} { return -code break }

    set count 0
    for { set x 0 } { $x < 5 } { incr x } {
        incr count
        retbreak
    }

    checkthat $count == 1
    finalize { proc retbreak }
}

test "return continue" {
    proc retcont {} { return -code continue }
    set res OK
    for { set x 0 } { $x < 5 } { incr x } {
        retcont
        set res FAIL
    }

    checkthat $res eq OK

    finalize { proc retcont }
}

test "return return" {
    proc retret {} { return -code return }
    proc thinger {} {
        uplevel  { set res OK }
        retret
        uplevel { set res FAIL }
    }
    thinger

    checkthat $res eq OK
    finalize { proc retret proc thinger }
}

test "changed proc" {
    namespace eval temp {
        variable count 0
    }

    proc inner {} {
        variable ::temp::count
        set count 0
    }
    proc outer {} {
        inner 
        inner
    }
    outer
    checkthat $::temp::count == 0

    proc inner {} {
        variable ::temp::count
        set count 5
    }

    outer
    checkthat $::temp::count == 5

    finalize { proc inner proc outer ns temp } 
}

test "proc diff namespace" {
    namespace eval temp {
        proc fancy {} { return NS }
    }
    proc fancy {} { return GL }
    proc fancier {} { return [fancy] }

    checkthat [fancier] eq GL
    checkthat [namespace eval temp { fancier }] eq GL

    proc temp::fancier {} { return [fancy] }
    checkthat [namespace eval temp { fancier }] eq NS

    finalize { proc fancy proc fancier ns temp }
}

test "expand" {
  checkthat [+ {*}[list 1 1]] == 2
  checkthat [+ {*}5 {*}5] == 10
  set boo [list 3 3]
  checkthat [list {*}$boo] eq $boo

  checkthat [concat {*}$boo] eq "3 3"
}

test "info vars" {
  checkthat [llength [info vars]] == 0
  set x 4
  checkthat [info vars] eq "x"
  checkthat [info vars y] eq {}
  checkthat [info vars ?] eq "x"
}

test "info exists" {
  checkthat [info exists x] == 0
  set x 4
  checkthat [info exists x] == 1
  checkthat [info exists current_test] == 0
  variable  testlib::current_test
  checkthat [info exists current_test] == 1

  set arr(4) 2
  checkthat [info exists arr(3)] == 0
  checkthat [info exists arr(4)] == 1
}

test "info level" {
  checkthat [uplevel {info level}] == 0
  checkthat [info level] == 1
  proc getlevel {} {
    return [info level]
  }

  checkthat [getlevel] == 2
  finalize { proc getlevel }
}

test "absolute uplevel" {
  assert_err { uplevel banana { puts FAIL } }
  proc 33xy x { return $x }
  proc nop x { return $x }
  assert_err { uplevel 33xy 4 }
  checkthat [uplevel nop 4] == 4
  uplevel #0 { checkthat [info level] == 0 }
  finalize { proc 33xy proc nop }
}

test "info commands vs info procs" {
  proc this_is_a_proc {} {}
  checkthat [lsearch [info commands] this_is_a_proc] >= 0
  checkthat [lsearch [info commands] set] >= 0
  checkthat [lsearch [info procs] this_is_a_proc] >= 0
  checkthat [lsearch [info procs] set] == -1

  finalize { proc this_is_a_proc }
}

test "info args" {
    assert_err { info args puts }
    assert_err { info args }
    proc blah {x y {z 3}} {}
    checkthat [info args blah] eq [list x y z]

    proc argsproc { one two args } { }
    checkthat [info args argsproc] eq [list one two args]

    finalize { proc blah proc argsproc }
}

test "format" {
    checkthat [format "hi %s" billy] eq "hi billy"
    checkthat [format "hi %%"] eq "hi %"
    checkthat [format "letter %c!" 65] eq "letter A!"

    assert_err { format "Oh %c" candy }
    assert_err { format "hi %s" }
    assert_err { format " % " }

    checkthat [format "%d %d %d" 1 2 3] eq "1 2 3"

    assert_err { format "%d" pants }
}

test "subst" {
    checkthat [subst {A cat}] eq "A cat"
    set cat 44
    checkthat [subst {A $cat}] eq "A $cat"
    checkthat [subst {one \n two}] eq "one \n two"
    checkthat [subst -nobackslashes {one \n two}] eq {one \n two}

    set boo(\n) 4
    checkthat [subst { $boo(\n) }] eq " 4 "
    checkthat [subst -nobackslashes { $boo(\n) }] eq " 4 "

    set fish WOO
    checkthat [subst -nobackslashes { \$fish }] eq { \WOO }
}