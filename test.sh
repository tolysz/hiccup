die() {
  echo $1
  exit 1
}

cabal exec -- ghc -e Tests.runUnit Tests.hs || die "build failure"
cabal exec -- runghc -W Main.hs atests.tcl

