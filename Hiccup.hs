{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Hiccup (runTcl, runTclWithArgs, mkMainInterp, 
               interpEvalStr
--                , hiccupTests
               ) where

import Util
import TclLib.Interp
import TclLib.LibUtil
import qualified TclObj as T

import Internal.InterpSpec
import TclLib (libCmds, libInits)
-- import Test.HUnit
--

baseCmds = mergeCmdLists [interpCmds, libCmds]

processArgs al = ["argc" * T.fromInt (length al), "argv" * toTclList al]
  where (*) name val = (pack name, val)
        toTclList = T.fromList . map T.fromBStr

interpVars al = "tcl_version" * show hiccupVersion : processArgs al
  where (*) name val = (pack name, T.fromStr val)

hiccupVersion = 0.49

mkMainInterp args cmds = mkInterp spec
 where spec = emptyInterp {
                 ispecSafe = False,
                 ispecVars = interpVars args,
                 ispecCmds = mergeCmdLists [baseCmds, makeCmdList cmds],
                 ispecInits = libInits
              }

runTcl v = mkMainInterp [] [] >>= interpEvalStr v

runTclWithArgs args cmds v = mkMainInterp args cmds >>= interpEvalStr v


-- hiccupTests = TestList [
--     "stuff" ~: (runTcl "expr 3 + 4") `should_be` (Right "7")
--   ]
--   where should_be c v = c >>= \r -> assertEqual "" r v
