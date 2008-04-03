module Expr.TExp where

import qualified TclObj as T
import Util

data Op = OpDiv | OpPlus | OpMinus | OpTimes | OpEql | OpNeql |
          OpLt | OpGt | OpLte | OpGte | OpStrNe | OpStrEq | OpAnd |
          OpOr
  deriving (Show,Eq)

data TExp = TOp !Op TExp TExp | TNot TExp | TVar String 
            | TFun String [TExp] | TVal T.TclObj 
   deriving (Show,Eq)