{-# LANGUAGE BangPatterns
           , OverloadedStrings
  #-}

module Expr.Compile
  ( compileExpr
  , getUnFun
  , getOpFun
  ) where

import qualified MathOp as Math
import qualified TObj as T
import Expr.TExp

compileExpr fwr sc = comp
 where comp e = case e of
                Item (AStr s) -> CStrTok (sc s)
                Item v        -> CItem v
                DepItem d     -> DItem (updep d)
                UnApp op v    -> CApp op (comp v)
                BinApp op a b -> CApp2 op (comp a) (comp b)
                TernIf a b c  -> CTern (comp a) (comp b) (comp c)
                Paren e       -> comp e
       updep d = case d of
                 DFun f ex -> DFun f (map comp ex)
                 DCom cmd  -> DCom (fwr cmd)
                 DVar vn   -> DVar vn

getUnFun op = case op of
  OpNot -> Math.opNot
  OpNeg -> Math.opNegate

getOpFun !op = case op of
    OpLt -> up Math.lessThan
    OpPlus -> Math.plus
    OpTimes -> Math.times
    OpMinus -> Math.minus
    OpDiv -> Math.divide 
    OpExp -> Math.pow
    OpEql -> up Math.equals
    OpNeql -> up Math.notEquals
    OpGt -> up Math.greaterThan
    OpLte -> up Math.lessThanEq
    OpGte -> up Math.greaterThanEq
    OpStrEq -> sup T.strEq
    OpStrNe -> sup T.strNe
    OpAnd -> cmdBool (&&)
    OpOr -> cmdBool (||)
    OpLShift -> Math.leftShift
    OpRShift -> Math.rightShift
    OpIn     -> Math.opIn
 where up f a b = return (f a b)
       {-# INLINE up #-}
       sup f a b = return (T.fromBool (f a b))
{-# INLINE getOpFun #-}

cmdBool f a b = do 
   ab <- T.asBool a
   bb <- T.asBool b
   return $! T.fromBool (ab `f` bb)
{-# INLINE cmdBool #-}
