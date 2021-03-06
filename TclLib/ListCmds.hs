module TclLib.ListCmds (listCmds) where
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence ((><))

import qualified TclObj as T
import Common
import ArgParse
import Util
import Match (MatchType(..),matchFun)
import Proc.Util (mkLambda)
import TclLib.LibUtil

listCmds = makeCmdList $
  [("list", cmdList),("lindex",cmdLindex),
   ("llength",cmdLlength), ("lappend", cmdLappend), ("lsearch", cmdLsearch),
   ("lset", cmdLset), ("lassign", cmdLassign), ("lsort", cmdLsort),
   ("lrange", cmdLrange), ("lmap", cmdLmap), ("lreverse", cmdLreverse),
   ("join", cmdJoin), ("concat", cmdConcat), ("lrepeat", cmdLrepeat)]

cmdList = return . T.fromList

cmdLindex args = case args of
          [l,i] -> T.asList i >>= worker l 
          (l:inds) -> worker l inds
          _     -> vArgErr "lindex list ?index...?"
    where worker lst inds = case inds of
             [] -> return lst
             (i:ix) -> do
                 items <- T.asSeq lst
                 let ilen = S.length items                            
                 ind <- toIndex ilen i
                 if ind >= ilen || ind < 0 then ret 
                                           else worker (S.index items ind) ix

cmdLlength args = case args of
        [lst] -> T.asSeq lst >>= return . T.fromInt . S.length
        _     -> vArgErr "llength list"

cmdLset args = case args of
        [name,val] -> modifyVar (T.asVarName name) (\_ -> return val)
        [name,ind,val] -> modifyVar (T.asVarName name) $
                           \old -> do
                               items <- T.asSeq old
                               if T.isEmpty ind 
                                  then return $! val
                                  else do
                                      i <- toIndex (S.length items) ind
                                      rangeCheck items i
                                      return $! T.fromSeq (S.update i val items)
        _              -> argErr "lset"
 where rangeCheck seq i = when (i < 0 || i >= (S.length seq)) $ fail "list index out of range" 
       modifyVar vn f = do
            o <- varGetNS vn
            n <- f o
            varSetNS vn $! n
                              
cmdLassign args = case args of
  (list:(varnames@(_:_))) -> do l <- T.asList list
                                let (src,rest) = splitAt (length varnames) l
                                zipWithM_ setter varnames (src ++ repeat T.empty)
                                return (T.fromList rest)
  _ -> argErr "lassign"
 where setter n v = varSetNS (T.asVarName n) v

cmdJoin args = case args of
   [lst]     -> dojoin lst (pack " ")
   [lst,sep] -> dojoin lst (T.asBStr sep)
   _         -> vArgErr "join list ?joinString?"
 where dojoin ll sep = do
         lst <- T.asList ll
         return $ T.fromBStr (joinWithBS (map T.asBStr lst) sep)

cmdConcat = return . T.objconcat

cmdLreverse args = case args of
   [lst] -> T.asSeq lst >>= return . T.fromSeq . S.reverse  
   _     -> vArgErr "lreverse list"

cmdLappend args = case args of
        (n:news) -> do 
             let vn = T.asVarName n 
             items <- varGetNS vn >>= T.asSeq 
             varSetNS vn $ T.fromSeq (items >< (S.fromList news))
        _        -> vArgErr "lappend varName ?value value ...?"

searchArgs = mkArgSpecs 2 [
     NoArg "exact" (setMatchType ExactMatch),
     NoArg "glob" (setMatchType GlobMatch),
     OneArg "start" setStart
   ]
 where setMatchType mt (s,_) = (s,mt)
       setStart s (_,mt)     = (s,mt)

cmdLsearch args_ = do
     ((start,mtype),args) <- parseArgs searchArgs (T.fromInt 0, GlobMatch) args_
     case args of
       [lsto,pat] -> do 
              let p = T.asBStr pat
              lst <- T.asList lsto
              sind <- toIndex (length lst) start
              let ilst = drop sind $ zip [0..] lst
              return . T.fromInt $ findIt (matchFun mtype) p ilst
       _         -> vArgErr "lsearch ?options? list pattern"
 where findIt _ _ []         = -1
       findIt f p ((i,e):xs) = if f p (T.asBStr e) then i else findIt f p xs

data SortType = AsciiSort | IntSort deriving (Eq,Show)
data SortFlags = SF { sortType :: SortType, sortReverse :: Bool, noCase :: Bool } deriving (Eq, Show)

sortArgs = mkArgSpecs 1 [
       NoArg "ascii" (setSortType AsciiSort), 
       NoArg "integer" (setSortType IntSort), 
       NoArg "increasing" (setSortReverse False), 
       NoArg "decreasing" (setSortReverse True), 
       NoArg "nocase" (\x -> x { noCase = True })
      ]
 where setSortType v = (\x -> x { sortType = v })
       setSortReverse v = (\x -> x { sortReverse = v })
                        
defaultSort = SF { sortType = AsciiSort, sortReverse = False, noCase = False }

cmdLsort args_ = do 
    (sf,args) <- parseArgs sortArgs defaultSort args_
    case args of
       [lst] -> dosort sf lst
       _     -> argErr "lsort"
 where dosort sf lst = do
              items <- T.asList lst 
              sortEm sf items >>= return . T.fromList


-- TODO: This is so ugly.
sortEm :: SortFlags -> [T.TclObj] -> TclM [T.TclObj]
sortEm (SF stype rev nocase) lst = do 
     pairs <- modder
     return $ post pairs
  where paired f = mapM (\x -> f x >>= \nx -> return (nx, x)) lst
        caser = downCase `ifTrue` nocase
        retSortFst :: (Ord a) => [(a,T.TclObj)] -> TclM [T.TclObj]
        retSortFst = return . map snd . sortBy (comparing fst)
        post = reverse `ifTrue` rev
        modder = case stype of
                  AsciiSort -> paired (return . caser . T.asBStr) >>= retSortFst
                  IntSort   -> paired (T.asInt) >>= retSortFst
        ifTrue fun b = if b then fun else id

cmdLrepeat args = case args of 
    (ti:x:xs) -> do 
          i <- T.asInt ti 
          when (i <= 0) $ fail "must have a count of at least 1"
          return $ T.fromList (concat (replicate i (x:xs)))
    _ -> vArgErr "lrepeat positiveCount value ?value ...?"

cmdLrange args = case args of
   [lst,beg,e] -> do 
          items <- T.asSeq lst
          let ilen = S.length items
          i1 <- toIndex ilen beg
          i2 <- toIndex ilen e
          return $ T.fromSeq (S.take (i2 - i1 + 1) (S.drop i1 items))
   _           -> vArgErr "lrange list first last"

cmdLmap args = case args of
  [fun,lst] -> do
         fn <- mkLambda fun 
         T.asList lst >>= mapM (fn . box) >>= return . T.fromList 
  _          -> vArgErr "lmap lambda list"
 where box i = [i]   
