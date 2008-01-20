module ListProcs (listProcs,procList) where
import Common
import qualified TclObj as T
import qualified Data.ByteString.Char8 as B
import Control.Monad
import qualified Data.Sequence as S
import Data.Sequence ((><))

listProcs = makeProcMap $
  [("list", procList),("lindex",procLindex),
   ("llength",procLlength), ("lappend", procLappend), ("lset", procLset)]

procList, procLindex, procLlength :: TclProc
procList = return . T.mkTclList

procLindex args = case args of
          [l]   -> return l
          [l,i] -> do items <- T.asSeq l
                      ind   <- T.asInt i
                      if ind >= S.length items then ret else return (S.index items ind)
          _     -> argErr "lindex"

procLlength args = case args of
        [lst] -> T.asList lst >>= return . T.mkTclInt . length
        _     -> argErr "llength"

procLset args = case args of
        [name,ind,val] -> do old <- varGetRaw (T.asBStr name)
                             items <- T.asSeq old
                             i <- T.asInt ind
                             let nl = T.mkTclList' (S.update i val items)
                             varSet (T.asBStr name) nl
        _              -> argErr "lset"
                              


procLappend args = case args of
        (n:news) -> varModify (T.asBStr n)  $
                \old -> do items <- T.asSeq old
                           return $ T.mkTclList' (items >< (S.fromList news))
        _        -> argErr "lappend"

