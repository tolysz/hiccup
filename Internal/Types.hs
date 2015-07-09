{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Internal.Types where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.IORef
import qualified TclObj as T
import qualified EventMgr as Evt

import Util
import TclErr
import TclChan

class Runnable t where
  evalTcl :: t -> TclM T.TclObj

newtype TclM a = TclM { unTclM :: ExceptT Err (StateT TclState IO) a }
 deriving (MonadState TclState, MonadIO, Applicative, Functor, Monad, MonadError Err)

data Namespace = TclNS {
         nsName :: BString,
         nsCmds :: !CmdMap,
         nsFrame :: !FrameRef,
         nsExport :: [BString],
         nsParent :: Maybe NSRef,
         nsChildren :: Map.Map BString NSRef,
         nsPath :: [NSRef],
         nsPathLinks :: [NSRef],
         nsUnknown :: Maybe BString
         } 


type FrameRef = IORef TclFrame
type NSRef = IORef Namespace
type UpMap = Map.Map BString (FrameRef,BString)

data TclFrame = TclFrame { 
      frVars :: !VarMap, 
      upMap :: !UpMap,
      frNS :: NSRef,
      frTag :: !Int,
      frInfo :: [T.TclObj] }

type TclStack = [FrameRef]

data Interp = Interp { interpState :: IORef TclState }
type InterpMap = Map.Map BString Interp

data TclState = TclState { 
    interpSafe :: Bool,
    tclChans :: ChanMap, 
    tclInterps :: InterpMap,
    tclEvents :: Evt.EventMgr T.TclObj,
    tclStack :: !TclStack, 
    tclHidden :: CmdMap,
    tclGlobalNS :: !NSRef,
    tclCmdCount :: !Int,
    tclCmdWatchers :: [IO ()] }

type TclCmd = [T.TclObj] -> TclM T.TclObj

type CmdRef = IORef TclCmdObj

data TclCmdObj = TclCmdObj { 
                   cmdName :: BString, 
                   cmdOrigNS :: Maybe NSRef,
                   cmdParent :: Maybe CmdRef,
                   cmdKids :: [CmdRef],
                   cmdCore :: !CmdCore }

cmdIsProc cmd = case cmdCore cmd of
                  ProcCore {} -> True
                  _        -> False

cmdIsEnsem cmd = case cmdCore cmd of
                  EnsemCore {} -> True
                  _        -> False

type ArgSpec = Either BString (BString,T.TclObj)
type ArgList = [ArgSpec]
newtype ParamList = ParamList (BString, Bool, ArgList)

data CmdCore = CmdCore !TclCmd 
             | EnsemCore { ensemName :: BString,
                           ensemCmd :: TclCmd }
             | ProcCore { procBody :: BString, 
                          procArgs :: !ParamList,
                          procAction :: !(TclM T.TclObj) }

type ProcKey = BString
data CmdMap = CmdMap { 
      cmdMapEpoch :: !Int,
      unCmdMap :: !(Map.Map ProcKey CmdRef) 
  } 

type TclArray = Map.Map BString T.TclObj
data TclVar = ScalarVar !T.TclObj | ArrayVar TclArray | Undefined deriving (Eq,Show)
type TraceCB = Maybe (IO ())
type VarMap = Map.Map BString (TraceCB,TclVar)

runTclM :: TclM a -> TclState -> IO (Either Err a, TclState)
runTclM = runStateT . runExceptT . unTclM

execTclM c e = do 
  (r,s) <- runTclM c e 
  case r of
    Right _ -> return s
    Left e -> error (show e)
