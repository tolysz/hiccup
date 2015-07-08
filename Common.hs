{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Common (TclM
       ,TclState
       ,Runnable(..)
       ,applyTo
       ,registerWatcher
       ,procBody
       ,getOriginName
       ,runTclM
       ,makeState
       ,setErrorInfo
       ,runInterp
       ,registerInterp
       ,getInterp
       ,getInterpNames
       ,deleteInterp
       ,interpHide
       ,runCheckResult
       ,withProcScope
       ,withNS
       ,getCmd
       ,getFrameInfo
       ,getProcInfo
       ,getCmdNS
       ,getCurrNS
       ,registerEnsem
       ,registerProc
       ,registerCmd
       ,varGetNS
       ,varSetNS
       ,varSetHere
       ,varExists
       ,varUnsetNS
       ,varTraceNS
       ,renameCmd
       ,getArray
       ,addChan
       ,removeChan
       ,getChan
       ,namesChan
       ,evtAdd
       ,evtGetDue
       ,evtInfo
       ,evtCancel
       ,evtNextDeadline
       ,uplevel
       ,upvar
       ,io
       ,tclErr
       ,ret
       ,argErr
       ,stackLevel
       ,getCmdCount
       ,globalVars
       ,localVars
       ,commandNames
       ,currentVars
       ,currentNS
       ,parentNS
       ,existsNS
       ,deleteNS
       ,childrenNS
       ,variableNS
       ,getUnknownNS
       ,setUnknownNS
       ,exportNS
       ,getExportsNS
       ,getExportCmds
       ,importNS
       ,forgetNS
       ,addToPathNS
       ,getPathNS
       ,commonTests
    ) where


import qualified Data.ByteString.Char8 as B
-- import Control.Monad.Error (throwError)
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.IORef
import Data.Unique
import Control.Monad.Error

import Proc.Params

import Internal.Types
import Internal.Util
import Internal.InterpSpec

import Match (globMatch, globMatches)
import qualified EventMgr as Evt

import qualified TclObj as T
import qualified TclChan as C
import VarName
import TclErr
import Util
import CmdList

import Test.HUnit

getOrigin :: TclCmdObj -> TclM NSRef
getOrigin p = case cmdParent p of
                Nothing  -> case cmdOrigNS p of
                              Nothing -> fail "Can't find origin namespace!"
                              Just nsr -> return nsr
                Just par -> readRef par >>= getOrigin
                 
getOriginName p = getOrigin p >>= readRef >>= \ns -> return $ fixNSName (nsName ns) (cmdName p)


fixNSName rt t = if rt == nsSep then B.append rt t
                                else B.concat [rt, nsSep, t]

applyTo !f args =
   -- modify (\x -> let !r = x { tclCmdCount = (tclCmdCount x) + 1 } in r)
   case cmdCore f of
     CmdCore c      -> c args
     e@(EnsemCore {})  -> (ensemCmd e) args
     p@(ProcCore _ _ c) -> case cmdOrigNS f of
                              Nothing -> error "can't find orig namespace"
                              Just nsr -> withProcScope (procArgs p) nsr c (cmdName f) args
{-# INLINE applyTo #-}



mkCmdAlias :: CmdRef -> [T.TclObj] -> TclM TclCmdObj
mkCmdAlias cr adl = do
    p <- readRef cr 
    return $! p { cmdCore = (cmdCore p) `withAct` (inner cr), 
                  cmdParent = Just cr, 
                  cmdOrigNS = Nothing } 
 where inner cr args = do 
            p <- readRef cr
            p `applyTo` (adl ++ args)
       withAct core a = case core of
          CmdCore _ -> CmdCore a
          EnsemCore {} -> error "ensemble alias unsupported!"
          ProcCore n b c -> ProcCore n b c
  
addKid cr k = cr .= (\p -> p { cmdKids = k:(cmdKids p) })


toCmdObjs = mapM toTclCmdObj . unCmdList

toTclCmdObj cs = return (bsn, emptyCmd { cmdName = bsn,
                                         cmdCore = CmdCore v} )
 where bsn = pack n
       n = cmdSpecName cs
       v = cmdSpecCmd cs

-- tclErr :: String -> TclM a
-- tclErr :: String -> Control.Monad.Trans.Error.ErrorT Err m b
tclErr s = do
  attempt (setErrorInfo s)
  TclM $ throwError (eDie s)

setErrorInfo :: String -> TclM ()
setErrorInfo s = do
  glFr <- getGlobalNS >>= getNSFrame
  void $ varSet (VarName (pack "errorInfo") Nothing) (T.fromStr s) glFr


makeVarMap = Map.fromList . mapSnd (\c -> (Nothing, ScalarVar c))

runInterp t (Interp i) = do
  bEnv <- readIORef i
  (r,i') <- runTclM t bEnv
  writeIORef i i'
  return $! r

inInterp c i = io (runInterp c i) >>= fixres
 where fixres (Right x) = return x
       fixres (Left e) = throwError e

modInterps f s = s { tclInterps = f (tclInterps s) }
getInterps = gets tclInterps

registerInterp path interp cmd = inner path
 where regInterp n = do 
             modify (modInterps (Map.insert n interp))
             registerCmd n cmd
       inner path = case path of
          [n] -> regInterp n >> ret
          (x:xs) -> lookupInterp x >>= \v -> inner xs `inInterp` v
          [] -> fail "invalid interpreter path"
  
lookupInterp n = do
   its <- getInterps
   case Map.lookup n its of
     Nothing -> tclErr $ "could not find interpreter " ++ show n
     Just v  -> return v

getInterp nl = do
  ir <- get >>= io . newIORef >>= return . Interp
  inner ir nl
 where inner ir []     = return ir
       inner (Interp ir) (x:xs) = do
         cir <- ir `refExtract` tclInterps
         case Map.lookup x cir of
            Nothing -> tclErr $ "could not find interpreter " ++ show x
            Just v  -> inner v xs

getInterpNames :: TclM [BString]
getInterpNames = getInterps >>= return . Map.keys

deleteInterp path = case path of
 [n] -> do
   im <- getInterps
   case Map.lookup n im of
     Nothing -> tclErr $ "could not find interpreter " ++ show n
     Just _  -> do 
       modify (modInterps (Map.delete n))
       renameCmd n (pack "")
       ret
 (n:nx) -> do 
   i <- lookupInterp n
   deleteInterp nx `inInterp` i
 _ -> fail "invalid interp path"

interpHide path name = do
  getInterp path >>= \i -> (hideCmd name) `inInterp` i

hideCmd name = do
  mpr <- getCmd name
  case mpr of
   Nothing -> tclErr $ "unknown command" ++ show name
   Just pr -> do removeCmd pr
                 pref <- io (newIORef pr)
                 st <- get
                 let nst = st { tclHidden = modmap (tclHidden st) (Map.insert name pref) }
                 put nst
 where modmap (CmdMap n m) f = CmdMap (n+1) (f m)

baseInit cmdlst = do
   exportAll "::tcl::mathop"
   exportAll "::tcl::mathfunc"
   setUnknownNS $ pack "::unknown"
   (io . toCmdObjs) cmdlst >>= mapM_ (\(n,p) -> registerCmdObj (parseProc n) p)
 where exportAll ns = withNS (pack ns) (exportNS False (pack "*") >> ret)

makeState :: InterpSpec -> IO TclState
makeState is = do 
    (fr,nsr) <- makeGlobal
    nsr `modifyIORef` (addChildNS (pack "") nsr)
    hiddens <- cmdListToCmdMap (ispecHidden is)
    st <- return $! mkState nsr hiddens fr
    runInits st
 where mkState nsr hiddens fr = TclState { interpSafe = ispecSafe is,
                                   tclChans = ispecChans is,
                                   tclInterps = Map.empty,
                                   tclEvents = Evt.emptyMgr,
                                   tclHidden = hiddens,
                                   tclStack = [fr],
                                   tclGlobalNS = nsr,
                                   tclCmdCount = 0,
                                   tclCmdWatchers = [] }
       makeGlobal = do 
           fr <- createFrame (makeVarMap (ispecVars is)) 
           nsr <- globalNS fr
           setFrNS fr nsr
           return (fr, nsr)
       runInits = let initlist = baseInit (ispecCmds is) : (ispecInits is)
                  in execTclM .sequence_ $ initlist
       globalNS fr = newIORef $ emptyNS nsSep fr
       toCmdRefs (a,b) = newIORef b >>= \br -> return (a,br)
       cmdListToCmdMap cl = toCmdObjs cl >>= mapM toCmdRefs >>= return . CmdMap 0 . Map.fromList
       


getNsCmdMap :: NSRef -> TclM CmdMap
getNsCmdMap !nsr = liftIO (readIORef nsr >>= \v -> return $! (nsCmds v))
{-# INLINE getNsCmdMap #-}


stackLevel = getStack >>= return . pred . length
globalVars = getGlobalNS >>= getNSFrame >>= getFrameVars >>= return . Map.keys 
localVars = getFrame >>= getFrameVars >>= return . Map.keys 
currentVars = do mv <- getFrame >>= getUpMap
                 vs <- localVars
                 return $ vs ++ Map.keys mv

     
commandNames mns procsOnly = nsList >>= mapM mapElems >>= return . map cmdName . filt . concat
 where mapElems e = getNsCmdMap e >>= mapM readRef . Map.elems . unCmdMap
       nsList = do
          c <- getNamespaceHere mns
          has_par <- hasParent c
          case (has_par,isJust mns) of
             (True,False)  -> do
                 gns <- getGlobalNS 
                 return [gns, c]
             _ -> return [c]
       filt = if procsOnly then filter cmdIsProc else id

cmdMapElems :: CmdMap -> [CmdRef]
cmdMapElems = Map.elems . unCmdMap

argErr s = tclErr ("wrong # args: " ++ s)

getChan n = gets tclChans >>= \m -> return (C.lookupChan n m)
modChan f = modify (\s -> s { tclChans = f (tclChans s) })
addChan c    = modChan (C.insertChan c)
removeChan c = modChan (C.deleteChan c)
namesChan :: TclM [BString]
namesChan = gets tclChans >>= return . C.namesChan

evtAdd e t = do 
  em <- gets tclEvents
  (tag,m) <- io $ Evt.addEvent e t em
  modify (\s -> s { tclEvents = m })
  return (T.fromBStr tag)

evtInfo :: TclM [BString]
evtInfo = do
 em <- gets tclEvents
 return $ Evt.eventNames em

evtCancel eid = do
  modify (\s -> s { tclEvents = Evt.cancelEvent eid (tclEvents s) })

evtNextDeadline :: TclM (Maybe Evt.EvtTime)
evtNextDeadline = gets tclEvents >>= return . Evt.nextDeadline

evtGetDue = do
  em <- gets tclEvents
  (d,em') <- io $ Evt.getDue em
  when (not (null d)) $ modify (\s -> s { tclEvents = em' })
  return d

upped !s !fr = getUpMap fr >>= \f -> return $! (let !sf = f in Map.lookup s sf)
{-# INLINE upped #-}

getCmd !pname = getCmdNS (parseProc pname)

getFrameInfo = getFrame >>= (`refExtract` frInfo)

getProcInfo !pname = do
  mcmd <- getCmd pname
  case mcmd of
   Nothing -> err
   Just cmd -> case cmdCore cmd of
                  pi@(ProcCore {}) -> return pi
                  _ -> err
 where err = tclErr $ show pname ++ " isn't a procedure"

getCmdNS (NSQual nst n) =
  tryHere `ifNoResult` tryPaths `ifNoResult` tryGlobal
 where
  tryHere = getNamespaceHere nst >>= getCmdNorm n
  ifNoResult f v = (f `ifFails` Nothing) >>= 
                            \r -> case r of
                                   Nothing -> v
                                   _       -> return $! r
  {-# INLINE ifNoResult #-}
  tryPaths 
   | globalQual = return Nothing
   | otherwise = do
       nsr <- getCurrNS
       path <- nsr `refExtract` nsPath
       case path of
         [] -> return Nothing
         lst -> let getInNS nsr = getNamespace (return nsr) nst >>= getCmdNorm n
                in foldr1 ifNoResult (map getInNS lst)
  globalQual = isGlobalQual nst 
  tryGlobal = if not globalQual
               then do ns2 <- if noNsQual nst then getGlobalNS else getNamespaceHere (asGlobal nst)
                       getCmdNorm n ns2
               else return Nothing
{-# INLINE getCmdNS #-}

getCmdRef !i !nsr = do
  currpm <- getNsCmdMap nsr
  return $! pmLookup i currpm
 where pmLookup :: ProcKey -> CmdMap -> Maybe CmdRef
       pmLookup !i !m = Map.lookup i (unCmdMap m)
       {-# INLINE pmLookup #-}
{-# INLINE getCmdRef #-}

getCmdNorm :: ProcKey -> NSRef -> TclM (Maybe TclCmdObj)
getCmdNorm !i !nsr = do
  cr <- getCmdRef i nsr
  case cr of
     Nothing -> return Nothing
     Just v  -> liftIO (readIORef v >>= return . Just)
{-# INLINE getCmdNorm #-}

deleteCmd name = (`changeCmds` (Map.delete name))

removeCmd cmd = case cmdOrigNS cmd of
    Nothing -> io (putStrLn $ "NO ORIGIN FOR " ++ show (cmdName cmd)) >> return ()
    Just nsr -> deleteCmd (cmdName cmd) nsr

registerCmd name pr = 
    let pn@(NSQual _ n) = parseProc name
    in registerCmdObj pn (emptyCmd { cmdName = n,
                                     cmdCore = CmdCore pr })
registerProc name pr = 
    let pn@(NSQual _ n) = parseProc name
    in registerCmdObj pn (emptyCmd { cmdName = n,
                                     cmdCore = pr })

registerCmdObj (NSQual nst k) newCmd = getNamespaceHere nst >>= regInNS
 where 
  regInNS nsr = do 
           newc <- io . newIORef $ newCmd { cmdOrigNS = Just nsr, cmdName = k }
           changeCmds nsr (Map.insert k newc)
           return newc

registerEnsem :: BString -> TclCmd -> TclM ()
registerEnsem name en = do
   let pn@(NSQual _ n) = parseProc name
   registerCmdObj pn (emptyCmd { cmdName = n,
                                 cmdCore = EnsemCore n en })
   return ()

varSetRaw !n v = varSetNS (parseVarName n) v

varSetNS qvn v = usingNsFrame qvn (\n f -> varSet n v f)
{-# INLINE varSetNS #-}

varSetHere vn v = getFrame >>= varSet vn v
{-# INLINE varSetHere #-}

varSet vn v frref = do
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Nothing    -> modVar (vnName vn) >> return v
         Just (f,s) -> varSet (vn {vnName = s}) v f
 where cantSetErr why = fail $ "can't set " ++ showVN vn ++ ":" ++ why
       modVar str = do
         vm <- getFrameVars frref
         newVal <- case vnInd vn of
             Nothing -> case fmap snd (Map.lookup str vm) of
                          Just (ArrayVar _) -> cantSetErr "variable is array"
                          _                 -> return (ScalarVar v)
             Just i  -> case snd (Map.findWithDefault (Nothing,Undefined) str vm) of
                          ArrayVar prev -> return (ArrayVar (Map.insert i v prev))
                          Undefined     -> return (ArrayVar (Map.singleton i v))
                          _     -> cantSetErr "variable isn't array"
         insertVar frref str $! newVal

varExists :: BString -> TclM Bool
varExists name = (varGetRaw name >> return True) `ifFails` False

renameCmd old new = do
  let pold = parseProc old
  mpr <- getCmdNS pold
  case mpr of
   Nothing -> tclErr $ "can't rename, bad command " ++ show old
   Just pr -> do removeCmd pr
                 unless (bsNull new) (registerCmdObj (parseProc new) pr >> return ())

varUnsetNS :: NSQual VarName -> TclM RetVal
varUnsetNS qns = usingNsFrame qns varUnset

varUnset vn frref = do
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Nothing    -> modVar >> ret
         Just (f,s) -> do 
             when (not (isArr vn)) $ do 
                 changeUpMap frref (Map.delete (vnName vn))
             varUnset (vn {vnName = s}) f
 where noExist = cantUnset "no such variable" 
       cantUnset why = fail $ "can't unset " ++ showVN vn ++ ": " ++ why
       modArr v f = ArrayVar (f v)
       modVar = do
         vm <- getFrameVars frref
         let str = vnName vn
         val <- maybe noExist (return . snd) (Map.lookup str vm)
         case vnInd vn of
           Nothing -> case val of
                       Undefined -> noExist
                       _         -> deleteVar frref str
           Just i  -> case val of
                        ArrayVar prev -> case Map.lookup i prev of 
                                           Nothing -> cantUnset "no such element in array"
                                           Just _  -> insertVar frref str (prev `modArr` (Map.delete i))
                        ScalarVar _   -> cantUnset "variable isn't array"
                        _             -> noExist

usingNsFrame :: NSQual VarName -> (VarName -> FrameRef -> TclM RetVal) -> TclM RetVal 
usingNsFrame (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Nothing = getFrame 
       lookupNsFrame (Just n) = (getNamespaceHere' n `orElse` tryGlobal n) >>= getNSFrame
       tryGlobal (NS _ t) = getNamespaceHere' (NS True t)
{-# INLINE usingNsFrame #-}

{- This specialization is ugly, but GHC hasn't been doing it for me and it
 - knocks a few percent off the runtime of my benchmarks. -}
usingNsFrame2 :: NSQual BString -> (BString -> FrameRef -> TclM b) -> TclM b
usingNsFrame2 (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Nothing  = getFrame 
       lookupNsFrame (Just n) = (getNamespaceHere' n `orElse` tryGlobal n) >>= getNSFrame
       tryGlobal (NS _ t) = getNamespaceHere' (NS True t)
{-# INLINE usingNsFrame2 #-}



getArray :: BString -> TclM TclArray
getArray name = usingNsFrame2 (parseProc name) getArray'

getArray' :: BString -> FrameRef -> TclM TclArray
getArray' name frref = do
   var <- varLookup name frref
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> fail $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> fail $ "can't read " ++ show name ++ ": no such variable"

varLookup :: BString -> FrameRef -> TclM (Maybe TclVar)
varLookup !name !frref = do
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> getFrameVars frref >>= \m -> return $! fmap snd (Map.lookup name m)
      Just (f,n) -> varLookup n f

varTraceNS fn qns = usingNsFrame qns (varTrace' fn)
varTrace' fn vn frref = traceVar frref (vnName vn) fn >> ret


varGetRaw :: BString -> TclM RetVal
varGetRaw !n = varGetNS (parseVarName n)

varGetNS :: NSQual VarName -> TclM RetVal
varGetNS qns = usingNsFrame qns varGet'
{-# INLINE varGetNS #-}

varGet' vn !frref = do
  var <- varLookup (vnName vn) frref
  case var of
   Nothing -> cantReadErr "no such variable"
   Just o  -> o `getInd` (vnInd vn)
 where cantReadErr why  = fail $ "can't read " ++ showVN vn ++ ": " ++ why
       getInd (ScalarVar o) Nothing = return $! o
       getInd (ArrayVar o) (Just i) = case Map.lookup i o of
                                         Just v -> return $! v
                                         _      -> cantReadErr "no such element in array"
       getInd (ScalarVar _) _       = cantReadErr "variable isn't array"
       getInd (ArrayVar _)  _       = cantReadErr "variable is array"
       getInd Undefined     _       = cantReadErr "no such variable"
       {-# INLINE getInd #-}


uplevel :: Int -> TclM a -> TclM a
uplevel i p = do
  (curr,new) <- liftM (splitAt i) getStack
  when (null new) (fail ("bad level: " ++ show i))
  putStack new
  res <- p `ensure` (modStack (curr ++))
  return res
{-# INLINE uplevel #-}

getUpFrame i = do st <- getStack
                  if length st <= i
                      then fail "too far up the stack"
                      else return $! (st!!i)
                  
linkToFrame name (upfr, upname) = do
  frref <- getFrame
  changeUpMap frref (Map.insert name (upfr, upname))

upvar n d s = do
   upfr <- getUpFrame n
   s `linkToFrame` (upfr, d)
{-# INLINE upvar #-}

deleteNS name = do 
    let nst = parseNSTag name 
    nsr <- getNamespaceHere' nst
    ns <- readRef nsr
    mapM_ (rmPathRef nsr) (nsPathLinks ns)
    kids <- mapM (`refExtract` cmdKids) (cmdMapElems (nsCmds ns))
    mapM_ (\k -> readRef k >>= removeCmd) (concat kids)
    whenJust (nsParent ns) $ \p -> p .= removeChild (nsTail nst)
 where rmPathRef dest src = src .= (modNsPath (filter (/= dest)))

onNsChildren f = \v -> v { nsChildren = f (nsChildren v) }
{-# INLINE onNsChildren #-}
removeChild child = onNsChildren (Map.delete child)
addChildNS name child = onNsChildren (Map.insert name child)

getNamespaceHere = getNamespace getCurrNS
getNamespace getcurr nst = case nst of 
        Nothing  -> getcurr
        Just nst -> getNamespace' getcurr nst
{-# INLINE getNamespace #-}

-- TODO: Unify namespace getters
getNamespaceHere' = getNamespace' getCurrNS
getNamespace' getcurr (NS gq nsl) = do
    base <- if gq then getGlobalNS else getcurr
    case nsl of
      [] -> return $! base
      _  -> foldM getKid base nsl
 where getKid !nsref !k = do 
          kids <- nsref `refExtract` nsChildren
          case Map.lookup k kids of
             Nothing -> tclErr $ "can't find namespace " ++ show k ++ " in " ++ show nsl 
             Just v  -> return $! v
{-# INLINE getNamespace' #-}

getOrCreateNamespace (NS gq nsl) = do
    base <- if gq then getGlobalNS else getCurrNS 
    foldM getKid base nsl
 where getKid nsref k = do 
          kids <- nsref `refExtract` nsChildren
          case Map.lookup k kids of
             Nothing -> io (mkEmptyNS k nsref)
             Just v  -> return $! v

existsNS ns = (getNamespaceHere' (parseNSTag ns) >> return True) `ifFails` False


getUnknownNS :: TclM (Maybe BString)
getUnknownNS = getCurrNS >>= (`refExtract` nsUnknown)

setUnknownNS n = getCurrNS >>= \nsr -> nsr .= (\ns -> ns { nsUnknown = (Just n) })

variableNS name val = do
  let (NSQual ns (VarName n ind)) = parseVarName name
  ensureNotArr ind
  nsfr <- getNamespaceHere ns >>= getNSFrame
  fr <- getFrame
  same <- sameTags fr nsfr
  if same then insertVar fr name varVal
          else n `linkToFrame` (nsfr, n)
 where
   ensureNotArr v = whenJust v $! \_ -> tclErr $ "can't define " ++ show name ++ ": name refers to value in array"
   varVal = maybe Undefined ScalarVar val
--    getTag :: (MonadIO m) => IORef TclFrame -> ErrorT Err m Int
   getTag = (`refExtract` frTag)
   sameTags f1 f2 = do
      t1 <- getTag f1
      t2 <- getTag f2
      return (t1 == t2)

exportNS clear name = do
  nsr <- getCurrNS
  nsr .= (\n -> n { nsExport = (name:(getPrev n)) })
 where getPrev n = if clear then [] else nsExport n

getExportsNS = 
  getCurrNS >>= (`refExtract` (reverse . nsExport))


importNS :: Bool -> BString -> TclM T.TclObj
importNS force name = do
    let (NSQual nst n) = parseProc name
    nsr <- getNamespaceHere nst
    exported <- getExports nsr n
    mapM_ (importCmd nsr) exported
    return . T.fromBList $ exported
 where importCmd nsr n = do
            mcr <- getCmdRef n nsr
            whenJust mcr $ \cr -> do
              let dest_name = NSQual Nothing n
              when (not force) $ do
                 samename <- getCmdNS dest_name
                 whenJust samename $ \_  -> tclErr $ "can't import command " ++ show n ++ ": already exists"
              cmdLink cr dest_name

cmdLink cr dest_name = do
    np <- mkCmdAlias cr []
    newc <- registerCmdObj dest_name np
    addKid cr newc

getExportCmds = do
   nsr <- getCurrNS
   ns <- readRef nsr
   let expats = nsExport ns
   let nscmds = Map.toList (unCmdMap (nsCmds ns))
   let is_exported v = or (map (`globMatch` v) expats)
   return $ filter (\(k,_) -> is_exported k) nscmds

getExports nsr pat = do 
   ns <- readRef nsr
   let expats = nsExport ns
   let pnames = Map.keys (unCmdMap (nsCmds ns))
   let exported = filter (\v -> or (map (`globMatch` v) expats)) pnames
   return (globMatches pat exported)

forgetNS name = do
   let qns@(NSQual nst n) = parseProc name
   case nst of 
     Just _ -> do
        nsr <- getNamespaceHere nst
        exported <- getExports nsr n
        cns <- getCurrNS
        mapM_ (\x -> deleteCmd x cns) exported
     Nothing -> do
       mCmd <- getCmdNS qns 
       case mCmd of
         Just cmd -> whenJust (cmdParent cmd) $ \_ -> removeCmd cmd
         Nothing -> fail "no such command to forget"


setFrNS !frref !nsr = modifyIORef frref (\f -> f { frNS = nsr })

withProcScope :: ParamList -> NSRef -> TclM T.TclObj -> BString -> [T.TclObj] -> TclM T.TclObj
withProcScope pl !nsr f pn args = do
    vl <- bindArgs pl args
    fr <- io $! createFrameWithNS nsr ((T.fromBStr pn):args) $! makeVarMap vl
    withScope fr f
{-# INLINE withProcScope #-}

withScope :: FrameRef -> TclM T.TclObj -> TclM T.TclObj
withScope !frref fun = do
  stack <- getStack
  -- when (length stack > 10000) (tclErr $ "Stack too deep: " ++ show 10000)
  putStack $! frref : stack
  fun `ensure` (modStack (drop 1))

mkEmptyNS name parent = do
    pname <- liftM nsName (readIORef parent)
    emptyFr <- createFrame emptyVarMap
    new <- newIORef $ (emptyNS (fixNSName pname name) emptyFr) { nsParent = Just parent }
    parent `modifyIORef` (addChildNS name new)
    setFrNS emptyFr new
    return $! new

withNS name f = do
     newCurr <- getOrCreateNamespace (parseNSTag name)
     withExistingNS f newCurr

withExistingNS f !nsref = do
  fr <- getNSFrame nsref
  withScope fr f

addToPathNS nst = do
  pns <- getNamespaceHere' nst
  cnsr <- getCurrNS
  cnsr .= modNsPath ((pns:))
  pns .= (\n -> n { nsPathLinks = (cnsr:(nsPathLinks n)) })

modNsPath f = \ns -> ns { nsPath = f (nsPath ns) }

getPathNS = do
  cnsr <- getCurrNS
  pathr <- cnsr `refExtract` nsPath
  mapM (`refExtract` nsName) pathr

currentNS = getCurrNS >>= (`refExtract` nsName)

parentNS nst = do
 par <- getNamespaceHere nst >>= (`refExtract` nsParent)
 case par of
   Nothing -> return (pack "")
   Just v  -> readRef v >>= return . nsName

childrenNS nst = do
  ns <- getNamespaceHere nst >>= readRef
  let prename = if nsSep `B.isSuffixOf` (nsName ns) then nsName ns else B.append (nsName ns) nsSep
  (return . map (B.append prename) . Map.keys . nsChildren) ns


createFrame !vref = createFrameWithNS undefined [] vref
createFrameWithNS nsref frinv !vref = do
   tag <- uniqueInt
   newIORef $! TclFrame { frVars = vref, upMap = Map.empty, frTag = tag, frNS = nsref,
                          frInfo = frinv }
 where uniqueInt = liftM hashUnique newUnique

changeUpMap fr fun = fr .= (\f -> f { upMap = fun (upMap f) })

lookupInsert !k !v !m = Map.insertLookupWithKey inserter k v m
 where inserter _ (ntr,nv) (otr,_) = (ntr `mplus` otr, nv)

traceInsert !fref !k !v = do
  fr <- readIORef fref
  let vars = frVars fr
  let (mold,nvars) = lookupInsert k v vars
  whenJust mold (\(tr,_) -> whenJust tr (\op -> op))
  writeIORef fref (fr { frVars = nvars })

traceVar !fref !k tr = do
  fr <- readRef fref
  let vars = frVars fr
  let mval = fmap snd (Map.lookup k vars)
  insertTraceVar tr fref k (maybe Undefined id mval)

traceDelete !frref !k = do
  fr <- readIORef frref
  let vars = frVars fr
  let val = Map.lookup k vars 
  whenJust val (\(tr,_) -> whenJust tr (\op -> op))
  writeIORef frref (fr { frVars = Map.delete k vars })
  
insertTraceVar tr fr k v = io $ traceInsert fr k (Just tr,v)
insertVar fr k v = io $ traceInsert fr k (Nothing,v)
{-# INLINE insertVar #-}

deleteVar fr k = io $ traceDelete fr k

changeCmds nsr fun = nsr .= updateNS >> notifyWatchers
 where update (CmdMap e m) = CmdMap (e+1) (fun m)
       updateNS ns = ns { nsCmds = update (nsCmds ns) }

emptyCmdMap = CmdMap 0 Map.empty
emptyVarMap = Map.empty

emptyCmd = TclCmdObj { 
       cmdName = pack "",
       cmdCore = CmdCore (\_ -> fail "empty command"),
       cmdOrigNS = Nothing,
       cmdParent = Nothing,
       cmdKids = [] }

emptyNS name frame = TclNS { nsName = name, 
                  nsCmds = emptyCmdMap, nsFrame = frame, 
                  nsExport = [],
                  nsParent = Nothing, nsChildren = Map.empty,
                  nsPath = [],
                  nsPathLinks = [],
                  nsUnknown = Nothing }

-- # TESTS # --

runCheckResult :: TclM RetVal -> Either Err RetVal -> IO Bool
runCheckResult t v =
  do retv <- liftM fst $ evalClean t
     return (retv == v)

errWithEnv :: TclM a -> IO (Either Err a)
errWithEnv t = evalClean t >>= return . fst

evalClean :: TclM a -> IO (Either Err a, TclStack)
evalClean t =
    do st <- mkEmptyState
       (retv, resStack) <- runTclM t st
       return (retv, tclStack resStack)

mkEmptyState = makeState (ISpec { ispecSafe = False,
                                   ispecVars = [],
                                   ispecChans = C.baseChans,
                                   ispecCmds = emptyCmdList,
                                   ispecHidden = emptyCmdList,
                                   ispecInits = []})

commonTests = TestList [ setTests, getTests, unsetTests, withScopeTests ] where
  b = pack

  checkErr a s = errWithEnv a >>= \v -> assertEqual "err match" (Left (eDie s)) v
  checkNoErr a = errWithEnv a >>= \v -> assertBool "err match" (isRight v)
   where isRight (Right _) = True
         isRight _         = False

  vExists vn env = readVars env >>= \vm -> assert (Map.member (b vn) vm)
  readVars frref = readIORef frref >>= return . frVars 


  vEq vn frref val = do
     vm <- readVars frref
     assert (fmap snd (Map.lookup (b vn) vm) == (Just (ScalarVar val)))

  value = int 666
  name = b "varname"
  int = T.fromInt

  setTests = TestList [
       "set exists" ~: (varSetRaw (b "x") (int 1)) `checkExists` "x"
       ,"set exists2" ~: (varSetRaw (b "boogie") (int 1)) `checkExists` "boogie"
       ,"checkeq" ~: checkEq (varSetRaw name value) "varname" value
     ]
    where evalGetHead a = evalClean a >>= return . head . snd 
          checkExists a n = evalGetHead a >>= vExists n
          checkEq a n val = evalGetHead a >>= \v -> vEq n v val

  withScopeTests = TestList [
      "with scope" ~: getVM (varSetRaw (b "x") (int 1)) (\m -> not (Map.null m))
    ]
   where getVM f c = do vmr <- createFrame emptyVarMap 
                        (res,_) <- evalClean (withScope vmr f)
                        case res of
                         Left e -> error (show e)
                         Right _ -> do vm <- readVars vmr
                                       assertBool "getVM" (c vm)
                        

  getTests = TestList [
       "non-exist" ~: (varGetRaw (b "boo")) `checkErr` "can't read \"boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSetRaw name value) >> varGetRaw name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnsetNS (parseVarName (b "boo"))) `checkErr` "can't unset \"boo\": no such variable"
     ]


-- # ENDTESTS # --
