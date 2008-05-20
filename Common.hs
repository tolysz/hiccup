{-# LANGUAGE BangPatterns #-}
module Common (TclM
       ,TclState
       ,Err(..)
       ,TclCmd,applyTo,cmdBody
       ,getOrigin
       ,runTclM
       ,makeState
       ,runCheckResult
       ,withLocalScope
       ,withNS
       ,makeCmdList
       ,makeNsCmdList
       ,mergeCmdLists
       ,getCmd
       ,getCmdNS
       ,registerProc
       ,varGetNS
       ,varModify
       ,varSetNS
       ,varSetHere
       ,varExists
       ,varUnset
       ,varUnsetNS
       ,renameCmd
       ,getArray
       ,addChan
       ,removeChan
       ,getChan
       ,evtAdd
       ,evtGetDue
       ,uplevel
       ,upvar
       ,io
       ,tclErr
       ,treturn
       ,ret
       ,argErr
       ,stackLevel
       ,getCmdCount
       ,globalVars
       ,localVars
       ,currentVars
       ,currentNS
       ,parentNS
       ,existsNS
       ,deleteNS
       ,childrenNS
       ,variableNS
       ,exportNS
       ,getExportsNS
       ,importNS
       ,commandNames
       ,commonTests
    ) where


import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.IORef
import Data.Unique

import Types

import Match (globMatch, globMatches)
import qualified EventMgr as Evt

import qualified TclObj as T
import TclChan
import VarName
import TclErr
import Util

import Test.HUnit

getOrigin p = if nso == nsSep 
                  then return $ B.append nso pname 
                  else return $ B.concat [nso, nsSep, pname]
 where nso = maybe nsSep id (cmdOrigNS p)
       pname = cmdName p

applyTo !f !args = do 
   modify (\x -> x { tclCmdCount = (tclCmdCount x) + 1 })
   (cmdAction f) args
{-# INLINE applyTo #-}

mkCmdAlias nsr pn = do
    mcr <- getCmdRef pn nsr
    case mcr of
      Nothing -> fail "trying to import proc that doesn't exist"
      Just cr -> do 
          newcmd <- readRef cr >>= \p -> return $! p { cmdAction = inner cr } 
          return newcmd
 where inner cr args = do 
            p <- readRef cr
            p `applyTo` args
  
type CmdSpec = (String,TclCmd)
data CmdList = CmdList { unCmdList :: [CmdSpec] }

makeCmdList = makeNsCmdList ""
makeNsCmdList p = CmdList . mapFst (\n -> p ++ n)

mergeCmdLists :: [CmdList] -> CmdList
mergeCmdLists = CmdList . concat . map unCmdList

toTclCmdObj (n,v) = return (bsn, TclCmdObj bsn False errStr Nothing v)
 where errStr = pack $ show bsn ++ " isn't a procedure"
       bsn = pack n

tclErr :: String -> TclM a
tclErr s = do
  (setErrorInfo s) `orElse` ret
  throwError (EDie s)

setErrorInfo s = do
  glFr <- getGlobalNS >>= getNSFrame
  varSet (VarName (pack "errorInfo") Nothing) (T.fromStr s) glFr


makeVarMap = Map.fromList . mapSnd ScalarVar

makeState :: [(BString,T.TclObj)] -> CmdList -> IO TclState
makeState = makeState' baseChans

makeState' :: ChanMap -> [(BString,T.TclObj)] -> CmdList -> IO TclState
makeState' chans vlist cmdlst = do 
    (fr,nsr) <- makeGlobal
    addChildNS nsr (pack "") nsr
    st <- return $! mkState nsr fr
    execTclM runRegister st
 where mkState nsr fr = TclState { tclChans = chans,
                                   tclEvents = Evt.emptyMgr,
                                   tclStack = [fr],
                                   tclGlobalNS = nsr,
                                   tclCmdCount = 0 }
       makeGlobal = do 
           fr <- createFrame (makeVarMap vlist) 
           nsr <- globalNS fr
           setFrNS fr nsr
           return (fr, nsr)
       exportAll ns = withNS (pack ns) (exportNS False (pack "*"))
       runRegister = do
           exportAll "::tcl::mathop"
           exportAll "::tcl::mathfunc"
           mapM toTclCmdObj (unCmdList cmdlst) >>= mapM (\(n,p) -> registerCmd (parseProc n) p)
       globalNS fr = newIORef $ TclNS { nsName = nsSep, 
                         nsCmds = emptyCmdMap, nsFrame = fr, 
                         nsExport = [],
                         nsParent = Nothing, nsChildren = Map.empty }

getStack = gets tclStack
{-# INLINE getStack  #-}

getNsCmdMap !nsr = liftIO (readIORef nsr >>= \v -> return $! (nsCmds v))
{-# INLINE getNsCmdMap #-}

putStack s = modify (\v -> v { tclStack = s })
{-# INLINE putStack  #-}
modStack :: (TclStack -> TclStack) -> TclM ()
modStack f = modify (\v -> v { tclStack = f (tclStack v) })
{-# INLINE modStack #-}

getFrame = do st <- gets tclStack
              case st of
                 (fr:_) -> return $! fr
                 _      -> tclErr "stack badness"

io :: IO a -> TclM a
io = liftIO
{-# INLINE io #-}

getCmdCount :: TclM Int
getCmdCount = gets tclCmdCount

stackLevel = getStack >>= return . pred . length
globalVars = getGlobalNS >>= getNSFrame >>= getFrameVars >>= return . Map.keys 
localVars = getFrame >>= getFrameVars >>= return . Map.keys 
currentVars = do f <- getFrame
                 vs <- getFrameVars f
                 mv <- getUpMap f
                 return $ Map.keys vs ++ Map.keys mv

commandNames procsOnly = nsCmdMap >>= (mapM readRef) . cmdMapElems >>= return . map cmdName . filt
 where nsCmdMap = getCurrNS >>= getNsCmdMap
       filt = if procsOnly then filter cmdIsProc else id

cmdMapElems :: CmdMap -> [CmdRef]
cmdMapElems = Map.elems . unCmdMap

argErr s = tclErr ("wrong # args: " ++ s)


modChan f = modify (\s -> s { tclChans = f (tclChans s) })
getChan n = gets tclChans >>= \m -> return (lookupChan n m)
addChan c    = modChan (insertChan c)
removeChan c = modChan (deleteChan c)

evtAdd e t = do 
  em <- gets tclEvents
  (tag,m) <- io $ Evt.addEvent e t em
  modify (\s -> s { tclEvents = m })
  return (T.fromBStr tag)

evtGetDue = do
  em <- gets tclEvents
  (d,em') <- io $ Evt.getDue em
  when (not (null d)) $ modify (\s -> s { tclEvents = em' })
  return d

upped !s !fr = getUpMap fr >>= \f -> return $! (let !sf = f in (Map.lookup s sf))
{-# INLINE upped #-}


{-
 -- Intended for future caching of method lookups in AST
data NsEpoch = NsEpoch (Maybe NSRef) !Int deriving (Eq) 
getNsEpoch nst = do
  nsr <- getNamespace nst
  cmEpoch <- liftM cmdMapEpoch (getNsCmdMap nsr)
  return $! (NsEpoch (Just nsr) cmEpoch)
-}
  
getCmd !pname = getCmdNS (parseProc pname)

getCmdNS (NSQual nst n) = do
  res <- tryHere `ifFails` Nothing
  case res of
    Nothing -> tryGlobal
    _       -> return $! res
 where
  tryHere = getNamespace nst >>= getCmdNorm n
  tryGlobal = if not (isGlobalQual nst)
               then do ns2 <- if noNsQual nst then getGlobalNS else getNamespace (asGlobal nst)
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
     Just v  -> readRef v >>= return . Just
{-# INLINE getCmdNorm #-}


rmProcNS (NSQual nst n) = getNamespace nst >>= rmFromNS
 where rmFromNS nsref = io $ changeCmds nsref (Map.delete n) 


registerProc name body pr = 
    let pn@(NSQual _ n) = parseProc name
    in registerCmd pn (TclCmdObj n True body Nothing pr)

registerCmd (NSQual nst k) newProc = getNamespace nst >>= regInNS
 where 
  pmInsert proc m = Map.insert k proc m
  regInNS nsr = do fn <- nsr `refExtract` nsName
                   newc <- io . newIORef $ setOrigin fn newProc
                   io $ changeCmds nsr (pmInsert newc)
  setOrigin fn x = if cmdOrigNS x == Nothing then x { cmdOrigNS = Just fn } else x

varSetRaw :: BString -> T.TclObj -> TclM RetVal
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
         let changeVar = insertVar frref str
         newVal <- case vnInd vn of
             Nothing -> case Map.lookup str vm of
                          Just (ArrayVar _) -> cantSetErr "variable is array"
                          _                 -> return (ScalarVar v)
             Just i  -> case Map.findWithDefault (ArrayVar Map.empty) str vm of
                          ArrayVar prev -> return (ArrayVar (Map.insert i v prev))
                          Undefined     -> return (ArrayVar (Map.singleton i v))
                          _     -> cantSetErr "variable isn't array"
         changeVar $! newVal


varModify !vn f = do 
  val <- varGetNS vn
  res <- f val
  varSetNS vn res
{-# INLINE varModify #-}

varExists :: BString -> TclM Bool
varExists name = (varGet name >> return True) `ifFails` False

renameCmd old new = do
  let pold = parseProc old
  mpr <- getCmdNS pold
  case mpr of
   Nothing -> tclErr $ "can't rename, bad command " ++ show old
   Just pr -> do rmProcNS pold
                 unless (bsNull new) (registerProc new (cmdBody pr) (cmdAction pr))

varUnset :: BString -> TclM RetVal
varUnset name = varUnsetNS (parseVarName name)

varUnsetNS :: NSQual VarName -> TclM RetVal
varUnsetNS qns = usingNsFrame qns varUnset'

usingNsFrame :: NSQual VarName -> (VarName -> FrameRef -> TclM RetVal) -> TclM RetVal 
usingNsFrame (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Nothing = getFrame 
       lookupNsFrame (Just n) = getNamespace' n >>= getNSFrame
{-# INLINE usingNsFrame #-}

{- This specialization is ugly, but GHC hasn't been doing it for me and it
 - knocks a few percent off the runtime of my benchmarks. -}
usingNsFrame2 :: NSQual BString -> (BString -> FrameRef -> TclM b) -> TclM b
usingNsFrame2 (NSQual !ns !vn) f = lookupNsFrame ns >>= f vn
 where lookupNsFrame Nothing = getFrame 
       lookupNsFrame ns  = getNamespace ns >>= getNSFrame
{-# INLINE usingNsFrame2 #-}

varUnset' vn frref = do
     isUpped <- upped (vnName vn) frref 
     case isUpped of
         Nothing    -> modVar >> ret
         Just (f,s) -> do 
             when (not (isArr vn)) $ do 
                 changeUpMap frref (Map.delete (vnName vn))
             varUnset' (vn {vnName = s}) f
 where noExist = cantUnset "no such variable" 
       cantUnset why = fail $ "can't unset " ++ showVN vn ++ ": " ++ why
       modArr v f = ArrayVar (f v)
       modVar = do
         vm <- getFrameVars frref
         let str = vnName vn
         let deleteVar = changeVars frref (Map.delete str)
         val <- maybe noExist return (Map.lookup str vm)
         case vnInd vn of
           Nothing -> deleteVar
           Just i  -> case val of
                        ArrayVar prev -> case Map.lookup i prev of 
                                           Nothing -> cantUnset "no such element in array"
                                           Just _  -> insertVar frref str (prev `modArr` (Map.delete i))
                        ScalarVar _   -> cantUnset "variable isn't array"
                        _             -> noExist


getArray :: BString -> TclM TclArray
getArray name = usingNsFrame2 (parseProc name) getArray'

getArray' :: BString -> FrameRef -> TclM TclArray
getArray' name frref = do
   var <- varLookup name frref
   case var of
      Just (ArrayVar a) -> return a
      Just _            -> fail $ "can't read " ++ show name ++ ": variable isn't array"
      Nothing           -> fail $ "can't read " ++ show name ++ ": no such variable"

varLookup !name !frref = do
   isUpped <- upped name frref
   case isUpped of
      Nothing    -> getFrameVars frref >>= \m -> return $! (Map.lookup name m)
      Just (f,n) -> varLookup n f

varGet :: BString -> TclM RetVal
varGet !n = varGetNS (parseVarName n)

varGetNS :: NSQual VarName -> TclM RetVal
varGetNS qns = usingNsFrame qns varGet'
{-# INLINE varGetNS #-}

varGet' vn !frref = do
  var <- varLookup (vnName vn) frref
  case var of
   Nothing -> cantReadErr "no such variable"
   Just o  -> o `getInd` (vnInd vn)
 where cantReadErr why  = fail $ "can't read " ++ showVN vn ++ ": " ++ why
       getInd (ScalarVar o) Nothing = return o
       getInd (ScalarVar _) _       = cantReadErr "variable isn't array"
       getInd (ArrayVar o) (Just i) = maybe (cantReadErr "no such element in array") return (Map.lookup i o)
       getInd (ArrayVar _)  _       = cantReadErr "variable is array"
       getInd Undefined     _       = cantReadErr "no such variable"


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
 ns <- getNamespace' nst >>= readRef
 whenJust (nsParent ns) $ \p -> removeChild p (nsTail nst)

removeChild nsr child = io (modifyIORef nsr (\v -> v { nsChildren = Map.delete child (nsChildren v) } ))
addChildNS nsr name child = (modifyIORef nsr (\v -> v { nsChildren = Map.insert name child (nsChildren v) } ))

getNamespace nst = case nst of 
        Nothing  -> getCurrNS
        Just nst -> getNamespace' nst
{-# INLINE getNamespace #-}

-- TODO: Unify namespace getters
getNamespace' (NS gq nsl) = do
    base <- if gq then getGlobalNS else getCurrNS 
    case nsl of
      [] -> return $! base
      _  -> foldM getKid base nsl
 where getKid !nsref !k = do 
          kids <- nsref `refExtract`  nsChildren
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

existsNS ns = (getNamespace' (parseNSTag ns) >> return True) `ifFails` False

variableNS name val = do
  let (NSQual ns (VarName n ind)) = parseVarName name
  ensureNotArr ind
  nsfr <- getNamespace ns >>= getNSFrame
  fr <- getFrame
  same <- sameTags fr nsfr
  if same then insertVar fr name varVal
          else n `linkToFrame` (nsfr, n)
 where
   ensureNotArr v = whenJust v $! \_ -> tclErr $ "can't define " ++ show name ++ ": name refers to value in array"
   varVal = maybe Undefined ScalarVar val
   sameTags f1 f2 = do
      t1 <- getTag f1
      t2 <- getTag f2
      return (t1 == t2)

exportNS clear name = do
  nsr <- getCurrNS
  io $ modifyIORef nsr (\n -> n { nsExport = (name:(getPrev n)) })
 where getPrev n = if clear then [] else nsExport n

getExportsNS = 
  getCurrNS >>= readRef >>= return . reverse . nsExport

whenJust x f = case x of
      Nothing -> return ()
      Just v  -> f $! v
{-# INLINE whenJust #-}

importNS force name = do
    let (NSQual nst n) = parseProc name
    nsr <- getNamespace nst
    exported <- getExports nsr n
    mapM (importProc nsr) exported
    return . T.mkTclList . map T.fromBStr $ exported
 where importProc nsr n = do
            np <- mkCmdAlias nsr n 
            when (not force) $ do
                 oldp <- getCmdNS (NSQual Nothing n)
                 whenJust oldp $ \_  -> tclErr $ "can't import command " ++ show n ++ ": already exists"
            registerCmd (NSQual Nothing n) np
       getExports nsr pat = do 
               ns <- readRef nsr
               let exlist = nsExport ns
               let pnames = Map.keys (unCmdMap (nsCmds ns))
               let filt = filter (\v -> or (map (`globMatch` v) exlist)) pnames
               return (globMatches pat filt)


getTag frref = do
  f <- readRef frref
  return (frTag f)

setFrNS !frref !nsr = modifyIORef frref (\f -> f { frNS = nsr })

withLocalScope vl f = do
    ns <- getCurrNS
    fr <- io $! createFrameWithNS ns $! makeVarMap vl
    withScope fr f
{-# INLINE withLocalScope #-}

withScope :: FrameRef -> TclM a -> TclM a
withScope !frref fun = do
  stack <- getStack
  -- when (length stack > 10000) (tclErr $ "Stack too deep: " ++ show 10000)
  putStack $ frref : stack
  fun `ensure` (modStack (drop 1))

mkEmptyNS name parent = do
    pname <- liftM nsName (readIORef parent)
    emptyFr <- createFrame emptyVarMap
    let sep = if pname == nsSep then B.empty else nsSep
    let fullname = B.concat [pname, sep, name]
    new <- newIORef $ TclNS { nsName = fullname, 
                              nsCmds = emptyCmdMap, nsFrame = emptyFr, 
                              nsExport = [],
                              nsParent = Just parent, nsChildren = Map.empty }
    addChildNS parent name new
    setFrNS emptyFr new
    return $! new

withNS :: BString -> TclM a -> TclM a
withNS name f = do
     newCurr <- getOrCreateNamespace (parseNSTag name)
     withExistingNS f newCurr

withExistingNS f !nsref = do
  fr <- getNSFrame nsref
  withScope fr f

getFrameVars :: FrameRef -> TclM VarMap
getFrameVars !frref = (frref `refExtract` frVars) 
{-# INLINE getFrameVars #-}

getUpMap !frref = frref `refExtract` upMap 
{-# INLINE getUpMap #-}

getNSFrame :: NSRef -> TclM FrameRef
getNSFrame !nsref = nsref `refExtract` nsFrame 


getCurrNS = getFrame >>= \fr -> liftIO (readIORef fr >>= \f -> return $! (frNS f))
{-# INLINE getCurrNS #-}

getGlobalNS = gets tclGlobalNS
{-# INLINE getGlobalNS #-}

readRef :: IORef a -> TclM a
readRef !r = (liftIO . readIORef) r
{-# INLINE readRef #-}

refExtract !ref !f = liftIO (readIORef ref >>= \x -> (return $! f x)) 
{-# INLINE refExtract #-}

currentNS = getCurrNS >>= readRef >>= return . nsName

parentNS nst = do
 ns <- getNamespace nst >>= readRef
 case nsParent ns of
   Nothing -> return B.empty
   Just v  -> readRef v >>= return . nsName

childrenNS nst = do
  ns <- getNamespace nst >>= readRef
  let prename = if nsSep `B.isSuffixOf` (nsName ns) then nsName ns else B.append (nsName ns) nsSep
  (return . map (B.append prename) . Map.keys . nsChildren) ns

ensure action p = do
   r <- action `catchError` (\e -> p >> throwError e)
   p
   return $! r
{-# INLINE ensure #-}

ret :: TclM RetVal
ret = return T.empty
{-# INLINE ret #-}

treturn :: BString -> TclM RetVal
treturn = return . T.fromBStr
{-# INLINE treturn #-}

createFrame !vref = do
   tag <- liftM hashUnique newUnique
   res <- newIORef $! TclFrame { frVars = vref, upMap = Map.empty, frTag = tag, frNS = undefined }
   return $! res

createFrameWithNS !nsref !vref = do
   tag <- liftM hashUnique newUnique
   res <- newIORef $! TclFrame { frVars = vref, upMap = Map.empty, frTag = tag, frNS = nsref }
   return $! res

changeUpMap fr fun = io (modifyIORef fr (\f -> f { upMap = fun (upMap f) }))

changeVars !fr fun = liftIO (modifyIORef fr (\f -> let !r = fun (frVars f) in f { frVars = r } ))
{-# INLINE changeVars #-}

insertVar fr !k !v = changeVars fr (Map.insert k v)
{-# INLINE insertVar #-}

changeCmds nsr fun = do 
           ns <- readIORef nsr 
           modKids (\x -> changeCmds x id) ns
           writeIORef nsr (updateNS ns)
 where update (CmdMap e m) = CmdMap (e+1) (fun m)
       updateNS ns = ns { nsCmds = update (nsCmds ns) }

modKids f ns = let kidfun kr = do 
                        kid <- readIORef kr
                        when (nsParent kid /= Nothing) (f kr)
              in mapM_ kidfun (Map.elems (nsChildren ns))

emptyCmdMap = CmdMap 0 Map.empty
emptyCmdList = CmdList []
emptyVarMap = Map.empty

-- # TESTS # --

runCheckResult :: TclM RetVal -> Either Err RetVal -> IO Bool
runCheckResult t v =
  do st <- mkEmptyState
     retv <- liftM fst (runTclM t st)
     return (retv == v)

errWithEnv :: TclM a -> IO (Either Err a)
errWithEnv t =
    do st <- mkEmptyState
       retv <- liftM fst (runTclM t st)
       return retv

mkEmptyState = makeState [] emptyCmdList

commonTests = TestList [ setTests, getTests, unsetTests, withScopeTests ] where

  b = pack

  evalWithEnv :: TclM a -> IO (Either Err a, TclStack)
  evalWithEnv t =
    do st <- mkEmptyState
       (retv, resStack) <- runTclM t st
       return (retv, tclStack resStack)


  checkErr a s = errWithEnv a >>= \v -> assertEqual "err match" (Left (EDie s)) v
  checkNoErr a = errWithEnv a >>= \v -> assertBool "err match" (isRight v)

  checkExists a n = do (_,(v:_)) <- evalWithEnv a
                       vExists n v

  vExists vn env = readIORef env >>= \fr -> return (frVars fr) >>= \vm -> assert (Map.member (b vn) vm)

  checkEq :: TclM t -> String -> T.TclObj -> Assertion
  checkEq a n val = do (_,(v:_)) <- evalWithEnv a
                       vEq n v val

  vEq vn env val = do
     vm <- readIORef env >>= return . frVars 
     assert ((Map.lookup (b vn) vm) == (Just (ScalarVar val)))

  value = int 666
  name = b "varname"

  isRight (Right _) = True
  isRight _         = False
  int = T.mkTclInt

  setTests = TestList [
       "set exists" ~: (varSetRaw (b "x") (int 1)) `checkExists` "x"
       ,"set exists2" ~: (varSetRaw (b "boogie") (int 1)) `checkExists` "boogie"
       ,"checkeq" ~: checkEq (varSetRaw name value) "varname" value
     ]
  withScopeTests = TestList [
      "with scope" ~: getVM (varSetRaw (b "x") (int 1)) (\m -> not (Map.null m))
    ]
   where getVM f c = do vmr <- createFrame emptyVarMap 
                        (res,_) <- evalWithEnv (withScope vmr f)
                        case res of
                         Left e -> error (show e)
                         Right _ -> do fr <- readIORef vmr
                                       vm <- return (frVars fr) 
                                       assertBool "getVM" (c vm)
                        

  getTests = TestList [
       "non-exist" ~: (varGet (b "boo")) `checkErr` "can't read \"boo\": no such variable"
       ,"no err if exists" ~: checkNoErr ((varSetRaw name value) >> varGet name)
     ]

  unsetTests = TestList [
       "non-exist" ~: (varUnset (b "boo")) `checkErr` "can't unset \"boo\": no such variable"
     ]

-- # ENDTESTS # --
