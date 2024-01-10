module Eval(eval, evalShow, Value(..)) where

import Data.Maybe(fromJust)
import Data.List(sortBy, findIndex)
import Data.Ord(Ordering, comparing)
import qualified Data.Map as Map
import qualified Data.Set as Set

----

import Id(Id)
import Expr(Expr(..))

import Timeline(Timeline, Instant)
import qualified Timeline as T

import Memory(Addr, Memory)
import qualified Memory as Mem

import Environment(Env)
import qualified Environment as Env

import qualified Flist as FL

import StateIO(SIO, getS, putS, modifyS, runS, liftSIO, failS, evalS, logS)

----

joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

----

data Dump = Dump {
              dumpEnv        :: Env Addr
            , dumpOuterStack :: [(Instant, Env Addr)]
            }

initialDump :: Env Addr -> Dump
initialDump env = Dump {
                    dumpEnv        = env
                  , dumpOuterStack = []
                  }

dumpExtend1 :: Id -> Addr -> Dump -> Dump
dumpExtend1 x addr dump =
  dump {
    dumpEnv = Env.extend1 x addr (dumpEnv dump)
  }

dumpExtend :: [(Id, Addr)] -> Dump -> Dump
dumpExtend rib dump =
  dump {
    dumpEnv = Env.extend rib (dumpEnv dump)
  }

dumpCanPop :: Dump -> Bool
dumpCanPop dump = not (null (dumpOuterStack dump))

dumpPush :: Instant -> Env Addr -> Dump -> Dump
dumpPush i env dump =
  Dump env ((i, dumpEnv dump) : dumpOuterStack dump)

dumpPop :: Dump -> (Instant, Dump)
dumpPop dump =
  let ((instant, env) : stack) = dumpOuterStack dump
   in (instant, Dump env stack)

dumpExternalRib :: Dump -> [(Id, Addr)]
dumpExternalRib dump = Env.externalRib (dumpEnv dump)

----

data Value = VNone
           | VBool Bool
           | VInt Integer
           | VString String
           | VInstant Dump Instant
           | VTuple [Value]
           | VList (FL.Flist Addr)
           | VClosure String Dump [Id] Expr
           | VBuiltin BuiltinFunction
           -- Improper values
           | VPendingVar Instant Addr
           | VPendingBuiltin Instant Dump BuiltinFunction [Value]

isTruthy :: Value -> EvalM Bool
isTruthy val = do
  val' <- fullyResolveValue val
  case val' of
    VPendingVar _ _ -> failS "Truth value is pending."
    VNone           -> return False
    VBool False     -> return False
    VInt 0          -> return False
    VString ""      -> return False
    VTuple []       -> return False
    VList fl | FL.isEmpty fl
                    -> return False
    _               -> return True
  
data BuiltinFunction =
    BuiltinPrint
  | BuiltinInput
  | BuiltinNow
  | BuiltinForce
  -- Subscripting
  | BuiltinGetitem
  | BuiltinSetitem
  -- Attributes
  | BuiltinGetattr
  | BuiltinSetattr
  -- Logical
  --   NB. 'and' and 'or' are desugared to ifs by the parser
  | BuiltinNot
  -- Relational
  | BuiltinEq
  | BuiltinNe
  | BuiltinLt
  | BuiltinLe
  | BuiltinGt
  | BuiltinGe
  -- Arithmetic
  | BuiltinAdd
  | BuiltinSub
  | BuiltinMul
  | BuiltinPow
  | BuiltinDiv
  | BuiltinMod
  | BuiltinUminus
  -- Misc
  | BuiltinLen
  | BuiltinRange
  | BuiltinError

instance Show BuiltinFunction where
  show BuiltinPrint   = "print"
  show BuiltinInput   = "input"
  show BuiltinNow     = "now"
  show BuiltinForce   = "force"
  show BuiltinGetitem = "__getitem__"
  show BuiltinSetitem = "__setitem__"
  show BuiltinGetattr = "__getattr__" 
  show BuiltinSetattr = "__setattr__"
  -- Logical
  show BuiltinNot     = "__not__"
  -- Relational
  show BuiltinEq      = "__eq__"
  show BuiltinNe      = "__ne__"
  show BuiltinLt      = "__lt__"
  show BuiltinLe      = "__le__"
  show BuiltinGt      = "__gt__"
  show BuiltinGe      = "__ge__"
  -- Arithmetic
  show BuiltinAdd     = "__add__"
  show BuiltinSub     = "__sub__"
  show BuiltinMul     = "__mul__"
  show BuiltinPow     = "__pow__"
  show BuiltinDiv     = "__div__"
  show BuiltinMod     = "__mod__"
  show BuiltinUminus  = "__uminus__"
  -- Misc
  show BuiltinLen     = "len"
  show BuiltinRange   = "range"
  show BuiltinError   = "__error__"

builtinArity :: BuiltinFunction -> Int
builtinArity BuiltinPrint   = 1 -- overridden: arbitrary number
builtinArity BuiltinInput   = 0
builtinArity BuiltinNow     = 0
builtinArity BuiltinGetitem = 2
builtinArity BuiltinSetitem = 3
builtinArity BuiltinGetattr = 2
builtinArity BuiltinSetattr = 3
builtinArity BuiltinForce   = 1
-- Logical
builtinArity BuiltinNot = 1
-- Relational
builtinArity BuiltinEq = 2
builtinArity BuiltinNe = 2
builtinArity BuiltinLt = 2
builtinArity BuiltinLe = 2
builtinArity BuiltinGt = 2
builtinArity BuiltinGe = 2
-- Arithmetic
builtinArity BuiltinAdd    = 2
builtinArity BuiltinSub    = 2
builtinArity BuiltinMul    = 2
builtinArity BuiltinPow    = 2
builtinArity BuiltinDiv    = 2
builtinArity BuiltinMod    = 2
builtinArity BuiltinUminus = 1
-- Misc
builtinArity BuiltinLen    = 1
builtinArity BuiltinRange  = 3 -- overridden: up to 3
builtinArity BuiltinError  = 1

globalBuiltins :: [(String, BuiltinFunction)]
globalBuiltins = [
    ("print", BuiltinPrint)
  , ("input", BuiltinInput)
  , ("now", BuiltinNow)
  , ("force", BuiltinForce)
  , ("__getitem__", BuiltinGetitem)
  , ("__setitem__", BuiltinSetitem)
  , ("__getattr__", BuiltinGetattr)
  , ("__setattr__", BuiltinSetattr)
  -- Logical
  , ("__not__", BuiltinNot)
  -- Relational
  , ("__eq__", BuiltinEq)
  , ("__ne__", BuiltinNe)
  , ("__lt__", BuiltinLt)
  , ("__le__", BuiltinLe)
  , ("__gt__", BuiltinGt)
  , ("__ge__", BuiltinGe)
  -- Arithmetic
  , ("__add__", BuiltinAdd)
  , ("__sub__", BuiltinSub)
  , ("__mul__", BuiltinMul)
  , ("__pow__", BuiltinPow)
  , ("__div__", BuiltinDiv)
  , ("__mod__", BuiltinMod)
  , ("__uminus__", BuiltinUminus)
  -- Misc
  , ("len", BuiltinLen)
  , ("range", BuiltinRange)
  , ("__error__", BuiltinError)
  ]

isProperValue :: Value -> Bool
isProperValue (VPendingVar _ _)         = False
isProperValue (VPendingBuiltin _ _ _ _) = False
isProperValue _                         = True

isProperMValue :: Maybe Value -> Bool
isProperMValue Nothing    = False
isProperMValue (Just val) = isProperValue val

data Event = EvWrite Instant String Value String
           | EvRead Instant Addr
           | EvEvalIf Instant Addr Dump Value Expr Expr

eventWritesMemory :: Event -> Bool
eventWritesMemory (EvWrite _ _ _ _)      = False
eventWritesMemory (EvRead _ _)           = True
eventWritesMemory (EvEvalIf _ _ _ _ _ _) = True

eventInstant :: Event -> Instant
eventInstant (EvWrite i _ _ _)      = i
eventInstant (EvRead i _)           = i
eventInstant (EvEvalIf i _ _ _ _ _) = i

eventAddress :: Event -> Instant
eventAddress (EvWrite _ _ _ _)      = error "Write event has no address"
eventAddress (EvRead _ a)           = a
eventAddress (EvEvalIf _ a _ _ _ _) = a

-- Principles:
-- - Write events are postponed.
-- - Read events are postponed as far as possible
--   (but they may be triggered early if the read value is observed).

data PendingAt = PendingAt Dump Value Expr Instant

data EvalState =
  EvalState {
    sMemory               :: Memory Value
  , sEvents               :: [Event]
  , sMemoryWritingEvents  :: Set.Set (Instant, Addr)
  , sTimeTravelTargets    :: Set.Set Instant
  , sPendingAts           :: [PendingAt]
  -- Cannot produce IO events before this instant:
  , sIOBarrier            :: Instant
  -- Ranges in which assigning to a given address is forbidden:
  , sForbiddenAssignments :: Map.Map Addr [(Instant, Instant)]
  -- Values that we are currently resolving, to avoid time paradoxes
  , sCurrentlyResolving   :: Set.Set (Instant, Addr)
  }

type EvalM = SIO EvalState



--- Monad ---

liftMemory0 :: (Memory Value -> Memory Value) -> EvalM ()
liftMemory0 f = do
  mem <- f . sMemory <$> getS
  modifyS $ \ state -> state { sMemory = mem }
  return ()

liftMemory1 :: (Memory Value -> (b, Memory Value)) -> EvalM b
liftMemory1 f = do
  (b, mem) <- f . sMemory <$> getS
  modifyS $ \ state -> state { sMemory = mem }
  return b

allocateM :: EvalM Addr
allocateM = liftMemory1 Mem.allocate

insertInstantAfterM :: Instant -> EvalM Instant
insertInstantAfterM i = liftMemory1 (Mem.insertAfter i)

loadM :: Instant -> Addr -> EvalM (Maybe Value)
loadM i a = do
  memory <- sMemory <$> getS
  return $ Mem.lookup i a memory

storeOverwritingM :: Instant -> Addr -> Value -> EvalM ()
storeOverwritingM i a v = liftMemory0 (Mem.put i a v)

storeFirstTimeM :: Instant -> Addr -> Value -> EvalM ()
storeFirstTimeM i a v = do
  mVal <- loadM i a
  case mVal of
    Nothing -> storeOverwritingM i a v
    Just _  -> failS "Time paradox"

emitEventM :: Event -> EvalM ()
emitEventM event = do
  modifyS $ \ state -> state {
    sEvents = event : sEvents state
  }
  if eventWritesMemory event
   then modifyS $ \ state -> state {
          sMemoryWritingEvents =
            Set.insert (eventInstant event, eventAddress event)
                       (sMemoryWritingEvents state)
        }
   else return ()

removeEventM :: Event -> EvalM ()
removeEventM event = do
  events <- sEvents <$> getS
  case findIndex (\ e -> eventInstant e == eventInstant event) events of
    Nothing -> return ()
    Just idx ->
      modifyS $ \ state -> state {
        sEvents = take idx events ++ drop (idx + 1) events
      }
  if eventWritesMemory event
   then modifyS $ \ state -> state {
          sMemoryWritingEvents =
            Set.delete (eventInstant event, eventAddress event)
                       (sMemoryWritingEvents state)
        }
   else return ()

getTimelineM :: EvalM Timeline
getTimelineM = Mem.timeline . sMemory <$> getS

getSortedEventsM :: EvalM [Event]
getSortedEventsM = do
    events <- sEvents <$> getS
    sortedInstants <- T.instants <$> getTimelineM
    return $ sortEvents sortedInstants events

sortEvents :: [Instant] -> [Event] -> [Event]
sortEvents instants events =
  sortBy (comparing
           (\ e -> Map.findWithDefault 0 (eventInstant e) instantIndices))
         events
  where
    instantIndices = Map.fromList $ zip instants [1..]

replaceEventsM :: [Event] -> EvalM ()
replaceEventsM events = do
  modifyS $ \ state -> state { sEvents = events }

thereIsMemoryWritingEventAtM :: Instant -> Addr -> EvalM Bool
thereIsMemoryWritingEventAtM instant addr = do
  readEvents <- sMemoryWritingEvents <$> getS
  return $ Set.member (instant, addr) readEvents

createTimeTravelTargetM :: Instant -> EvalM ()
createTimeTravelTargetM instant = do
  modifyS $ \ state ->
    state { sTimeTravelTargets = Set.insert instant (sTimeTravelTargets state) } 

thereIsTimeTravelTargetAtM :: Instant -> EvalM Bool
thereIsTimeTravelTargetAtM instant =
  Set.member instant . sTimeTravelTargets <$> getS

removeTimeTravelTargetM :: Instant -> EvalM ()
removeTimeTravelTargetM instant = do
  modifyS $ \ state ->
    state { sTimeTravelTargets = Set.delete instant (sTimeTravelTargets state) } 

pushPendingAtM :: PendingAt -> EvalM ()
pushPendingAtM pa = do
  modifyS $ \ state ->
    state { sPendingAts = pa : sPendingAts state }

thereArePendingAtsM :: EvalM Bool
thereArePendingAtsM = not . null . sPendingAts <$> getS

popPendingAtM :: EvalM PendingAt
popPendingAtM = do
  state <- getS
  putS $ state { sPendingAts = tail (sPendingAts state) }
  return $ head (sPendingAts state)

precedesIOBarrierM :: Instant -> EvalM Bool
precedesIOBarrierM instant = do
  timeline <- getTimelineM
  ioBarrier <- sIOBarrier <$> getS
  return $ T.precedes instant ioBarrier timeline

checkIOBarrierM :: Instant -> EvalM ()
checkIOBarrierM instant = do
  b <- precedesIOBarrierM instant
  if b
   then failS "I/O event too far back in time."
   else return ()

forbidIOBeforeM :: Instant -> EvalM ()
forbidIOBeforeM instant = do
  timeline <- getTimelineM
  ioBarrier <- sIOBarrier <$> getS
  if T.precedes ioBarrier instant timeline
   then modifyS $ \ state -> state { sIOBarrier = instant } 
   else return ()

forbidAssignentsBetweenM :: Addr -> Instant -> Instant -> EvalM ()
forbidAssignentsBetweenM addr i0 i1 = do
  modifyS $ \ state -> state {
    sForbiddenAssignments =
      Map.insert addr
        ((i0, i1) : Map.findWithDefault [] addr (sForbiddenAssignments state))
        (sForbiddenAssignments state)
  }

instantInRangeM :: Instant -> Instant -> Instant -> EvalM Bool
instantInRangeM instant iStart iEnd = do
  timeline <- getTimelineM
  let afterStart = iStart == instant || T.precedes iStart instant timeline
  let beforeEnd  = iEnd == instant   || T.precedes instant iEnd timeline
  return (afterStart && beforeEnd)

checkAssignmentNotForbiddenM :: Addr -> Instant -> EvalM ()
checkAssignmentNotForbiddenM addr instant = do
  forbiddenRanges <- Map.findWithDefault [] addr . sForbiddenAssignments <$> getS
  bs <- mapM (\ (a, b) -> instantInRangeM instant a b) forbiddenRanges
  if or bs
   then failS "Assignment would lead to time paradox."
   else return ()

markCurrentlyResolvingM :: Instant -> Addr -> EvalM ()
markCurrentlyResolvingM i a =
  modifyS $ \ state -> state {
    sCurrentlyResolving = Set.insert (i, a) (sCurrentlyResolving state)
  }

unmarkCurrentlyResolvingM :: Instant -> Addr -> EvalM ()
unmarkCurrentlyResolvingM i a =
  modifyS $ \ state -> state {
    sCurrentlyResolving = Set.delete (i, a) (sCurrentlyResolving state)
  }

checkCurrentlyResolvingM :: Instant -> Addr -> EvalM Bool
checkCurrentlyResolvingM i a = do
  currentlyResolving <- sCurrentlyResolving <$> getS
  return $ Set.member (i, a) currentlyResolving

---

loadAddress :: Instant -> Addr -> EvalM Value
loadAddress i0 addr = do
  mVal <- Mem.lookup i0 addr . sMemory <$> getS
  case mVal of
    Nothing -> return $ VPendingVar i0 addr
    Just v  -> return $ v

showValueM :: Instant -> Value -> EvalM String
showValueM i0 val = do
    val' <- fullyResolveValue val
    rec val'
  where
    rec VNone          = return "None"
    rec (VBool b)      = return $ show b
    rec (VInt n)       = return $ show n
    rec (VString s)    = return $ show s
    rec (VInstant e i) = return ("<instant " ++ show i ++ ">")
    rec (VTuple [v])   = do
      s <- showValueM i0 v
      return ("(" ++ s ++ ",)")
    rec (VTuple vs)    = do
      ss <- mapM (showValueM i0) vs
      return ("(" ++ joinS ", " ss ++ ")")
    rec (VList addrs)  = do
      ss <- mapM (\ a -> do v <- loadAddress i0 a; showValueM i0 v)
                 (FL.toList addrs)
      return ("[" ++ joinS ", " ss ++ "]")
    rec (VClosure fnName _ _ _) = do
      return ("<function " ++ fnName ++ ">")
    rec (VBuiltin builtin) = do
      return ("<built-in function " ++ show builtin ++ ">")
    --
    rec (VPendingVar i a) =
      return ("<pending var " ++ show i ++ " " ++ show a ++ ">")
    rec (VPendingBuiltin _ _ builtin values) = do
      ss <- mapM (showValueM i0) values
      return ("<pending built-in "
              ++ show builtin
              ++ "(" ++ joinS ", " ss ++ ")"
              ++ ">")

printValue :: Instant -> Value -> EvalM ()
printValue i0 val = do
  str <- showValueM i0 val
  liftSIO $ putStr str

printValueLn :: Instant -> Value -> EvalM ()
printValueLn i0 val = do
  printValue i0 val
  liftSIO $ putStrLn ""

makeList :: [Value] -> Instant -> EvalM Value
makeList vs i0 = do
  addrs <- mapM (const allocateM) vs
  mapM_ (uncurry (storeFirstTimeM i0)) (zip addrs vs)
  return $ VList (FL.fromList addrs)

--- Evaluator ---

evalExpr :: Dump -> Expr -> Instant -> EvalM (Instant, Value)
evalExpr dump ENone i0 = return (i0, VNone)
evalExpr dump (EVar x) i0 = do
  case Env.lookup x (dumpEnv dump) of
    Nothing   -> failS ("Unbound variable: " ++ x)
    Just addr -> do
      mVal <- Mem.lookup i0 addr . sMemory <$> getS
      case mVal of
        Nothing -> return (i0, VPendingVar i0 addr)
        Just v  -> return (i0, v)
evalExpr dump (EConstBool n) i0 = return (i0, VBool n)
evalExpr dump (EConstInt n) i0 = return (i0, VInt n)
evalExpr dump (EConstString n) i0 = return (i0, VString n)
evalExpr dump (ELet x e1 e2) i0 = do
  (i1, v1) <- evalExpr dump e1 i0
  addr <- allocateM
  i2 <- insertInstantAfterM i1
  storeFirstTimeM i2 addr v1
  (i3, v2) <- evalExpr (dumpExtend1 x addr dump) e2 i2
  return (i3, v2)
evalExpr dump (EAssign (EVar x) e) i0 = do
  case Env.lookup x (dumpEnv dump) of
    Nothing   -> failS ("Unbound variable: " ++ x)
    Just addr -> do
      (i1, v1) <- evalExpr dump e i0
      i2 <- insertInstantAfterM i1
      checkAssignmentNotForbiddenM addr i2
      storeFirstTimeM i2 addr v1
      return (i2, VNone)
evalExpr _ (EAssign _ _) _ =
  failS "LHS of assignment must be a variable."
evalExpr dump (ESeq es) i0 = evalSeq dump es i0
evalExpr dump (EAt e1 e2) i0 = do
  (i1, v1) <- evalExpr dump e1 i0
  i2 <- insertInstantAfterM i1
  createTimeTravelTargetM i2
  pushPendingAtM (PendingAt dump v1 e2 i2)
  i3 <- insertInstantAfterM i2
  runPendingAtsIfPossibleM False
  return (i3, VNone)
evalExpr dump (EOuter e) i0 = do
  if dumpCanPop dump
   then do let (j1, dump') = dumpPop dump
           (j2, value) <- evalExpr dump' e j1
           i1 <- insertInstantAfterM i0
           return (i1, value)
   else failS "Cannot refer to time outside the origin."
---
evalExpr dump (ETuple es) i0 = do
  (i1, vs) <- evalMany dump es i0
  return (i1, VTuple vs)
evalExpr dump (EList es) i0 = do
  (i1, vs) <- evalMany dump es i0
  list <- makeList vs i1
  return (i1, list)
evalExpr dump (ELambda fnName xs body) i0 = do
  return (i0, VClosure fnName dump xs body)
evalExpr dump (EApp fun args) i0 = do
  (i1, vFun)  <- evalExpr dump fun i0
  (i2, vArgs) <- evalMany dump args i1
  vFun' <- fullyResolveValue vFun
  case vFun' of
    VClosure fnName dump' params body -> do
      if length params == length args
       then do addrs <- mapM (const allocateM) params
               i3 <- insertInstantAfterM i2
               mapM_ (uncurry (\ a v -> storeFirstTimeM i3 a v))
                     (zip addrs vArgs)
               i4 <- insertInstantAfterM i3
               evalExpr (dumpExtend (zip params addrs) dump') body i4
       else failS (fnName ++ " "
                   ++ "takes " ++ show (length params) ++ " arguments "
                   ++ "but " ++ show (length args) ++ " was given")
    VBuiltin builtin -> builtinApply dump builtin vArgs i2
    _ -> do
      svFun' <- showValueM i0 vFun'
      failS ("Object " ++ svFun' ++ " is not callable.")
evalExpr dump (EIf cond thenBranch elseBranch) i0 = do
  (i1, vCond) <- evalExpr dump cond i0
  mVal <- resolveValue False vCond
  if isProperMValue mVal
   then evalIf dump (fromJust mVal) thenBranch elseBranch i1
   else do
     addr <- allocateM
     emitEventM $ EvEvalIf i1 addr dump vCond thenBranch elseBranch
     i2 <- insertInstantAfterM i1
     return (i2, VPendingVar i0 addr)
evalExpr dump fullExpr@(EWhile cond body) i0 = do
  (i1, vCond) <- evalExpr dump cond i0
  b <- isTruthy vCond
  if b
   then do (i2, _) <- evalExpr dump body i1
           evalExpr dump fullExpr i2
   else return (i1, VNone)

evalIf :: Dump -> Value -> Expr -> Expr -> Instant -> EvalM (Instant, Value)
evalIf dump vCond thenBranch elseBranch i0 = do
  ss <- showValueM i0 vCond 
  b <- isTruthy vCond
  if b
   then evalExpr dump thenBranch i0
   else evalExpr dump elseBranch i0

builtinApply :: Dump -> BuiltinFunction -> [Value] -> Instant
             -> EvalM (Instant, Value)
builtinApply dump BuiltinPrint vs i0 = rec True vs i0
  where
    rec isFirst []       i0 = return (i0, VNone)
    rec isFirst (v : vs) i0 = do
      let isLast = null vs
      emitEventM $ EvWrite i0 (if isFirst then "" else " ")
                              v
                              (if isLast then "\n" else "")
      i1 <- insertInstantAfterM i0
      if isLast
       then return (i1, VNone)
       else rec False vs i1
builtinApply dump BuiltinInput [] i0 = do
  addr <- allocateM
  emitEventM $ EvRead i0 addr
  i1 <- insertInstantAfterM i0
  return (i1, VPendingVar i0 addr)
builtinApply dump BuiltinNow [] i0 = do
  createTimeTravelTargetM i0
  i1 <- insertInstantAfterM i0
  return (i1, VInstant dump i0)
builtinApply dump BuiltinForce [v] i0 = do
  v' <- fromJust <$> resolveValue True v
  return (i0, v')
builtinApply dump BuiltinGetitem [v1, v2] i0 = do
  v1' <- fromJust <$> resolveValue True v1
  v2' <- fromJust <$> resolveValue True v2
  case (v1', v2') of
    (VTuple vs, VInt i)
      | 0 <= i && i < fromIntegral (length vs)
                  -> return (i0, vs !! fromIntegral i)
      | otherwise -> failS "Tuple index out of range."
    (VString str, VInt i) -> do
      index <- case () of
                 () | 0 <= i && i < fromIntegral (length str)
                   -> return $ fromIntegral i
                 () | -fromIntegral (length str) <= i && i <= -1
                   -> return (length str + fromIntegral i)
                 _ -> failS "String index out of range."
      return (i0, VString [str !! index])
    (VList xs, VInt i) -> do
      index <- case () of
                 () | 0 <= i && i < fromIntegral (FL.length xs)
                   -> return $ fromIntegral i
                 () | -fromIntegral (FL.length xs) <= i && i <= -1
                   -> return (FL.length xs + fromIntegral i)
                 _ -> failS "List index out of range."
      v <- loadAddress i0 (xs FL.!! index)
      return (i0, v)
    _ -> failS "Object is not subscriptable."
builtinApply dump BuiltinSetitem [v1, v2, v3] i0 = do
  v1' <- fromJust <$> resolveValue True v1
  v2' <- fromJust <$> resolveValue True v2
  case (v1', v2') of
    (VList xs, VInt i) -> do
      index <- case () of
                 () | 0 <= i && i < fromIntegral (FL.length xs)
                   -> return $ fromIntegral i
                 () | -fromIntegral (FL.length xs) <= i && i <= -1
                   -> return (FL.length xs + fromIntegral i)
                 _ -> failS "List index out of range."
      let addr = xs FL.!! index
      storeOverwritingM i0 addr v3
      return (i0, VNone)
    _ -> failS "Object does not support item assignment."
builtinApply dump BuiltinError [v] i0 = do
  v' <- fullyResolveValue v
  case v' of
    VString s -> failS s
    _ -> failS "Object has no len()."
builtinApply dump builtin vArgs i =
  return (i, VPendingBuiltin i dump builtin vArgs)

evalSeq :: Dump -> [Expr] -> Instant -> EvalM (Instant, Value)
evalSeq dump es i0 = do
  (i1, vs) <- evalMany dump es i0
  return (i1, if null vs then VNone else last vs)

evalMany :: Dump -> [Expr] -> Instant -> EvalM (Instant, [Value])
evalMany dump []       i0 = return (i0, [])
evalMany dump (e : es) i0 = do
  (i1, v) <- evalExpr dump e i0
  i2 <- insertInstantAfterM i1
  (i3, vs) <- evalMany dump es i2
  return (i3, v : vs)

runPendingAtsIfPossibleM :: Bool -> EvalM ()
runPendingAtsIfPossibleM force = rec []
  where
    rec :: [PendingAt] -> EvalM ()
    rec pas = do
      b <- thereArePendingAtsM
      if b
       then do pa@(PendingAt dump val expr instant) <- popPendingAtM
               mVal <- resolveValue force val
               case mVal of
                 Nothing ->
                   if force
                    then failS "Unknown time travel destination"
                    else -- Leave this "at" pending and continue
                         rec (pa : pas)
                 Just (VInstant dump' j0) -> do
                   b <- thereIsTimeTravelTargetAtM j0 
                   if b
                    then do
                      removeTimeTravelTargetM j0
                      let dump'' = dumpPush instant (dumpEnv dump') dump
                      (_, _) <- evalExpr dump'' expr j0
                      rec pas
                    else failS "Multiple travelers to single point in time."
                 Just v1' -> if isProperValue v1'
                              then failS "'at' expects an instant"
                              else failS "Time loop."
       else mapM_ pushPendingAtM pas

evalFullProgram :: Dump -> Expr -> Instant -> EvalM (Instant, Value)
evalFullProgram dump expr i0 = do
  (i1, val) <- evalExpr dump expr i0
  runPendingAtsIfPossibleM True
  return (i1, val)

fullyResolveValue :: Value -> EvalM Value
fullyResolveValue val = do
  mVal' <- resolveValue True val
  return $ maybe val id mVal'

valueCompare :: Value -> Value -> EvalM Ordering
valueCompare VNone           VNone           = return EQ
valueCompare (VBool b1)      (VBool b2)      = return $ compare b1 b2
valueCompare (VInt n1)       (VInt n2)       = return $ compare n1 n2
valueCompare (VString s1)    (VString s2)    = return $ compare s1 s2
valueCompare (VInstant _ i1) (VInstant _ i2) =
  if i1 == i2
   then return EQ
   else do timeline <- getTimelineM
           if T.precedes i1 i2 timeline
            then return LT
            else return GT
valueCompare (VTuple vs1)    (VTuple vs2)    = rec vs1 vs2
  where
    rec []         []         = return EQ
    rec []         (_ : _)    = return LT
    rec (_ : _)    []         = return GT
    rec (v1 : vs1) (v2 : vs2) = do
      v1' <- fullyResolveValue v1
      v2' <- fullyResolveValue v2
      cmp <- valueCompare v1' v2'
      case cmp of
        LT -> return LT
        GT -> return GT
        EQ -> rec vs1 vs2
valueCompare (VBuiltin b1)   (VBuiltin b2)   =
  return $ compare (show b1) (show b2)
valueCompare _ _ = failS "Comparison not supported:"

-- Resolve a possibly improper value to a proper value.
-- This may trigger reads.
-- Reads may trigger previous events.
-- The first parameter ("goBack") is a boolean representing the ability
-- to go back in time before time travel target points to resolve the value
-- in question.
resolveValue :: Bool -> Value -> EvalM (Maybe Value)
resolveValue goBack (VPendingVar i0 addr) = do
    -- 1) Lookup in memory if this memory cell has already been defined.
    -- 2) Otherwise, check if there is a read for this instant.
    --    Then:
    --    - Trigger all I/O events up to this point.
    --    - Forbid performing further I/O events in the past beyond this point.
    -- 3) Otherwise, go back in time until the memory cell is defined.
    --    - Forbid assigning this memory cell in all of the intermediate points.
    oldState <- getS
    precedingInstants <- T.precedingInstants i0 <$> getTimelineM
    mVal <- rec goBack precedingInstants
    case mVal of
      Just val -> do
        return $ Just val
      Nothing  -> do
        putS oldState
        return Nothing
  where
    rec :: Bool -> [Instant] -> EvalM (Maybe Value)
    rec goBack []       =
      failS "Pending value cannot be resolved. Possible time paradox."
    rec goBack (i : is) = do
      --- Avoid time paradoxes
      b <- checkCurrentlyResolvingM i addr
      if b
       then failS "Time paradox: value depends on itself."
       else markCurrentlyResolvingM i addr
      ---
      b <- thereIsTimeTravelTargetAtM i
      mValue <- do
             if b && not goBack
               then return Nothing
               else do
                 mFinalValue <- do
                   mVal <- loadM i addr
                   case mVal of
                     Just val -> do
                       -- Forbid assignments between i and i0
                       forbidAssignentsBetweenM addr i i0
                       resolveValue goBack val
                     Nothing -> do
                       b <- thereIsMemoryWritingEventAtM i addr
                       if b
                        then do runEventsUntil i0
                                rec True (i : is) -- Retry
                        else rec goBack is
                 case mFinalValue of
                   Just finalValue -> do
                     storeOverwritingM i addr finalValue 
                     return $ Just finalValue
                   Nothing -> return Nothing
      -- Avoid time paradoxes
      unmarkCurrentlyResolvingM i addr
      --
      return mValue
---- Logical built-ins ----
resolveValue goBack (VPendingBuiltin _ _ BuiltinNot [v]) = do
  v' <- fullyResolveValue v
  b <- isTruthy v'
  if b
   then return . Just $ VBool False
   else return . Just $ VBool True
---- Relational built-ins ----
resolveValue goBack (VPendingBuiltin _ _ BuiltinEq [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  ord <- valueCompare v1' v2'
  return . Just . VBool $ ord == EQ
resolveValue goBack (VPendingBuiltin _ _ BuiltinNe [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  ord <- valueCompare v1' v2'
  return . Just . VBool $ ord /= EQ
resolveValue goBack (VPendingBuiltin _ _ BuiltinLe [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  ord <- valueCompare v1' v2'
  return . Just . VBool $ ord `elem` [LT, EQ]
resolveValue goBack (VPendingBuiltin _ _ BuiltinLt [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  ord <- valueCompare v1' v2'
  return . Just . VBool $ ord == LT
resolveValue goBack (VPendingBuiltin _ _ BuiltinGe [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  ord <- valueCompare v1' v2'
  return . Just . VBool $ ord `elem` [GT, EQ]
resolveValue goBack (VPendingBuiltin _ _ BuiltinGt [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  ord <- valueCompare v1' v2'
  return . Just . VBool $ ord == GT
---- Arithmetic built-ins ----
resolveValue goBack (VPendingBuiltin _ _ BuiltinAdd [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  case (v1', v2') of
    (VInt n1, VInt n2) -> return . Just $ VInt (n1 + n2)
    (VString s1, VString s2) -> return . Just $ VString (s1 ++ s2)
    (VList xs1, VList xs2) -> return . Just $ VList (xs1 FL.++ xs2)
    _ -> failS "Unsupported operand types for +"
resolveValue goBack (VPendingBuiltin _ _ BuiltinSub [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  case (v1', v2') of
    (VInt n1, VInt n2) -> return . Just $ VInt (n1 - n2)
    _ -> failS "Unsupported operand types for -"
resolveValue goBack (VPendingBuiltin _ _ BuiltinMul [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  case (v1', v2') of
    (VInt n1, VInt n2) -> return . Just $ VInt (n1 * n2)
    (VInt n, VString s) -> return . Just $ VString (concat $ replicate (fromIntegral n) s)
    (VString s, VInt n) -> return . Just $ VString (concat $ replicate (fromIntegral n) s)
    (VInt n, VList xs) -> return . Just $ VList (FL.concat $ replicate (fromIntegral n) xs)
    (VList xs, VInt n) -> return . Just $ VList (FL.concat $ replicate (fromIntegral n) xs)
    _ -> failS "Unsupported operand types for *"
resolveValue goBack (VPendingBuiltin _ _ BuiltinDiv [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  case (v1', v2') of
    (VInt n1, VInt n2) -> return . Just $ VInt (n1 `div` n2)
    _ -> failS "Unsupported operand types for /"
resolveValue goBack (VPendingBuiltin _ _ BuiltinMod [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  case (v1', v2') of
    (VInt n1, VInt n2) -> return . Just $ VInt (n1 `mod` n2)
    _ -> failS "Unsupported operand types for %"
resolveValue goBack (VPendingBuiltin _ _ BuiltinPow [v1, v2]) = do
  v1' <- fullyResolveValue v1
  v2' <- fullyResolveValue v2
  case (v1', v2') of
    (VInt n1, VInt n2) -> return . Just $ VInt (n1 ^ n2)
    _ -> failS "Unsupported operand types for **"
resolveValue goBack (VPendingBuiltin _ _ BuiltinUminus [v]) = do
  v' <- fullyResolveValue v
  case v' of
    VInt n -> return . Just $ VInt (-n)
    _ -> failS "Unsupported operand types for -"
---- Misc built-ins ----
resolveValue goBack (VPendingBuiltin _ _ BuiltinLen [v]) = do
  v' <- fullyResolveValue v
  case v' of
    VTuple vs -> return . Just $ VInt (fromIntegral (length vs))
    VList xs  -> return . Just $ VInt (fromIntegral (FL.length xs))
    VString s -> return . Just $ VInt (fromIntegral (length s))
    _ -> failS "Object has no len()."
resolveValue goBack (VPendingBuiltin i0 _ BuiltinRange [v]) = do
  v' <- fullyResolveValue v
  case v' of
    VInt n -> do
      lst <- makeList (map VInt [0..fromIntegral (n - 1)]) i0
      return . Just $ lst
    _ -> failS "Invalid argument for range()."
resolveValue goBack v@(VPendingBuiltin _ dump builtin values) =
  failS (
    "Invalid invocation to built-in operator: " ++ show builtin ++ "/" ++ show (length values)
  )
----
resolveValue _ v@VNone              = return $ Just v
resolveValue _ v@(VBool _)          = return $ Just v
resolveValue _ v@(VInt _)           = return $ Just v
resolveValue _ v@(VString _)        = return $ Just v
resolveValue _ v@(VInstant _ _)     = return $ Just v
resolveValue _ v@(VTuple _)         = return $ Just v
resolveValue _ v@(VList _)          = return $ Just v
resolveValue _ v@(VClosure _ _ _ _) = return $ Just v
resolveValue _ v@(VBuiltin _)       = return $ Just v

eval :: Expr -> IO (Either String ())
eval expr = do
  res <- evalShow expr
  case res of
    Left msg -> return (Left msg)
    Right _  -> return (Right ())

evalShow :: Expr -> IO (Either String (Value, String))
evalShow expr = flip evalS initialState $ do
    let i0 = T.start
    (i1, globalEnv) <- initializeGlobalEnv i0
    let globalDump = initialDump globalEnv
    (endInstant, value) <- evalFullProgram globalDump expr i1
    runAllPendingEvents
    str <- showValueM endInstant value
    return (value, str)
  where
    initialState :: EvalState
    initialState = EvalState {
                     sMemory               = Mem.empty
                   , sEvents               = []
                   , sMemoryWritingEvents  = Set.empty
                   , sTimeTravelTargets    = Set.empty
                   , sPendingAts           = []
                   , sIOBarrier            = T.start
                   , sForbiddenAssignments = Map.empty
                   , sCurrentlyResolving   = Set.empty
                   }

initializeGlobalEnv :: Instant -> EvalM (Instant, Env Addr)
initializeGlobalEnv i0 = do
  let names  = map fst globalBuiltins
  let values = map (VBuiltin . snd) globalBuiltins
  addrs <- mapM (const allocateM) globalBuiltins
  mapM_ (\ (a, v) -> storeFirstTimeM i0 a v) (zip addrs values)
  i1 <- insertInstantAfterM i0
  return (i1, Env.extend (zip names addrs) Env.empty)

runAllPendingEvents :: EvalM ()
runAllPendingEvents = rec
  where
    rec :: EvalM ()
    rec = do
      timeline <- getTimelineM
      sortedInstants <- T.instants <$> getTimelineM
      events <- sEvents <$> getS
      let sortedEvents = sortEvents sortedInstants events
      if null sortedEvents
       then return ()
       else do runEvent (head sortedEvents)
               rec

runEventsUntil :: Instant -> EvalM ()
runEventsUntil instant = rec
  where
    rec :: EvalM ()
    rec = do
      timeline <- getTimelineM
      sortedInstants <- T.instants <$> getTimelineM
      events <- sEvents <$> getS
      let previousEvents =
            [event | event <- events,
                     T.precedesEq (eventInstant event) instant timeline]
      let sortedPreviousEvents = sortEvents sortedInstants previousEvents
      if null sortedPreviousEvents
       then return ()
       else do runEvent (head sortedPreviousEvents)
               rec

runEvent :: Event -> EvalM ()
runEvent event = do
    removeEventM event
    checkIOBarrierM (eventInstant event)
    executeEvent event
    forbidIOBeforeM (eventInstant event)
  where
    executeEvent :: Event -> EvalM ()
    executeEvent (EvWrite i0 prefix val suffix) = do
      liftSIO $ putStr prefix
      val' <- fromJust <$> resolveValue True val
      case val' of
        VString str -> liftSIO $ putStr str
        _           -> printValue i0 val'
      liftSIO $ putStr suffix
    executeEvent (EvRead instant addr) = do
      str <- liftSIO $ getLine
      modifyS $ \ state -> state {
                             sMemory = Mem.put instant addr (VString str)
                                               (sMemory state)
                           }
    executeEvent (EvEvalIf i0 addr dump vCond thenBranch elseBranch) = do
      (i1, val) <- evalIf dump vCond thenBranch elseBranch i0
      storeFirstTimeM i0 addr val

