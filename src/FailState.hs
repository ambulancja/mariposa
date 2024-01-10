module FailState(
         FailState, failFS, getFS, putFS, modifyFS, logFS,
         runFS, evalFS, execFS
       ) where

import Debug.Trace(trace)

data FailState s a = FS (s -> Either String (s, a))

instance Functor (FailState s) where
  fmap f (FS ma) = FS (\ s0 ->
                     case ma s0 of
                       Left msg      -> Left msg
                       Right (s1, a) -> Right (s1, f a))

instance Applicative (FailState s) where
  pure a  = FS (\ s0 -> Right (s0, a))
  FS mf <*> FS ma = FS (\ s0 ->
                      case mf s0 of
                        Left msg      -> Left msg
                        Right (s1, f) ->
                          case ma s1 of
                            Left msg      -> Left msg
                            Right (s2, a) -> Right (s2, f a))

instance Monad (FailState s) where
  return      = pure
  FS ma >>= f = FS (\ s0 ->
                  case ma s0 of
                    Left msg      -> Left msg
                    Right (s1, a) ->
                      let FS mb = f a in mb s1)

failFS :: String -> FailState s a
failFS msg = FS (\ _ -> Left msg)

getFS :: FailState s s
getFS = FS (\ s -> Right (s, s))

modifyFS :: (s -> s) -> FailState s ()
modifyFS f = FS (\ s -> Right (f s, ()))

putFS :: s -> FailState s ()
putFS s = modifyFS (const s)

runFS :: FailState s a -> s -> Either String (s, a)
runFS (FS ma) s0 = ma s0

evalFS :: FailState s a -> s -> Either String a
evalFS fs s0 =
  case runFS fs s0 of
    Left msg     -> Left msg
    Right (_, a) -> Right a

execFS :: FailState s a -> s -> Either String s
execFS fs s0 =
  case runFS fs s0 of
    Left msg      -> Left msg
    Right (s1, _) -> Right s1

logFS :: String -> FailState s ()
logFS msg = trace msg (return ())

