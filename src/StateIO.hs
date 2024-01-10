module StateIO(
         SIO, failS, getS, putS, modifyS,
         liftSIO, runS, execS, evalS, logS
       ) where

import System.IO(hPutStrLn, stderr)
import System.Exit(exitFailure)
import Debug.Trace(trace)

data SIO s a = S (s -> IO (s, Either String a))

instance Functor (SIO s) where
  fmap f (S siea) =
    S $ \ s0 -> do (s1, ea) <- siea s0
                   case ea of
                     Left msg -> return (s1, Left msg)
                     Right a  -> return (s1, Right (f a))

instance Applicative (SIO s) where
  pure a = S $ \ s0 -> return (s0, Right a)
  S sief <*> S siea =
    S $ \ s0 -> do
      (s1, ef) <- sief s0
      case ef of
        Left msg -> return (s1, Left msg)
        Right f  -> do
          (s2, ea) <- siea s1
          case ea of
            Left msg -> return (s1, Left msg)
            Right a  -> return (s2, Right (f a))

instance Monad (SIO s) where
  return = pure
  S siea >>= f =
    S $ \ s0 -> do
      (s1, ea) <- siea s0
      case ea of
        Left msg -> return (s1, Left msg)
        Right a  -> let S sieb = f a in
                      sieb s1

liftSIO :: IO a -> SIO s a
liftSIO ma = S $ \ s0 -> do a <- ma
                            return (s0, Right a)

failS :: String -> SIO s a
failS msg = S $ \ s0 -> return (s0, Left msg)

getS :: SIO s s
getS = S $ \ s -> return (s, Right s)

modifyS :: (s -> s) -> SIO s ()
modifyS f = S $ \ s -> return (f s, Right ())

putS :: s -> SIO s ()
putS s = modifyS (const s)

runS :: SIO s a -> s -> IO (s, Either String a)
runS (S ma) s0 = ma s0

execS :: SIO s a -> s -> IO s
execS fs s0 = fst <$> runS fs s0

evalS :: SIO s a -> s -> IO (Either String a)
evalS fs s0 = do
  (_, ea) <- runS fs s0
  return ea

logS :: String -> SIO s ()
logS msg = trace msg (return ())

