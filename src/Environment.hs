module Environment(
         Env, empty, extend1, extend, Environment.lookup, externalRib
       ) where

import Id(Id)

data Env a = Empty
           | Rib [(Id, a)] (Env a)

empty :: Env a
empty = Empty

extend1 :: Id -> a -> Env a -> Env a
extend1 x value env = Rib [(x, value)] env

extend :: [(Id, a)] -> Env a -> Env a
extend rib env = Rib rib env

lookup :: Id -> Env a -> Maybe a
lookup _ Empty         = Nothing
lookup x (Rib rib env) = do
  case Prelude.lookup x rib of
    Nothing    -> Environment.lookup x env
    Just value -> Just value

externalRib :: Env a -> [(Id, a)]
externalRib Empty       = []
externalRib (Rib rib _) = rib
