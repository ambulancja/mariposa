
module Flist(
         Flist, empty, isEmpty, toList, fromList,
         (Flist.!!), (Flist.++), Flist.concat, Flist.length
       ) where

import qualified Data.Map as M

data Flist a = Flist {
                 len   :: Int
               , elems :: M.Map Int a
               }

empty :: Flist a
empty = Flist 0 M.empty

length :: Flist a -> Int
length = len

isEmpty :: Flist a -> Bool
isEmpty fl = len fl == 0

toList :: Flist a -> [a]
toList fl = map snd (M.toList (elems fl))

fromList :: [a] -> Flist a
fromList xs = Flist (Prelude.length xs) (M.fromList (zip [0..] xs))

(!!) :: Flist a -> Int -> a
fl !! i = M.findWithDefault (error "Flist index out of range") i (elems fl)

snoc :: Flist a -> a -> Flist a
snoc fl x = Flist (len fl + 1) (M.insert (len fl) x (elems fl))

(++) :: Flist a -> Flist a -> Flist a
fl1 ++ fl2 = foldl snoc fl1 (toList fl2)

concat :: [Flist a] -> Flist a
concat = foldl (Flist.++) empty

