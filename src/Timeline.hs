module Timeline(
         Timeline, Instant, start,
         empty,
         insertAfter, insertAfter',
         insertAtEnd, insertAtEnd',
         instants, precedingInstants, precedes, precedesEq
       ) where

import Data.List(findIndex)

type Instant  = Integer

data Timeline = Timeline {
                  tNextFreshInstant :: Instant
                , tInstants         :: [Instant]
                }

start :: Instant
start = 0

empty :: Timeline
empty = Timeline {
          tNextFreshInstant = start + 1
        , tInstants         = [start]
        }

insertAfter :: Instant -> Timeline -> (Instant, Timeline)
insertAfter instant timeline =
  let newInstant = tNextFreshInstant timeline
      index      = maybe 0 id (findIndex (== instant) (tInstants timeline))
      prev       = take (1 + index) (tInstants timeline)
      next       = drop (1 + index) (tInstants timeline)
      timeline'  = Timeline {
                     tNextFreshInstant = 1 + tNextFreshInstant timeline
                   , tInstants         = prev ++ [newInstant] ++ next
                   }
   in (newInstant, timeline')

insertAtEnd :: Timeline -> (Instant, Timeline)
insertAtEnd timeline = insertAfter (last (tInstants timeline)) timeline

insertAfter' :: Instant -> Timeline -> Timeline
insertAfter' instant timeline = snd (insertAfter instant timeline)

insertAtEnd' :: Timeline -> Timeline
insertAtEnd' timeline = snd (insertAtEnd timeline)

instants :: Timeline -> [Instant]
instants = tInstants

-- Given an instant i,
-- return a list of all the preceding instants [i0, i1, ..., iN]
-- where i0 = i
-- and instants are ordered "backwards" in time,
-- i.e. i_j follows i_{j+1}.
precedingInstants :: Instant -> Timeline -> [Instant]
precedingInstants instant timeline =
  case findIndex (== instant) (tInstants timeline) of
    Nothing -> error "Timeline.precedingInstants: Not an instant"
    Just i  -> reverse $ take (i + 1) (tInstants timeline)

precedes :: Instant -> Instant -> Timeline -> Bool
precedes i1 i2 timeline = idxOf i1 < idxOf i2
  where
    idxOf i = maybe 0 id (findIndex (== i) (tInstants timeline))

precedesEq :: Instant -> Instant -> Timeline -> Bool
precedesEq i1 i2 timeline = i1 == i2 || precedes i1 i2 timeline

