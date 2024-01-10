module Memory(
         Addr, Memory,
         empty, timeline,
         insertAfter, insertAfter',
         insertAtEnd, insertAtEnd',
         allocate,
         put, Memory.lookup, findWithDefault
       ) where

import qualified Data.Map as Map

import Timeline(Timeline, Instant)
import qualified Timeline as T

type Addr = Integer

data Memory a = Memory {
                  mTimeline :: Timeline
                , mStore    :: Map.Map (Instant, Addr) a
                , mNextFree :: Addr
                }

empty :: Memory a
empty = Memory {
          mTimeline = T.empty
        , mStore    = Map.empty
        , mNextFree = 0
        }

allocate :: Memory a -> (Addr, Memory a)
allocate memory =
  let addr    = mNextFree memory
      memory' = memory { mNextFree = addr + 1 }
   in (addr, memory')

timeline :: Memory a -> Timeline
timeline = mTimeline

insertAfter :: Instant -> Memory a -> (Instant, Memory a)
insertAfter instant memory =
  let timeline              = mTimeline memory
      (instant', timeline') = T.insertAfter instant timeline
      memory'               = memory { mTimeline = timeline' }
   in (instant', memory')

insertAfter' :: Instant -> Memory a -> Memory a
insertAfter' instant memory = snd (insertAfter instant memory)

insertAtEnd :: Memory a -> (Instant, Memory a)
insertAtEnd memory =
  let timeline              = mTimeline memory
      (instant', timeline') = T.insertAtEnd timeline
      memory'               = memory { mTimeline = timeline' }
   in (instant', memory')

insertAtEnd' :: Memory a -> Memory a
insertAtEnd' memory = snd (insertAtEnd memory)

put :: Instant -> Addr -> a -> Memory a -> Memory a
put instant addr value memory =
  memory {
    mStore = Map.insert (instant, addr) value (mStore memory)
  }

lookup :: Instant -> Addr -> Memory a -> Maybe a
lookup instant addr memory = Map.lookup (instant, addr) (mStore memory)

findWithDefault :: a -> Instant -> Addr -> Memory a -> a
findWithDefault defaultValue instant addr memory =
  case Memory.lookup instant addr memory of
    Nothing    -> defaultValue
    Just value -> value

