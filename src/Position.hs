module Position(
         Position, posLine, posColumn,
         startingPosition, posAfterChar, unknownPosition
       ) where

data Position =
  Position {
    posFilename :: String
  , posLine     :: Int
  , posColumn   :: Int
  }

instance Show Position where
  show pos = posFilename pos
             ++ ":" ++ show (posLine pos)
             ++ ":" ++ show (posColumn pos)

startingPosition :: String -> Position
startingPosition filename =
  Position {
    posFilename = "..."
  , posLine     = 1
  , posColumn   = 1
  }

posAfterChar :: Char -> Position -> Position
posAfterChar '\n' pos = pos { posLine = 1 + posLine pos, posColumn = 1 }
posAfterChar _    pos = pos { posColumn = 1 + posColumn pos }

unknownPosition :: Position
unknownPosition = Position "..." 0 0
