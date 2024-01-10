module Lexer(Token(..), TokenType(..), tokenize) where

----

import qualified Data.Map as Map
import Data.Map(Map)

import Data.List(isPrefixOf)

----

import FailState(FailState, getFS, modifyFS, failFS, evalFS, logFS)

import Id(Id) 
import Position(
         Position, posLine, posColumn,
         startingPosition, posAfterChar, unknownPosition
       )

----

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isIdent :: Char -> Bool
isIdent c = c == '_' || isDigit c || isAlpha c

----

data Token = Token {
               tokStartPos :: Position
             , tokEndPos   :: Position
             , tokTyp      :: TokenType
             }

instance Show Token where
  --show (Token startPos _ tokTyp) = show tokTyp ++ "@" ++ show startPos
  show (Token _ _ tokTyp) = show tokTyp

data TokenType =
    TEof
  | TId Id
  | TInt Integer
  | TString String
  -- Keywords
  | TDef
  | TAt
  | TPass
  | TIf
  | TElif
  | TElse
  | TWhile
  | TBreak
  | TContinue
  | TFor
  | TIn
  | TAnd
  | TOr
  | TNot
  | TReturn
  -- Keywords (constants)
  | TTrue
  | TFalse
  | TNone
  -- Reserved symbols
  | TEq
  | TNe
  | TLe
  | TLt
  | TGe
  | TGt
  | TAssign
  | TPlus
  | TMinus
  | TMul
  | TPow
  | TDiv
  | TMod
  | TComma
  | TDot
  | TColon
  | TOuter
  | TLParen
  | TRParen
  | TLBrack
  | TRBrack
  | TLBrace
  | TRBrace
  -- Indentation
  | TIndent
  | TDedent
  | TSep
  deriving (Eq, Show)

type M = FailState LexerState

data LexerState =
  LexerState {
    sPosition :: Position
  }

tokenize :: String -> Either String [Token]
tokenize string =
    case evalFS (tokenize' string) initialState of
      Left msg   -> Left msg
      Right toks -> evalFS (layout toks) ()
  where
    initialState = 
      LexerState {
        sPosition = startingPosition "..."
      }

consumeCharM :: Char -> M ()
consumeCharM c = do
  modifyFS $ \ state ->
    state { sPosition = posAfterChar c (sPosition state) }

consumeStringM :: String -> M ()
consumeStringM string = mapM_ consumeCharM string

getPosM :: M Position
getPosM = sPosition <$> getFS

failM :: String -> M a
failM msg = do
  pos  <- getPosM
  failFS $ unlines [
     "Lexer error"
   , "  Near: " ++ show pos
   , msg
   ]

---- Keywords ----

keywords :: Map String TokenType
keywords =
  Map.fromList [
    ("def", TDef)
  , ("at", TAt)
  , ("pass", TPass)
  , ("if", TIf)
  , ("elif", TElif)
  , ("else", TElse)
  , ("while", TWhile)
  , ("break", TBreak)
  , ("continue", TContinue)
  , ("for", TFor)
  , ("in", TIn)
  , ("and", TAnd)
  , ("or", TOr)
  , ("not", TNot)
  , ("return", TReturn)
  , ("None", TNone)
  , ("True", TTrue)
  , ("False", TFalse)
  ]

---- Reserved symbols ----

reservedSymbols :: [(String, TokenType)]
reservedSymbols = [
    ("==", TEq)
  , ("!=", TNe)
  , ("<=", TLe)
  , ("<", TLt)
  , (">=", TGe)
  , (">", TGt)
  , (",", TComma)
  , (".", TDot)
  , (":", TColon)
  , ("$", TOuter)
  , ("=", TAssign)
  , ("+", TPlus)
  , ("-", TMinus)
  , ("**", TPow)
  , ("*", TMul)
  , ("/", TDiv)
  , ("%", TMod)
  , ("(", TLParen)
  , (")", TRParen)
  , ("[", TLBrack)
  , ("]", TRBrack)
  , ("{", TLBrace)
  , ("}", TRBrace)
  ]

matchReservedSymbol :: String -> Maybe (String, TokenType, String)
matchReservedSymbol xs =
  let m = [(symbol, tokTyp, drop (length symbol) xs)
          | (symbol, tokTyp) <- reservedSymbols, isPrefixOf symbol xs]
   in if null m
       then Nothing
       else Just (head m)

matchesReservedSymbol :: String -> Bool
matchesReservedSymbol xs = maybe False (const True) (matchReservedSymbol xs)

-- Precondition: the string matches a reserved symbol
theReservedSymbol :: String -> (String, TokenType, String)
theReservedSymbol xs = maybe (error "No match") id (matchReservedSymbol xs)

----

tokenize' :: String -> M [Token]
tokenize' []           = return []
tokenize' ('\t' : xs)  = failM "Tabs are not allowed in source. Use spaces."
tokenize' (' ' : xs)   = do consumeCharM ' ' ; tokenize' xs
tokenize' ('\r' : xs)  = do consumeCharM '\r'; tokenize' xs
tokenize' ('\n' : xs)  = do consumeCharM '\n'; tokenize' xs
tokenize' xs@('#' : _) = let (comment, xs') = span (/= '\n') xs
                           in do consumeStringM comment
                                 tokenize' xs'
tokenize' xs@(d : _) | isDigit d = do
  start <- getPosM
  let (number, xs') = span isDigit xs
   in do consumeStringM number
         end <- getPosM
         (:) (Token start end (TInt (read number))) <$> tokenize' xs'
tokenize' xs@(c : _) | isIdent c = do
  start <- getPosM
  let (id, xs') = span isIdent xs
   in do consumeStringM id
         end <- getPosM
         let tokTyp = Map.findWithDefault (TId id) id keywords
         (:) (Token start end tokTyp) <$> tokenize' xs'
tokenize' xs | matchesReservedSymbol xs = do
  start <- getPosM
  let (symbol, tokenTyp, xs') = theReservedSymbol xs
  consumeStringM symbol
  end <- getPosM
  (:) (Token start end tokenTyp) <$> tokenize' xs'
tokenize' (d1 : d2 : d3 : xs)
  | [d1, d2, d3] == "\"\"\"" || [d1, d2, d3] == "'''"  = do
  -- Multiline string
  start <- getPosM
  consumeStringM [d1, d2, d3]
  (str, xs') <- readString True d1 xs
  end <- getPosM
  (:) (Token start end (TString str)) <$> tokenize' xs'
tokenize' (delim : xs) | delim == '"' || delim == '\'' = do
  -- Single-line string
  start <- getPosM
  consumeCharM delim
  (str, xs') <- readString False delim xs
  end <- getPosM
  (:) (Token start end (TString str)) <$> tokenize' xs'
tokenize' _ = failFS "Unrecognized input."

readString :: Bool -> Char -> String -> M (String, String)
readString _         _     [] =
  failM "EOF while scanning string literal"
-- Closing
readString False delim (delim' : rest)
  | delim == delim' = do
  consumeCharM delim
  return ("", rest)
readString True delim (d1 : d2 : d3 : rest)
  | [d1, d2, d3] == [delim, delim, delim] = do
  consumeStringM [delim, delim, delim]
  return ("", rest)
-- End of line
readString False     _     ('\n' : rest) =
  failM "EOL while scanning string literal"
-- Hex escape
readString multiLine delim ('\\' : 'x' : c1 : c2 : rest)
  | isHex c1 && isHex c2 = do
    consumeStringM ['\\', 'x', c1, c2]
    (str, rest') <- readString multiLine delim rest
    let chr = toEnum (16 * hexValue c1 + hexValue c2)
    return (chr : str, rest')
  where
    hex = Map.fromList [
            ('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5),
            ('6', 6), ('7', 7), ('8', 8), ('9', 9), ('a', 10), ('b', 11),
            ('c', 12), ('d', 13), ('e', 14), ('f', 15), ('A', 10), ('B', 11),
            ('C', 12), ('D', 13), ('E', 14), ('F', 15)
          ]
    isHex c = Map.member c hex
    hexValue c = Map.findWithDefault undefined c hex
-- Other escapes
readString multiLine delim ('\\' : chr : rest) | Map.member chr escapes = do
    consumeStringM ['\\', chr]
    (str, rest') <- readString multiLine delim rest
    let escapedChr = Map.findWithDefault undefined chr escapes
    return (escapedChr : str, rest')
  where
    escapes = Map.fromList [
                ('"', '"')
              , ('\'', '\'')
              , ('\\', '\\')
              , ('a', '\a')
              , ('b', '\b')
              , ('f', '\f')
              , ('n', '\n')
              , ('r', '\r')
              , ('v', '\v')
              , ('t', '\t')
              ]
-- Normal path
readString multiLine delim (chr : rest) = do
  consumeCharM chr
  (str, rest') <- readString multiLine delim rest
  return (chr : str, rest')

----

layout :: [Token] -> FailState () [Token]
layout []   = return []
layout toks = let pos = tokStartPos (head toks) in
                rec pos [] [posColumn pos] toks
  where
    isOpeningTok :: Token -> Bool
    isOpeningTok (Token _ _ TLParen) = True
    isOpeningTok (Token _ _ TLBrack) = True
    isOpeningTok (Token _ _ TLBrace) = True
    isOpeningTok _                   = False

    isClosingTok :: Token -> Bool
    isClosingTok (Token _ _ TRParen) = True
    isClosingTok (Token _ _ TRBrack) = True
    isClosingTok (Token _ _ TRBrace) = True
    isClosingTok _                   = False

    closeTok :: Token -> [Token] -> FailState () [Token]
    closeTok (Token _ _ TRParen) (Token _ _ TLParen : s) = return s
    closeTok (Token _ _ TRBrack) (Token _ _ TLBrack : s) = return s
    closeTok (Token _ _ TRBrace) (Token _ _ TLBrace : s) = return s
    closeTok _ _ = failFS "Closing delimiter does not match opening delimiter."

    rec :: Position -> [Token] -> [Int] -> [Token] -> FailState () [Token]
    rec lastPos parenStack levelStack [] =
      let mkTok       = Token lastPos lastPos in
        if null parenStack
         then return $ map (\ _ -> mkTok TDedent) (init levelStack)
         else failFS $ unlines ([
                "Premature EOF:"
              ] ++ [
                "  Open " ++ show tok ++ " near " ++ show (tokStartPos tok) ++ "."
                | tok <- parenStack
              ])
    rec lastPos [] levelStack (tok : toks) = do
      let prevLine    = posLine lastPos
          prevCol     = head levelStack
          newLine     = posLine (tokStartPos tok)
          newCol      = posColumn (tokStartPos tok)
          parenStack' = if isOpeningTok tok then [tok] else []
          mkTok       = Token lastPos lastPos
       in if newLine <= prevLine
           then (:) tok <$> rec (tokStartPos tok) parenStack' levelStack toks
           else do
             toks <-
               if newCol > prevCol
                then (++) [mkTok TIndent, tok]
                     <$> rec (tokStartPos tok) parenStack' (newCol : levelStack) toks
                else let (closedLevels, levelStack') = span (> newCol) levelStack
                      in
                        (++) ([mkTok TDedent | i <- closedLevels] ++ [tok])
                        <$> rec (tokStartPos tok) parenStack' levelStack' toks
             return $ [mkTok TSep] ++ toks
    rec lastPos parenStack levelStack (tok : toks) =
      (:) tok <$> 
      if isOpeningTok tok
       then rec (tokStartPos tok) (tok : parenStack) levelStack toks
       else
         if isClosingTok tok
          then do parenStack' <- closeTok tok parenStack
                  rec (tokStartPos tok) parenStack' levelStack toks
          else rec (tokStartPos tok) parenStack levelStack toks

----

example :: Either String [Token]
example = tokenize $ unlines [
            "def fact(x):"
          , "  if (x == 0"
          , "      and y == 3):"
          , "    return 1"
          , "  else:"
          , "    return x * fact(x - 1)"
          ]

