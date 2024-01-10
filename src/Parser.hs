module Parser(parse) where

import qualified Data.Set as Set

import Position(unknownPosition)
import FailState(FailState, getFS, modifyFS, evalFS, failFS, logFS)
import Lexer(Token(..), TokenType(..), tokenize)
import Id(Id)
import Expr(Expr(..), assignedVars)

parse :: String -> Either String Expr
parse str =
    case tokenize str of
      Left msg   -> Left msg
      Right toks ->
        let initialState = ParserState {
                             sInput = toks
                           }
         in evalFS parseProgram initialState

type M = FailState ParserState
data ParserState =
  ParserState {
    sInput :: [Token]
  }

failM :: String -> M a
failM msg = do
  input <- sInput <$> getFS
  let position = case input of
                   []        -> unknownPosition
                   (tok : _) -> tokStartPos tok
  failFS $ unlines [
     "  Near " ++ show position
   , msg
   ]

peekM :: M TokenType
peekM = do
  input <- sInput <$> getFS
  case input of
    [] -> return TEof
    (Token _ _ tokTyp : _) -> return tokTyp

consumeM :: M ()
consumeM = do
  modifyFS $ \ state -> state {
                 sInput = case sInput state of
                            []           -> []
                            (_ : input') -> input'
               }

matchM :: TokenType -> M ()
matchM tokTyp = do
  tokTyp' <- peekM
  if tokTyp == tokTyp'
   then consumeM
   else failM $ unlines [
          "Expected: " ++ show tokTyp ++ "."
        , "Found:    " ++ show tokTyp' ++ "."
        ]

parseProgram :: M Expr
parseProgram = do
  block <- parseLocallyScopedBlock []
  tokTyp <- peekM
  case tokTyp of
    TEof -> return block
    _ -> failM ("Trailing input: " ++ show tokTyp)

parseLocallyScopedBlock :: [Id] -> M Expr
parseLocallyScopedBlock locallyBound = do
  expr <- ESeq <$> parseStmts
  let vars = assignedVars expr Set.\\ Set.fromList locallyBound
  return $ foldr (\ x -> ELet x ENone) expr vars

parseStmts :: M [Expr]
parseStmts = do
  tokTyp <- peekM
  if tokEndsBlock tokTyp
   then return []
   else do
     expr <- parseStmt
     exprs <- parseMoreStmts
     return (expr : exprs)

parseMoreStmts :: M [Expr]
parseMoreStmts = do
  tokTyp <- peekM
  case tokTyp of
    TSep -> matchM TSep
    _    -> return ()
  tokTyp <- peekM
  if tokEndsBlock tokTyp
   then return []
   else do exprs <- parseStmts
           return exprs

parseStmt :: M Expr
parseStmt = do
  tokTyp <- peekM
  case tokTyp of
    TDef   -> parseDef
    TAt    -> parseAt
    TPass  -> parsePass
    TIf    -> parseIf
    TWhile -> parseWhile
    TFor   -> parseFor
    _      -> do expr <- parseCommandExpr
                 return expr

parseColonSuite :: M Expr
parseColonSuite = do
  matchM TColon
  matchM TSep
  matchM TIndent
  suite <- ESeq <$> parseStmts
  matchM TDedent
  return suite

parsePass :: M Expr
parsePass = do
  matchM TPass
  return ENone

parseDef :: M Expr
parseDef = do
  matchM TDef
  fnName <- parseId
  params <- parseParameters
  matchM TColon
  matchM TSep
  matchM TIndent
  body <- parseLocallyScopedBlock params
  matchM TDedent
  return $ EAssign (EVar fnName) (ELambda fnName params body)

parseAt :: M Expr
parseAt = do
  matchM TAt
  EAt <$> parseOuterExpr <*> parseColonSuite

parseIf :: M Expr
parseIf = do
    matchM TIf
    cond <- parseOuterExpr
    thenBranch <- parseColonSuite
    elseBranch <- parseElseBranches
    return $ EIf cond thenBranch elseBranch
  where
    parseElseBranches = do
      tokTyp <- peekM
      case tokTyp of
        TElif -> do
          matchM TElif
          cond <- parseOuterExpr
          thenBranch <- parseColonSuite
          elseBranch <- parseElseBranches
          return $ EIf cond thenBranch elseBranch
        TElse -> do
          matchM TElse
          parseColonSuite
        _ -> return ENone

parseWhile :: M Expr
parseWhile = do
  matchM TWhile
  cond <- parseOuterExpr
  body <- parseColonSuite
  return $ EWhile cond body

parseFor :: M Expr
parseFor = do
  matchM TFor
  guard <- parseOuterExpr
  case guard of
    EApp (EVar "__in__") [lhs, rhs] -> do
      body <- parseColonSuite
      getValue <- desugarDestructuringAssignment
                    lhs
                    (EApp (EVar "__getitem__") [EVar "$seq", EVar "$i"])
      return $ ELet "$seq" rhs
             $ ELet "$i" (EConstInt 0)
             $ ELet "$n" (EApp (EVar "len") [EVar "$seq"])
             $ EWhile (EApp (EVar "__lt__") [EVar "$i", EVar "$n"])
             $ ESeq [
                 getValue
               , body
               , EAssign (EVar "$i")
                         (EApp (EVar "__add__") [EVar "$i", EConstInt 1])
               ]
    _ -> failM "for: invalid syntax"

parseOuterExpr :: M Expr
parseOuterExpr = parseTuple

tokEndsSequence :: TokenType -> Bool
tokEndsSequence TRParen = True
tokEndsSequence TRBrack = True
tokEndsSequence TRBrace = True
tokEndsSequence TSep    = True
tokEndsSequence TDedent = True
tokEndsSequence TEof    = True
tokEndsSequence TAssign = True
tokEndsSequence _       = False

tokEndsBlock :: TokenType -> Bool
tokEndsBlock TDedent = True
tokEndsBlock TEof    = True
tokEndsBlock _       = False

parseCommandExpr :: M Expr
parseCommandExpr = do
  expr1 <- parseOuterExpr
  tokTyp <- peekM
  case tokTyp of
    TAssign -> do
      matchM TAssign
      expr2 <- parseOuterExpr
      desugarDestructuringAssignment expr1 expr2
    _ -> return expr1

desugarDestructuringAssignment :: Expr -> Expr -> M Expr
desugarDestructuringAssignment lhs rhs =
  case lhs of
    EVar _ -> return $ EAssign lhs rhs
    EApp (EVar "__getitem__") [lhs', index] ->
      return $ EApp (EVar "__setitem__") [lhs', index, rhs]
    EApp (EVar "__getattr__") [lhs', attribute] ->
      return $ EApp (EVar "__setattr__") [lhs', attribute, rhs]
    ETuple ls -> do
      let n = length ls
      let indices = map fromIntegral [0..n]
      let getItem i = EApp (EVar "__getitem__") [EVar "$", EConstInt i]
      assignments <- mapM (uncurry desugarDestructuringAssignment)
                          (zip ls (map getItem indices))
      return $ ELet "$" rhs
                 (EIf (EApp (EVar "__eq__") [
                        EApp (EVar "len") [EVar "$"],
                        EConstInt (fromIntegral n)
                       ])
                   (ESeq assignments)
                   (EApp (EVar "__error__") [
                     EConstString "Mismatch in number of values to unpack."
                   ]))
    EOuter lhs' -> do
      body <- desugarDestructuringAssignment lhs' (EVar "$")
      return $ EAt (EOuter (EApp (EVar "now") []))
                   (ELet "$" (EOuter rhs) body)
    _ -> failM "LHS of assignment must be an lvalue"

parseTuple :: M Expr
parseTuple = do
  expr <- parseExpr
  tokTyp <- peekM 
  case tokTyp of
    TComma -> do
      matchM TComma
      tokTyp <- peekM 
      if tokEndsSequence tokTyp
       then return $ ETuple [expr]
       else do
         exprs <- parseExprs1
         return $ ETuple (expr : exprs)
    _ -> return expr

parseExprs1 :: M [Expr]
parseExprs1 = do
  expr <- parseExpr
  tokTyp <- peekM 
  case tokTyp of
    TComma -> do
      matchM TComma
      tokTyp <- peekM 
      if tokEndsSequence tokTyp
       then return [expr]
       else do
         exprs <- parseExprs1
         return (expr : exprs)
    _ -> return [expr]

parseExpr :: M Expr
parseExpr = parseLevelExpr precedenceTable

--------
--- Precedence table ---

data Associativity = Unary
                   | LeftAssoc
                   | RightAssoc

data Operator = Operator {
                  opTokTyp  :: TokenType
                , opBuiltin :: [Expr] -> Expr
                }

type PrecedenceTable = [(Associativity, [Operator])]

unaryBuiltin :: Id -> [Expr] -> Expr
unaryBuiltin f [e] = EApp (EVar f) [e]
unaryBuiltin _ _ = error "(Impossible)"

binaryBuiltin :: Id -> [Expr] -> Expr
binaryBuiltin f [e1, e2] = EApp (EVar f) [e1, e2]
binaryBuiltin _ _ = error "(Impossible)"

binaryBuiltinAnd :: [Expr] -> Expr
binaryBuiltinAnd [e1, e2] = ELet "$" e1 (EIf (EVar "$") e2 (EVar "$"))
binaryBuiltinAnd _ = error "(Impossible)"

binaryBuiltinOr :: [Expr] -> Expr
binaryBuiltinOr [e1, e2] = ELet "$" e1 (EIf (EVar "$") (EVar "$") e2)
binaryBuiltinOr _ = error "(Impossible)"

precedenceTable :: PrecedenceTable
precedenceTable = [
    (LeftAssoc, [
      Operator TOr binaryBuiltinOr
    ])
  , (LeftAssoc, [
      Operator TAnd binaryBuiltinAnd
    ])
  , (Unary, [
      Operator TNot (unaryBuiltin "__not__")
    ])
  , (LeftAssoc, [
      Operator TIn (binaryBuiltin "__in__")
    ])
  , (LeftAssoc, [
      Operator TEq (binaryBuiltin "__eq__")
    , Operator TNe (binaryBuiltin "__ne__")
    , Operator TLe (binaryBuiltin "__le__")
    , Operator TLt (binaryBuiltin "__lt__")
    , Operator TGe (binaryBuiltin "__ge__")
    , Operator TGt (binaryBuiltin "__gt__")
    ])
  , (LeftAssoc, [
      Operator TPlus (binaryBuiltin "__add__")
    , Operator TMinus (binaryBuiltin "__sub__")
    ])
  , (LeftAssoc, [
      Operator TMul (binaryBuiltin "__mul__")
    , Operator TDiv (binaryBuiltin "__div__")
    , Operator TMod (binaryBuiltin "__mod__")
    ])
  , (RightAssoc, [
      Operator TPow (binaryBuiltin "__pow__")
    ])
  , (Unary, [
      Operator TMinus (unaryBuiltin "__uminus__")
    ])
 ]

matchOperator :: [Operator] -> M (Maybe Operator)
matchOperator operators = do
  tokTyp <- peekM
  if tokTyp `elem` map opTokTyp operators
   then do let op = head [op | op <- operators, opTokTyp op == tokTyp]
           matchM (opTokTyp op)
           return $ Just op
   else return Nothing

parseLevelExpr :: PrecedenceTable -> M Expr
parseLevelExpr [] = parseInnerExpr
parseLevelExpr ((Unary, operators) : table) = do
  mOp <- matchOperator operators
  case mOp of
    Just op -> do expr <- parseLevelExpr table
                  return $ opBuiltin op [expr]
    Nothing -> parseLevelExpr table
parseLevelExpr fullTable@((RightAssoc, operators) : table) = do
  expr1 <- parseLevelExpr table
  mOp <- matchOperator operators
  case mOp of
    Just op -> do expr2 <- parseLevelExpr fullTable
                  return $ opBuiltin op [expr1, expr2]
    Nothing -> return expr1
parseLevelExpr ((LeftAssoc, operators) : table) = do
    expr <- parseLevelExpr table
    rec expr
  where
    rec expr1 = do
      mOp <- matchOperator operators
      case mOp of
        Just op -> do expr2 <- parseLevelExpr table
                      rec (opBuiltin op [expr1, expr2])
        Nothing -> return expr1

--------

parseInnerExpr :: M Expr
parseInnerExpr = do
    expr <- parseAtom
    rec expr
  where
    rec :: Expr -> M Expr
    rec expr = do
      tokTyp <- peekM
      case tokTyp of
        TLParen -> do
          args <- parseArguments
          rec (EApp expr args)
        TLBrack -> do
          index <- parseSlice
          rec (EApp (EVar "__getitem__") [expr, index])
        TDot -> do
          matchM TDot
          attribute <- parseId
          rec (EApp (EVar "__getattr__") [expr, EConstString attribute])
        _ -> return expr
    parseSlice :: M Expr
    parseSlice = do
      matchM TLBrack
      expr <- parseOuterExpr
      matchM TRBrack
      return expr

parseAtom :: M Expr
parseAtom = do
  tokTyp <- peekM
  case tokTyp of
    TId x -> do
      consumeM
      return $ EVar x
    TNone  -> do
      consumeM
      return $ ENone
    TTrue  -> do
      consumeM
      return $ EConstBool True
    TFalse  -> do
      consumeM
      return $ EConstBool False
    TInt n -> do
      consumeM
      return $ EConstInt n
    TString s -> do
      consumeM
      return $ EConstString s
    TOuter -> do
      matchM TOuter
      expr <- parseAtom
      return $ EOuter expr
    TLParen -> do
      matchM TLParen
      tokTyp' <- peekM
      expr <- case tokTyp' of
                TRParen -> return $ ETuple []
                _       -> parseOuterExpr
      matchM TRParen
      return expr
    TLBrack -> do
      matchM TLBrack
      exprs <- parseList
      matchM TRBrack
      return $ EList exprs
    _ -> failM ("Unrecognized expression: " ++ show tokTyp)

parseParameters :: M [Id]
parseParameters = do
    matchM TLParen
    xs <- rec
    matchM TRParen
    return xs
  where
    rec = do
      tokTyp <- peekM 
      case tokTyp of
        TRParen -> return []
        _ -> rec1
    rec1 = do
      x <- parseId
      tokTyp <- peekM 
      case tokTyp of
        TComma -> do
          matchM TComma
          xs <- rec
          return (x : xs)
        _ -> return [x]

parseArguments :: M [Expr]
parseArguments = do
    matchM TLParen
    xs <- rec
    matchM TRParen
    return xs
  where
    rec = do
      tokTyp <- peekM 
      case tokTyp of
        TRParen -> return []
        _ -> rec1
    rec1 = do
      e <- parseExpr
      tokTyp <- peekM 
      case tokTyp of
        TComma -> do
          matchM TComma
          es <- rec
          return (e : es)
        _ -> return [e]

parseList :: M [Expr]
parseList = do
  tokTyp <- peekM
  case tokTyp of
    TRBrack -> return []
    _ -> do e <- parseExpr
            tokTyp <- peekM
            case tokTyp of
              TRBrack -> return [e]
              TComma -> do
                matchM TComma
                es <- parseList
                return (e : es)
              _ -> failM "Invalid list syntax"

parseId :: M Id
parseId = do
  tokTyp <- peekM
  case tokTyp of
    TId x -> do
      consumeM
      return x
    _ -> failM "Expected an identifier."

