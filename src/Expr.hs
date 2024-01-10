
module Expr(Expr(..), assignedVars) where

import qualified Data.Set as S

import Id(Id)

data Expr = ENone
          | EVar Id
          | EConstBool Bool
          | EConstInt Integer
          | EConstString String
          | ELet Id Expr Expr
          | EAssign Expr Expr
          | ESeq [Expr]
          | EAt Expr Expr
          | EOuter Expr
          | ETuple [Expr]
          | EList [Expr]
          | ELambda String [Id] Expr
          | EApp Expr [Expr]
          | EIf Expr Expr Expr
          | EWhile Expr Expr
  deriving Show

lvalueVars :: Expr -> S.Set Id
lvalueVars (EVar x)    = S.singleton x
lvalueVars (ETuple es) = S.unions $ map lvalueVars es
lvalueVars (EList es)  = S.unions $ map lvalueVars es
lvalueVars _           = S.empty

assignedVars :: Expr -> S.Set Id
assignedVars ENone              = S.empty
assignedVars (EVar x)           = S.empty
assignedVars (EConstBool _)     = S.empty
assignedVars (EConstInt _)      = S.empty
assignedVars (EConstString _)   = S.empty
assignedVars (ELet x e1 e2)     = assignedVars e1
                        `S.union` (assignedVars e2 S.\\ S.singleton x)
assignedVars (EAssign e1 e2)    = lvalueVars e1
                        `S.union` assignedVars e1
                        `S.union` assignedVars e2
assignedVars (ESeq es)          = S.unions $ map assignedVars es
assignedVars (EAt e1 e2)        = assignedVars e1 `S.union` assignedVars e2
assignedVars (EOuter e)         = assignedVars e
assignedVars (ETuple es)        = S.unions $ map assignedVars es
assignedVars (EList es)         = S.unions $ map assignedVars es
assignedVars (ELambda _ xs e)   = assignedVars e S.\\ S.fromList xs
assignedVars (EApp e es)        = assignedVars e
                        `S.union` S.unions (map assignedVars es)
assignedVars (EIf e1 e2 e3)     = assignedVars e1
                        `S.union` assignedVars e2
                        `S.union` assignedVars e3
assignedVars (EWhile e1 e2)     = assignedVars e1 `S.union` assignedVars e2

