module Lib where
import Data.List

-- | Datatype for representing variables and terms
data Term = Var String | Abs String (Term) | App (Term) (Term)
    deriving (Eq, Show)

-- | Identifies whether the given variable is fresh in the given term
isVarFresh :: Term -> String -> Bool
isVarFresh (term) x = notElem x (flatMapVars term)

flatMapVars :: Term -> [String]
flatMapVars (Var x) = [x]
flatMapVars (Abs x t) = flatMapVars t ++ [x]
flatMapVars (App t1 t2) = flatMapVars t1 ++ flatMapVars t2

-- | Identifies whether the given variable is free in the given term
isVarFree :: Term -> String -> Bool
isVarFree (term) x = elem x (freeVars term)

freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (Abs x t) = freeVars t \\ [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

-- | Identifies whether the given term is a subterm in the another given term
isSubTerm :: Term -> Term -> Bool
isSubTerm (term) (subTerm) | term == subTerm = True
isSubTerm (Var x) (subTerm) = Var x == subTerm
isSubTerm (Abs _ t) (subTerm) = isSubTerm t subTerm
isSubTerm (App t1 t2) (subTerm) = isSubTerm t1 subTerm || isSubTerm t2 subTerm

-- | Identifies whether the given variable is bound in the given term
isBound :: Term -> String -> Bool
isBound (Var _) (_) = False
isBound (Abs x t) varName = (varName == x && isBoundInner t varName) || isBound t varName
isBound (App t1 t2) varName = isBound t1 varName || isBound t2 varName

isBoundInner :: Term -> String -> Bool
isBoundInner (Var x) varName = x == varName
isBoundInner (App t1 t2) varName = isBoundInner t1 varName || isBoundInner t2 varName
isBoundInner _ _ = False