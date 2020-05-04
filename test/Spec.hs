module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "Syntax of Lambda Calculus Test" [findFreshVar, findFreshVar2, findFreeVariable, findFreeVariable2, findSubTerm, findBoundVar])

-- 2. identify whether the given variable is fresh in the given term;
findFreshVar :: TestTree
findFreshVar = testCase "Fresh Variable test"
  (assertEqual "Should determine whether variable is fresh" False (isVarFresh term "z"))
  where
  term = (Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z"))))

findFreshVar2 :: TestTree
findFreshVar2 = testCase "Fresh Variable test 2"
  (assertEqual "Should determine whether variable is fres" False (isVarFresh term "x"))
  where
  term = App (Abs "y" (App (Var "x") (Var "y"))) (Abs "x" (App (Var "x") (Var "y")))

-- 3. identify whether the given variable is free in the given term;
findFreeVariable :: TestTree
findFreeVariable = testCase "Free variable test"
  (assertEqual "Should determine whether variable is free" True (isVarFree term "z"))
  where
  term = (Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z"))))

findFreeVariable2 :: TestTree
findFreeVariable2 = testCase "Free variable test 2"
  (assertEqual "Should determine whether variable is free" True (isVarFree term "x"))
  where
  term = App (Abs "y" (App (Var "x") (Var "y"))) (Abs "x" (App (Var "x") (Var "y")))

-- 4. identify whether the given term is a subterm in the another given term;
findSubTerm :: TestTree
findSubTerm = testCase "Sub Term test"
  (assertEqual "Should determine whether variable is free" True (isSubTerm term subTerm))
  where
  term = (Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "z"))))
  subTerm = (Abs "y" (App (App (Var "x") (Var "y")) (Var "z")))

-- 5. identify whether the given variable is bound in the given term.
findBoundVar :: TestTree
findBoundVar = testCase "Find Bound variable test"
  (assertEqual "Should determine whether variable is bound" True (isBound term "x"))
  where
  term = App (Abs "y" (App (Var "x") (Var "y"))) (Abs "x" (App (Var "x") (Var "y")))