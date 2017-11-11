{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- List.hs
module List where

import Data.List
import Data.Typeable

import Test.QuickSpec hiding (lists, bools, arith)

type Var k a = (Typeable a, Arbitrary a, CoArbitrary a, k a)

listCons :: forall a . Var Ord a => a -> Sig
listCons a = background [
  "[]" `fun0` ([] :: [a]),
  ":" `fun2` ((:) :: a -> [a] -> [a])
  ]

lists :: forall a . Var Ord a => a -> [Sig]
lists a = [
  funs',
  funvars',
  vars',

  listCons a,

  "sort" `fun1` (sort :: [a] -> [a]),
  "map" `fun2` (map :: (a -> a) -> [a] -> [a]),
  "id" `fun1` (id :: [a] -> [a]),
  "reverse" `fun1` (reverse :: [a] -> [a]),
  "minimum" `fun1` (minimum :: [a] -> a),
  "length" `fun1` (length :: [a] -> Int),
  "++" `fun2` ((++) :: [a] -> [a] -> [a])
  ] where
  funs' = funs (undefined :: a)
  funVars' = vars ["f", "g", "h"] (undefined :: a -> a)
  vars' = ["xs", "ys", "zs"] vars' (undefined :: [a])

tvar :: A
tvar = undefined

main :: IO ()
main = quickSpec (lists tvar)
