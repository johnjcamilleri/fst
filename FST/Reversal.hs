{- |
Reverse an automaton
-}
module FST.Reversal (
  reversal
  ) where

import FST.Automaton

import Data.Array

-- | Reverse an automaton
reversal :: Eq a => Automaton a -> Automaton a
reversal automaton =
  reverseTrans $
  rename (transitionTable automaton)
  (alphabet automaton)
  (finals automaton)
  (initials automaton)
  (firstState automaton)

-- | Helper function for automaton reversal
reverseTrans :: Eq a => Automaton a -> Automaton a
reverseTrans automaton =
  construct bs table
  (alphabet automaton)
  (initials automaton)
  (finals automaton) where
    bs :: (StateTy, StateTy)
    bs = (firstState automaton, lastState automaton)

    table = assocs $ accumArray (++) [] bs
            [(s1,[(a,s)])
            | (s,tl) <- transitionTable automaton
            , (a,s1) <-  tl]

