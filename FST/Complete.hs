{-# LANGUAGE TupleSections #-}

{- |
Module      :  $Header$
Description :  Function for making an automaton complete
Maintainer  :  Markus Forsberg

Function for making an automaton complete (transition on every symbol at every state)
-}
module FST.Complete (
  complete
  ) where

import FST.Automaton
import Data.List ( (\\) )

-- | Make a automaton complete (transition on every symbol at every state)
complete :: Eq a => Automaton a -> Automaton a
complete auto =
  construct (firstState auto, sink) newTrans
            (alphabet auto) (initials auto)
            (finals auto) where
    sink     = lastState auto + 1
    sinkTr   = (sink, map (,sink) (alphabet auto))
    newTrans = sinkTr:completeStates auto sink (states auto) []

completeStates :: Eq a => Automaton a -> StateTy -> [StateTy] -> [(StateTy,Transitions a)] -> [(StateTy,Transitions a)]
completeStates _    _    []      trans = trans
completeStates auto sink (state:states) trans
 = completeStates auto sink states ((state, tr ++ nTr):trans)
  where
    tr  = transitionList auto state
    nTr = map (,sink) (alphabet auto \\ map fst tr)
