{- |
API for finite state automatons
-}
module Data.FST.AutomatonInterface (
  module Data.FST.RegTypes,
  module Data.FST.AutomatonTypes,
  
  -- * Types
  Automaton,
  
  -- * Construction of automatons
  compile,
  compileNFA,
  
  -- * Transformation of automatons
  determinize,
  minimize,
  complete,
  
  -- * Query inforation about automatons
  initial,
  numberOfStates,
  numberOfTransitions,
  showAutomaton,
  ) where

import Data.FST.Automaton
import Data.FST.AutomatonTypes
import Data.FST.Complete
import qualified Data.FST.Deterministic as D
import qualified Data.FST.LBFA as L
import Data.FST.RegTypes hiding (reversal)
import Data.FST.Reversal (reversal)

-- | Compile a non-deterministic finite-state automaton
compileNFA :: Ord a => Reg a -> Sigma a -> StateTy -> Automaton a
compileNFA = L.compileToAutomaton 

-- | Minimize an automaton using the Brzozowski algorithm. Note that
-- the determinize function must construct an automaton with the
-- usefulS property.
minimize :: Ord a => Automaton a -> Automaton a
minimize = determinize . reversal . determinize . reversal
{-# SPECIALIZE minimize :: Automaton String -> Automaton String #-}

-- | Make a non-deterministic finite-state automaton deterministic
determinize :: Ord a => Automaton a -> Automaton a
determinize = D.determinize 

-- | Compile a minimized non-deterministic finite-state automaton
compile :: Ord a => Reg a -> Sigma a -> StateTy -> Automaton a
compile reg sigma s = minimize $ L.compileToAutomaton reg sigma s

-- | Get the initial state of a finite-state automaton
initial :: Automaton a -> StateTy
initial automaton = head $ initials automaton

-- | Count the number of states in a finite-state automaton
numberOfStates :: Ord a => Automaton a -> Int
numberOfStates auto = length $ states auto

-- | Count the number of transitions in a finite-state automaton
numberOfTransitions :: Ord a => Automaton a -> Int
numberOfTransitions auto = sum [length (transitionList auto s)
                               | s <- states auto]
