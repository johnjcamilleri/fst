{- |
Type system for transducers
-}
module FST.TransducerTypes (

  -- * Types
  StateTy,
  FinalStates,
  FirstState,
  LastState,
  Sigma,
  Relation,
  Upper,
  Lower,
  Symbol (..),
  TTransitions,
  TTransitionTable,
  InitialStates,
  TransducerFunctions (..),
  ) where

import FST.AutomatonTypes (
  StateTy, FinalStates, Sigma, FirstState, LastState, InitialStates
  )

-- | A relation between upper/lower languages
type Relation a = (Upper a, Lower a)

-- | Upper language
type Upper a = Symbol a

-- | Lower language
type Lower a = Symbol a

-- | A symbol
data Symbol a = S a | Eps
    deriving (Show, Read, Eq)

-- | Transducer transitions
type TTransitions a = [(Relation a, StateTy)]

-- | Transducer transition table
type TTransitionTable a = [(StateTy, [(Relation a, StateTy)])]

-- | Class of TransducerFunctions
class TransducerFunctions f where
  states          :: f a -> [StateTy]
  isFinal         :: f a -> StateTy -> Bool
  initials        :: f a -> InitialStates
  finals          :: f a -> FinalStates
  transitionTable :: f a -> TTransitionTable a
  transitionList  :: f a -> StateTy -> TTransitions a
  transitionsU    :: Eq a => f a -> (StateTy, Symbol a) -> [(Symbol a, StateTy)]
  transitionsD    :: Eq a => f a -> (StateTy, Symbol a) -> [(Symbol a, StateTy)]
  firstState      :: f a -> StateTy
  lastState       :: f a -> StateTy
  alphabet        :: f a -> Sigma a
