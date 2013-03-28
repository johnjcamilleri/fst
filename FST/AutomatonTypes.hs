{- |
Types for Automaton
-}
module FST.AutomatonTypes (

  -- * Types
  StateTy, FirstState, LastState, InitialStates, FinalStates, 
  Transitions, TransitionTable,
  Sigma,

  -- * Type class
  AutomatonFunctions (..)
  ) where

-- | A state
type StateTy = Int

-- | First state
type FirstState = Int

-- | Last state
type LastState = Int

-- | Initial states
type InitialStates = [StateTy]

-- | Final states
type FinalStates = [StateTy]

-- | Transitions
type Transitions a = [(a, StateTy)]

-- | Table of transitions
type TransitionTable a = [(StateTy, Transitions a)]

-- | The alphabet of an automaton
type Sigma a = [a]

-- | Class of AutomatonFunctions
class AutomatonFunctions f where
  -- | Get the states of an automaton
  states          :: f a -> [StateTy]
  -- | Is the given state a final state?
  isFinal         :: f a -> StateTy -> Bool
  -- | Get the final states of an automaton
  finals          :: f a -> FinalStates
  -- | Get the initial states of an automaton
  initials        :: f a -> InitialStates
  -- | Get the transitions w.r.t. a state
  transitionList  :: f a -> StateTy -> Transitions a
  -- | Get the transitionTable
  transitionTable :: f a -> TransitionTable a
  -- | Get the transitions  w.r.t. a state and a symbol
  transitions     :: Eq a => f a -> (StateTy, a) -> [StateTy]
  -- | Get the first state of a automaton
  firstState      :: Eq a => f a -> StateTy
  -- | Get the last state of a automaton
  lastState       :: Eq a => f a -> StateTy
  -- | Get the alphabet of an automaton
  alphabet        :: f a -> Sigma a

