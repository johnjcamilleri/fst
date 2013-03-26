{- |
Module      :  $Header$
Description :  Left-biased finite automata
Maintainer  :  Markus Forsberg

Left-biased finite automata
-}
module FST.LBFA (
  module FST.Automaton,

  -- * Types
  LBFA,

  -- * Functions on LBFA
  initial,
  compileToLBFA,
  compileToAutomaton
  ) where

import Control.Monad.State 

import FST.RegTypes
import FST.Automaton
import FST.Deterministic
import FST.Complete
import FST.Utils (remove,merge)

import Data.List (delete,nub,(\\))

-- | Data type for LBFA (left-biased finite automata)
data LBFA a = LBFA {
      trans   :: [(StateTy, Transitions a)],
      initS   :: StateTy,
      finalS  :: [StateTy],
      alpha   :: Sigma a,
      lastS   :: StateTy
    }

instance AutomatonFunctions LBFA where
  -- | Get the states of a LBFA
  states lbfa             = map fst (trans lbfa)
  -- | Check if a state is a final state.
  isFinal lbfa s          = elem s (finals lbfa)
  -- | Get the initial states of a LBFA
  initials lbfa           = [(initS lbfa)]
  -- | Get the final states of a LBFA
  finals                  = finalS
  -- | Get the transition table
  transitionTable         = trans
  -- | Get the transitions of a state
  transitionList lbfa s   = case lookup s (trans lbfa) of
                             Just tl -> tl
                             _       -> []
  -- | Get the transitions of a state and a symbol
  transitions lbfa (s, a) = [ st | (b, st) <- transitionList lbfa s, a == b ]
  -- |
  firstState              = minimum . states
  -- | Get the max state of a LBFA
  lastState               = lastS
  -- | Get the alphabet of a LBFA
  alphabet                = alpha

-- | Get the initial state of a LBFA
initial :: LBFA a -> StateTy
initial = initS

-- | Does the LBFA accept epsilon?
acceptEpsilon :: LBFA a -> Bool
acceptEpsilon lbfa = isFinal lbfa (initial lbfa)

-- | Compile a regular expression to a LBFA
compileToLBFA :: Ord a => Reg a -> Sigma a -> StateTy -> LBFA a
compileToLBFA reg sigma = evalState $ build reg $ nub $ sigma ++ symbols reg

-- | Compile a regular expression to an minimal, useful and
-- deterministic automaton, using the LBFA algorithm while building.
compileToAutomaton :: Ord a => Reg a -> Sigma a -> StateTy -> Automaton a
compileToAutomaton reg sigma s = encode (compileToLBFA reg sigma s)

fetchState :: State StateTy StateTy
fetchState = do
  state <- get
  put (state + 1)
  return state

-- | Build a LBFA from a regular expression
build :: Ord a => Reg a -> Sigma a -> State StateTy (LBFA a)
build Empty sigma = do
  s <- fetchState
  return $ LBFA {
    trans  = [(s, [])],
    initS  = s,
    finalS = [],
    alpha  = sigma,
    lastS  = s
    }
  
build Epsilon sigma = do
  s <- fetchState
  return LBFA {
    trans  = [(s, [])],
    initS  = s,
    finalS = [s],
    alpha  = sigma,
    lastS  = s
    }

build (Symbol a) sigma = do
  s1 <- fetchState
  s2 <- fetchState
  return LBFA {
    trans  = [(s1, [(a, s2)]), (s2, [])],
    initS  = s1,
    finalS = [s2],
    alpha  = sigma,
    lastS  = s2
    }

build All sigma = build (allToSymbols sigma) sigma
build (r1 :.: r2) sigma = do
  lbfa1 <- build r1 sigma
  lbfa2 <- build r2 sigma
  s <- fetchState
  let transUnion  = (remove (initial lbfa1) (trans lbfa1)) ++
                    (remove (initial lbfa2) (trans lbfa2))
      transConc   = let t = transitionList lbfa2 (initial lbfa2) in
                    [ (f,t) | f <- finals lbfa1 ]
      transInit   = [(s, transitionList lbfa1 (initial lbfa1) ++
                       listEps lbfa1 (transitionList lbfa2 (initial lbfa2)))]
      fs  = finals lbfa2 ++ listEps lbfa2 (finals lbfa1) ++
            [ s | acceptEpsilon lbfa1 && acceptEpsilon lbfa2 ]
  return $ LBFA {
    trans  = transInit ++ merge transConc transUnion,
    finalS = fs \\ [(initial lbfa1), (initial lbfa2)],
    alpha  = sigma,
    initS  = s,
    lastS   = s
    }

build (r1 :|: r2) sigma = do
  lbfa1 <- build r1 sigma
  lbfa2 <- build r2 sigma
  s <- fetchState
  let transUnion  = (remove (initial lbfa1) (trans lbfa1)) ++
                    (remove (initial lbfa2) (trans lbfa2))
      transInit   = [(s, transitionList lbfa1 (initial lbfa1) ++
                      transitionList lbfa2 (initial lbfa2))]
      fs  = finals lbfa1 ++ finals lbfa2 ++
            [ s | acceptEpsilon lbfa1 || acceptEpsilon lbfa2 ]
  return $ LBFA {
    trans  = transInit ++ transUnion,
    finalS = fs \\ [(initial lbfa1),(initial lbfa2)],
    alpha  = sigma,
    initS  = s,
    lastS  = s
    }

build (Star r1) sigma = do
  lbfa1 <- build r1 sigma
  s <- fetchState
  let transUnion  = remove (initial lbfa1) (trans lbfa1)
      transLoop   = let t = transitionList lbfa1 (initial lbfa1)
                    in (s,t) : [ (f,t) | f <- finals lbfa1 ]
  return $ LBFA {
    trans  = merge transLoop transUnion,
    finalS = s:(delete (initial lbfa1) (finals lbfa1)),
    alpha  = sigma,
    initS  = s,
    lastS  = s
    }

build (Complement r1) sigma = do
  lbfa <- build r1 sigma
  let lbfa1 = decode $ determinize $ complete $ encode lbfa
  put (lastState lbfa1 + 1)
  return $ LBFA {
    trans  = trans lbfa1,
    finalS = states lbfa1 \\ finals lbfa1,
    alpha  = sigma,
    initS  = initial lbfa1,
    lastS  = lastState lbfa1
    }

build (r1 :&: r2) sigma = do
  lbfa1 <- build r1 sigma
  lbfa2 <- build r2 sigma
  let minS1 = firstState lbfa1
      minS2 = firstState lbfa2
      name (s1,s2) = (lastState lbfa2 - minS2 +1) *
                     (s1 - minS1) + s2 - minS2 + minS1
      nS = name (lastState lbfa1,lastState lbfa2) +1
      transInit = (nS, [ (a, name (s1, s2))
                       | (a,s1) <- transitionList
                                   lbfa1 (initial lbfa1)
                       , (b,s2) <- transitionList
                                   lbfa2 (initial lbfa2)
                       , a == b])
      transTable = [(name (s1, s2),
                     [(a, name (s3, s4))
                     | (a,s3)   <- tl1
                     , (b,s4)   <- tl2, a == b ])
                   | (s1,tl1) <- trans lbfa1
                   , (s2,tl2) <- trans lbfa2
                   , s1 /= initial lbfa1 || s2 /= initial lbfa2 ]
      transUnion = transInit:transTable
      fs  = [ nS | acceptEpsilon lbfa1 && acceptEpsilon lbfa2 ]
            ++ 
            [ name (f1,f2) | f1 <- finals lbfa1, f2 <- finals lbfa2 ]
  put (nS + 1)
  return LBFA {
    trans  = merge [ (s, []) | s <- fs ] transUnion,
    finalS = fs,
    alpha  = sigma,
    initS  = nS,
    lastS  = nS
   }

instance Convertable LBFA where
  encode lbfa = construct (firstState lbfa,lastState lbfa) (trans lbfa)
                          (alphabet lbfa) (initials lbfa) (finals lbfa)
  decode auto = LBFA {
    trans  = transitionTable auto,
    initS  = head (initials auto),
    finalS = finals auto,
    alpha  = alphabet auto,
    lastS  = lastState auto
    }

instance (Eq a,Show a) => Show (LBFA a) where
 show auto = unlines [
   "Transitions:", aux (trans auto),
   "Number of States   => "   ++ show countStates,
   "Initial            => "   ++ show (initial auto),
   "Finals             => "   ++ show (finals auto)
   ] where
   aux xs      = unlines [show s ++" => " ++ show tl | (s, tl) <- xs ]
   countStates = length $ nub $ map fst (trans auto) ++ finals auto

-- | If the LBFA accepts epsilon, return second argument
listEps :: LBFA a -> [b] -> [b]
listEps lbfa xs
 | acceptEpsilon lbfa = xs
 | otherwise          = []
