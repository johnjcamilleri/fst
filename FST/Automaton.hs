{- |
Finite state automatons
-}
module FST.Automaton (
  module FST.AutomatonTypes,
  
  -- * Types
  Automaton,
  Convertable (decode, encode),
  
  -- * Automaton construction
  construct,
  
  -- * Actions on automatons
  rename,
  showAutomaton,
  
  ) where

import FST.AutomatonTypes
import FST.Utils (tagging)
import Data.Maybe (fromJust, maybeToList)

-- | Data type for an automaton
data Automaton a = Automaton {
  stateTrans     :: TransitionTable a,
  initialStates  :: InitialStates,
  finalStates    :: FinalStates,
  alpha          :: Sigma a,
  firstS         :: FirstState,
  lastS          :: LastState
  } deriving (Show,Read)

-- | Construct an automaton
construct :: (FirstState,LastState) -> TransitionTable a ->
             Sigma a -> InitialStates -> FinalStates -> Automaton a
construct bs table sigma inits fs = Automaton {
  stateTrans    = table,
  initialStates = inits,
  finalStates   = fs,
  alpha         = sigma,
  firstS        = fst bs,
  lastS         = snd bs
  }

-- | Instance of AutomatonFunctions
instance AutomatonFunctions Automaton where
 states                 = map fst . stateTrans
 isFinal auto s         = s `elem` finalStates auto
 initials               = initialStates
 finals                 = finalStates
 transitionTable        = stateTrans
 transitionList auto s  = maybe [] id (lookup s (stateTrans auto))
 transitions auto (s,a) = [ st | (b, st) <- transitionList auto s, b == a ]
 firstState             = firstS
 lastState              = lastS
 alphabet               = alpha

-- | Convert automaton labelled with something other than
--   states to an 'Automaton'.
rename :: Eq b => [(b,[(a,b)])] -> Sigma a -> [b] -> [b] ->
                                         StateTy -> Automaton a
rename tTable sigma initS fs s
  = let (maxS, table) = tagging (map fst tTable) s
        nI            = map (`lookupState` table) initS
        nfs           = map (`lookupState` table) fs
        nTrans        = renameTable tTable table
     in construct (s, maxS) nTrans sigma nI nfs
 where lookupState st tab = fromJust (lookup st tab)
       renameTable [] _ = []
       renameTable ((b,tl):tll) table
        = let s1  = lookupState b table
              ntl = map (\(a, b1) -> (a, lookupState b1 table)) tl
           in (s1, ntl):renameTable tll table

-- | Type class for conversion to/from an automaton
class Convertable f where
  encode :: Eq a => f a -> Automaton a -- ^ From an automaton to an structure
  decode :: Eq a => Automaton a -> f a -- ^ From a structure to an Automaton

-- | Display the automaton
showAutomaton :: Show a => Automaton a -> String
showAutomaton auto = unlines
  [ "Transitions:"
  , aux  (stateTrans auto)
  , "Number of States   => " ++ show (length (stateTrans auto))
  , "Initials           => " ++ show (initials auto)
  , "Finals             => " ++ show (finals auto)
  ]
  where
    aux tr = unlines [ show s ++ " => " ++ show tl | (s, tl) <- tr ]
