{- |
Transducers and their functions
-}
module Data.FST.Transducer (
  module Data.FST.TransducerTypes,

  -- * Types
  Transducer,
  TConvertable (decode, encode),

  -- * Transducer construction
  construct,

  -- * Actions on transducers
  rename,
  initial,
  transitions,
  nullFirstState,
  productT,
  unionT,
  starT,
  compositionT,
  showTransducer
  ) where

import Data.FST.TransducerTypes
import Data.FST.Utils (tagging, remove, merge)

import Data.Maybe (fromJust)
import Data.List ((\\), nub, delete)

-- | Data type for a transducer
data Transducer a = Transducer {
  stateTrans  :: TTransitionTable a,
  initS       :: InitialStates,
  finalStates :: FinalStates,
  alpha       :: Sigma a,
  firstS      :: FirstState,
  lastS       :: LastState
  } deriving (Show,Read)

instance TransducerFunctions Transducer where
  states                  = map fst . stateTrans
  isFinal a s             = s `elem` finalStates a
  initials                = initS
  finals                  = finalStates
  transitionTable         = stateTrans
  transitionList a s      = case lookup s (stateTrans a) of
                            Just xs -> xs
                            _       -> []
  transitionsU auto (s,a) = [ (c, s1)
                            | ((b, c), s1) <- transitionList auto s, a == b ]
  transitionsD auto (s,a) = [ (b, s1)
                            | ((b, c), s1) <- transitionList auto s, a == c ]
  lastState               = lastS
  firstState              = firstS
  alphabet                = alpha

-- | Initial state
initial :: Transducer a -> StateTy
initial = head . initials

-- | Set first state to null
nullFirstState :: Transducer a -> Transducer a
nullFirstState transducer = transducer { firstS = 0 }

-- | Get transition as a list of states
transitions :: Eq a => Transducer a -> (StateTy,Relation a) -> [StateTy]
transitions transducer (s,r) = 
  [ r2 | (r1, r2) <- transitionList transducer s, r == r1 ] 

-- | Construct a transducer
construct :: (StateTy, StateTy) -> TTransitionTable a -> Sigma a ->
             InitialStates -> FinalStates -> Transducer a
construct (first, last) table sigma is fs =
  Transducer {
    stateTrans  = table,
    initS       = is,
    finalStates = fs,
    firstS      = first,
    lastS       = last,
    alpha       = sigma
    }

-- | Type class TConvertable
class TConvertable f where
 encode :: Eq a => f a -> Transducer a
 decode :: Eq a => Transducer a -> f a

-- | Convert transducer labelled with something other than states to a Transducer
rename :: Eq b => [(b,[(Relation a,b)])] -> Sigma a -> [b] -> [b] ->
                                          StateTy -> Transducer a
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
              ntl = [ (a, lookupState b table) | (a, b) <- tl ] 
           in (s1,ntl):renameTable tll table

-- |
renameT :: Transducer a -> Transducer a -> (Transducer a,Transducer a,StateTy)
renameT transd1 transd2 = (transd1, tr2, lastState tr2 + 1) where
  tr2 = rename (transitionTable transd2)
        (alphabet transd2) (initials transd2)
        (finals transd2) (lastState transd1 + 1) 

-- | Product of two transducers
productT :: Eq a => Transducer a -> Transducer a -> Transducer a
productT transd1 transd2 = productT' (renameT transd1 transd2) where
  productT' (t1,t2,s) = let
    transUnion  = remove (initial t1) (transitionTable t1) ++
                  remove (initial t2) (transitionTable t2)
    transConc   = let t = (transitionList t2 (initial t2))
                  in [(f, t)| f <- finals t1]
    transInit   = [(s, transitionList t1 (initial t1) ++
                       listEps t1 (transitionList t2 (initial t2)))]
    fs  = finals t2 ++ listEps t2 (finals t1) ++
          [ s | acceptEpsilon t1 && acceptEpsilon t2]
    in Transducer {
      stateTrans  = transInit ++ merge transConc transUnion,
      finalStates = fs \\ [initial t1, initial t2],
      alpha       = nub $ alphabet t1 ++ alphabet t2,
      initS       = [s],
      firstS      = firstState t1,
      lastS       = s
      }

-- | Union of two transducers
unionT :: Eq a => Transducer a -> Transducer a -> Transducer a
unionT transducer1 transducer2 = unionT' (renameT transducer1 transducer2)
 where unionT' (t1,t2,s) =
        let transUnion  = remove (initial t1) (transitionTable t1) ++
                        remove (initial t2) (transitionTable t2)
            transInit   = [(s, transitionList t1 (initial t1) ++
                             transitionList t2 (initial t2))]
            fs  = finals t1 ++ finals t2 ++ [ s | acceptEpsilon t1 || acceptEpsilon t2 ]
         in Transducer {
          stateTrans  = transInit ++ transUnion,
          finalStates = fs \\ [initial t1, initial t2],
          alpha       = nub (alphabet t1 ++ alphabet t2),
          initS       = [s],
          firstS      = firstState t1,
          lastS       = s
          }

-- | Kleene star of two transducers
starT :: Eq a => Transducer a -> Transducer a
starT t1
 = let s = lastState t1 +1
       transUnion  = remove (initial t1) (transitionTable t1)
       transLoop   = let t = transitionList t1 (initial t1) in
                         (s,t): [(f,t) | f <- finals t1]
    in Transducer  {
     stateTrans  = merge transLoop transUnion,
     finalStates = s:(delete (initial t1) (finals t1)),
     alpha       = alphabet t1,
     initS       = [s],
     firstS      = firstState t1,
     lastS       = s
     }

-- | Compose two transducers
compositionT :: Eq a => Transducer a -> Transducer a -> Transducer a
compositionT t1 t2 =
      let minS1 = firstState t1
          minS2 = firstState t2
          name (s1,s2) = (lastState t2 - minS2 +1) *
                         (s1 - minS1) + s2 - minS2 + minS1
          nS = name (lastState t1,lastState t2) +1
          transInit = (nS, [ ((a, d), name (s1, s2))
                           | ((a, b), s1) <- ((Eps,Eps), initial t1):transitionList t1 (initial t1)
                           , ((c, d), s2) <- ((Eps,Eps), initial t2):transitionList t2 (initial t2)
                           , (a, b) /= (Eps, Eps) || (c,d) /= (Eps,Eps)
                           , b == c ])
          transTable = [(name (s1,s2),[ ((a, d), name (s3, s4))
                                      | ((a, b), s3) <- ((Eps, Eps), s1):tl1
                                      , ((c, d), s4) <- ((Eps, Eps), s2):tl2
                                      , (a, b) /= (Eps, Eps) || (c,d) /= (Eps, Eps)
                                      , b == c])
                       | (s1, tl1) <- transitionTable t1
                       , (s2, tl2) <- transitionTable t2
                       , s1 /= initial t1 || s2 /= initial t2 ]
          transUnion = transInit:transTable
          fs  = [ nS | acceptEpsilon t1 && acceptEpsilon t2 ] ++
                [name (f1, f2) | f1 <- finals t1, f2 <- finals t2]
       in Transducer {
        stateTrans  = merge [(s, []) | s <- fs] transUnion,
        finalStates = fs,
        alpha       = nub $ alphabet t1 ++ alphabet t2 ,
        initS       = [nS],
        firstS      = min (firstState t1) (firstState t2),
        lastS       = nS
        }

-- | Does a transducer accept epsilon
acceptEpsilon :: Transducer a -> Bool
acceptEpsilon transducer = isFinal transducer (initial transducer)

-- | If the transducer accepts epsilon, return second argument
listEps :: Transducer a -> [b] -> [b]
listEps transducer xs = if acceptEpsilon transducer then xs else []

-- | Show a transducer
showTransducer :: Show a => Transducer a -> String
showTransducer transducer = unlines
    [ "Transitions:"
    , aux  (stateTrans transducer)
    , "Number of States      => " ++ show (length (transitionTable transducer))
    , "Number of Transitions => " ++ show (sum [length tl | (s,tl) <- transitionTable transducer])
    , "Alphabet              => " ++ show (alphabet transducer)
    , "Initials              => " ++ show (initials transducer)
    , "Finals                => " ++ show (finals transducer)
    ]
  where aux []          = []
        aux ((s,tl):xs) = show s ++" => " ++ aux2 tl ++ "\n" ++ aux xs
        aux2 [] = []
        aux2 ((r,s):tl)  = "( " ++  showR r ++ " ," ++ show s ++") " ++ aux2 tl
        showR (S a, S b) = "(" ++ show a ++":" ++ show b ++ ")"
        showR (S a, Eps) = "(" ++ show a ++":eps)"
        showR (Eps, S b) = "(eps:" ++ show b ++ ")"
        showR (Eps, Eps) = "(eps:eps)"
