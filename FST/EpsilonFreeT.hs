{- |
Function for constructing an epsilon-free transducer
-}
module FST.EpsilonFreeT (
  epsilonfree
  ) where

import FST.Transducer
import Data.List (partition)

-- | Construct an epsilon-free, usefulS transducer
epsilonfree :: Eq a => Transducer a -> Transducer a
epsilonfree transducer
 = epsFree transducer ([],initials transducer) [] []

epsFree :: Eq a => Transducer a -> ([StateTy],[StateTy]) -> FinalStates ->
                   [(StateTy,[(Relation a,StateTy)])] -> Transducer a
epsFree transducer (_,[]) fs table
 = construct (firstState transducer, lastState transducer)
             table (alphabet transducer) (initials transducer) fs
epsFree transducer (done,(s:undone)) fs table
 = let (newtl, fsB) = stateEpsRemove [] (transitionList transducer s)
                                        ([], False)
       newSts = filter (`notElem` s:done) (map snd newtl)
    in epsFree transducer (s:done, newSts ++ undone)
       (if fsB || isFinal transducer s then s:fs else fs)
       ((s,newtl):table)
 where
   epsTransitions :: Eq a => ((Symbol a, Symbol a), t) -> Bool
   epsTransitions (eps, _) = eps == (Eps, Eps)
   
   stateEpsRemove _       []    (tl, fsB) = (tl,fsB)
   stateEpsRemove history tlist (tl, fsB)
    = case partition epsTransitions tlist of
        ([],    ntl) -> (tl ++ ntl, fsB)
        (epstl, ntl) -> let newSts = filter (`notElem` history) (map snd epstl)
                            fsBnew = any (isFinal transducer) newSts
                        in stateEpsRemove (newSts ++ history)
                           (concatMap (transitionList transducer) newSts)
                           (ntl ++ tl, fsB || fsBnew)
