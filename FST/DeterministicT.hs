{- |
Function for making transducers deterministic
-}
module FST.DeterministicT (
  determinize
  ) where

import FST.Transducer

import Data.List (sort, nub)

-- | A subset is an ordered set without duplication
newtype SubSet = SubSet [StateTy]

-- | A list of subets
type SubSets = [SubSet]

-- | List of processed states
type Done = SubSets

-- | List of unprocessed states
type UnDone = SubSets

-- | Subset transitions
type SubTransitions a = [(SubSet, [(Relation a,SubSet)])]

instance Eq (SubSet) where
 (SubSet xs) == (SubSet ys) = xs == ys

sub :: [StateTy] -> SubSet
sub sts = SubSet $ sort $ nub sts

containsFinal :: Transducer a -> SubSet -> Bool
containsFinal automaton (SubSet xs) = or $ map (isFinal automaton) xs

-- | Construct a deterministic, usefulS transducer
determinize :: Ord a => Transducer a -> Transducer a
determinize automaton = let inS = sub $ initials automaton in
                            det automaton ([],[inS]) []

det :: Ord a => Transducer a -> (Done,UnDone) ->
                SubTransitions a -> Transducer a
det auto (done,[]) trans = rename (reverse trans)
                                  (alphabet auto) [sub (initials auto)]
                                  (filter (containsFinal auto) done)
                                  (firstState auto)
det auto (done,subset:undone) trans
 | elemSS done subset = det auto (done,undone) trans
 | otherwise = let (subs,nTrans) = getTransitions auto subset trans
                   nsubs         = filter (not.(elemSS (subset:done))) subs
                in det auto (subset:done,undone++nsubs) nTrans
 where elemSS subs sub = elem sub subs

getTransitions :: Ord a => Transducer a -> SubSet ->
                           SubTransitions a -> (SubSets, SubTransitions a)
getTransitions auto subset@(SubSet xs) trans
   = let tr = groupBySymbols (concat $ map (transitionList auto) xs) [] in
         (map snd tr, ((subset,tr):trans))

groupBySymbols :: Eq a => [(a,StateTy)] -> [(a,[StateTy])] -> [(a,SubSet)]
groupBySymbols []         tr = map (\(a,xs) -> (a,sub xs)) tr
groupBySymbols ((a,s):xs) tr = groupBySymbols xs (ins (a,s) tr)
 where ins (a1,s1) [] = [(a1,[s1])]
       ins (a1,s1) ((b,ys):zs)
        | a1 == b    = (b,s1:ys):zs
        | otherwise = (b,ys): ins (a,s) zs
