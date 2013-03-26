{- |
Module      :  $Header$
Description :  Function for making automatons deterministic
Maintainer  :  Markus Forsberg

Function for making automatons deterministic
-}
module FST.Deterministic (
  determinize
  ) where

import FST.Automaton

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
type SubTransitions a = [(SubSet, [(a, SubSet)])]

instance Eq (SubSet) where
 SubSet xs == SubSet ys = xs == ys

sub :: [StateTy] -> SubSet
sub sts = SubSet $ sort $ nub sts

containsFinal :: Automaton a -> SubSet -> Bool
containsFinal automaton (SubSet xs) = any (isFinal automaton) xs

-- | Make an automaton deterministic and usefulS
determinize :: Ord a => Automaton a -> Automaton a
determinize automaton = let inS = sub $ initials automaton
                        in det automaton ([],[inS]) []

det :: Ord a => Automaton a -> (Done,UnDone) ->
                SubTransitions a -> Automaton a
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

getTransitions :: Ord a => Automaton a -> SubSet ->
                           SubTransitions a -> (SubSets, SubTransitions a)
getTransitions auto subset@(SubSet xs) trans
   = let tr = groupBySymbols (concat $ map (transitionList auto) xs) []
     in (map snd tr, (subset,tr):trans)

groupBySymbols :: Eq a => [(a,StateTy)] -> [(a,[StateTy])] -> [(a,SubSet)]
groupBySymbols []         tr = map (\(a,xs) -> (a,sub xs)) tr
groupBySymbols ((a,s):xs) tr = groupBySymbols xs (ins (a,s) tr)
 where ins (a1,s1) [] = [(a1,[s1])]
       ins (a1,s1) ((b,ys):zs)
        | a1 == b    = (b,s1:ys):zs
        | otherwise = (b,ys): ins (a,s) zs
