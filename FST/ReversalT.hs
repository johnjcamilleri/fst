{- |
Module      :  $Header$
Description :  Reverse a transducer
Maintainer  :  Markus Forsberg

Reverse an transducer
-}
module FST.ReversalT (
  reversal
  ) where

import FST.Transducer

import Data.Array

-- | Reverse a transducer
reversal :: Eq a => Transducer a -> Transducer a
reversal transducer  = reverseTrans (rename (transitionTable transducer)
                                           (alphabet transducer)
                                           (finals transducer)
                                           (initials transducer)
                                           (firstState transducer))

-- | Helper function for transducer reversal
reverseTrans :: Eq a => Transducer a -> Transducer a
reverseTrans transducer = let bs    = (firstState transducer, lastState transducer)
                              table = assocs $ accumArray (\tl1 tl2 -> tl1 ++ tl2) []
                                      bs [(s1,[(a,s)]) | (s,tl) <- transitionTable transducer,
                                                         (a,s1) <-  tl]
                          in construct bs table (alphabet transducer) (initials transducer) (finals transducer)
