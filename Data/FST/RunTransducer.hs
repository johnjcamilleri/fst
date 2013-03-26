{- |
Running a transducer with some input
-}
module Data.FST.RunTransducer (
  -- * Run functions
  applyUp, applyDown
  ) where

import Data.FST.Transducer

import Data.Maybe (catMaybes)

-- | A transition betwee states in a transducer
type TransitionFunction a = (Transducer a -> (StateTy,Symbol a) ->
                                             [(Symbol a,StateTy)])

-- | Apply a transducer upwards
applyUp :: Eq a => Transducer a -> [a] -> Maybe [[a]]
applyUp transducer input
 = apply transducer transitionsD input (initial transducer) []

-- | Apply a transducer downwards
applyDown :: Eq a => Transducer a -> [a] -> Maybe [[a]]
applyDown transducer input
 = apply transducer transitionsU input (initial transducer) []

-- | Generic function for applying a transducer
apply :: Eq a => Transducer a -> TransitionFunction a -> [a] -> StateTy ->
                 [Symbol a] -> Maybe [[a]]
apply transducer transFun input s result =
 case (runEpsilon transducer transFun input s result,
       runSymbol  transducer transFun input s result) of
   (Just xs, Just ys) -> Just (xs ++ ys)
   (a,       Nothing) -> a
   (Nothing, b      ) -> b

runEpsilon :: Eq a => Transducer a -> TransitionFunction a -> [a] -> StateTy ->
                 [Symbol a] -> Maybe [[a]]
runEpsilon transducer transFun input s result =
 case transFun transducer (s, Eps) of
  [] -> Nothing
  tl -> case concat $ catMaybes $
         map (\(a,s1) -> apply transducer transFun input s1 (a:result)) tl of
         [] -> Nothing
         xs -> Just xs

runSymbol :: Eq a => Transducer a -> TransitionFunction a -> [a] -> StateTy ->
                 [Symbol a] -> Maybe [[a]]
runSymbol transducer _ [] s result
 | isFinal transducer s = Just [transform result]
 | otherwise            = Nothing
runSymbol transducer transFun (i:input) s result =
 case (transFun transducer (s,S i)) of
  [] -> Nothing
  tl -> case concat $ catMaybes $
         map (\(a,s1) -> apply transducer transFun input s1 (a:result)) tl of
         [] -> Nothing
         xs -> Just xs

transform :: [Symbol a] -> [a]
transform ys = reverse [ a | S a <- ys ]
