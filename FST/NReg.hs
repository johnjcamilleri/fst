{- |
Module      :  $Header$
Description :  Neutral regular expressions
Maintainer  :  Markus Forsberg

Neutral regular expressions
-}
module FST.NReg (
  -- * Types
  NReg(..),

  -- * Conversion functions
  toRReg,
  toReg,
  nVarToSymbol
  ) where

import Control.Monad
import FST.RegTypes
import FST.RRegTypes

-- | Neutral regular expressions
data NReg a = NCross      (NReg a) (NReg a)
            | NComp       (NReg a) (NReg a)
            | NUnion      (NReg a) (NReg a)
            | NProduct    (NReg a) (NReg a)
            | NIntersect  (NReg a) (NReg a)
            | NStar       (NReg a)
            | NComplement (NReg a)
            | NSymbol a
            | NRelation a a
            | NEpsilon
            | NEmptySet
            | NVar String
            | Fun String [NReg a]
            | NAll

-- | If possible, build a regular expression instead of a regular relation
toRReg :: Eq a => NReg a -> Maybe (RReg a)
toRReg reg = maybe (nRReg reg) (return . idR) (toReg reg)
 where
   nRReg :: Eq a => NReg a -> Maybe (RReg a)
   nRReg NEmptySet          = Just EmptyR
   nRReg (NRelation a b)    = Just (r a b)
   nRReg (NComp n1 n2)      = liftM2 (<.>) (toRReg n1) (toRReg n2)
   nRReg (NCross n1 n2)     = liftM2 (<*>) (toReg n1)  (toReg n2)
   nRReg (NUnion n1 n2)     = case (toRReg n1, toRReg n2) of
     (Just r1, Just r2) -> Just (r1 <|> r2)
     _                  -> fmap idR $ liftM2 (<|>) (toReg n1) (toReg n2)
   nRReg (NProduct n1 n2)   = case (toRReg n1, toRReg n2) of
     (Just r1,Just r2) -> Just (r1 |> r2)
     _                 -> fmap idR $ liftM2 (|>) (toReg n1) (toReg n2)
   nRReg (NStar n1)         = case toRReg n1 of
     Just r1 -> Just (star r1)
     _       -> liftM (idR . star) (toReg n1)
   nRReg (NIntersect n1 n2) = fmap idR $ liftM2 (<&>) (toReg n1) (toReg n2)
   nRReg (NComplement n1)   = fmap (idR . complement) (toReg n1)
   nRReg _                  = Nothing

-- | If possible, converts NReg to Reg
toReg :: Eq a => NReg a -> Maybe (Reg a)
toReg NEmptySet           = return empty
toReg NEpsilon            = return eps
toReg NAll                = return allS
toReg (NSymbol a)         = return (s a)
toReg (NStar n1)          = liftM  star       (toReg n1)
toReg (NComplement n1)    = liftM  complement (toReg n1)
toReg (NUnion n1 n2)      = liftM2 (<|>) (toReg n1) (toReg n2)
toReg (NIntersect n1 n2)  = liftM2 (<&>) (toReg n1) (toReg n2)
toReg (NProduct n1 n2)    = liftM2 (|>)  (toReg n1) (toReg n2)
toReg  _                  = Nothing

-- | Convert variables to symbols
nVarToSymbol :: NReg String -> NReg String
nVarToSymbol (NCross n1 n2)     = NCross      (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NComp n1 n2)      = NComp       (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NUnion n1 n2)     = NUnion      (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NProduct n1 n2)   = NProduct    (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NStar n1)         = NStar       (nVarToSymbol n1)
nVarToSymbol (NIntersect n1 n2) = NIntersect  (nVarToSymbol n1) (nVarToSymbol n2)
nVarToSymbol (NComplement n1)   = NComplement (nVarToSymbol n1)
nVarToSymbol (NVar str)         = NSymbol str
nVarToSymbol n1                 = n1
