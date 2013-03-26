{- |
Functions for constructing a simplified regular relation.
-}
module Data.FST.RRegTypes ( 
  module Data.FST.RegTypes,
  -- * Types
  RReg(..),
  -- * Combinators
  (<*>), (<.>),
  -- * Constructors
  idR, r,
  ) where

import Data.FST.RegTypes
import Data.FST.TransducerTypes (Symbol(..))

import Data.List (nub)

-- | Datatype for a regular relations
data RReg a =
    Cross    (Reg a)    (Reg a)        -- ^ Cross product     
  | Comp     (RReg a)   (RReg a)       -- ^ Composition       
  | ProductR (RReg a)   (RReg a)       -- ^ Concatenation     
  | UnionR   (RReg a)   (RReg a)       -- ^ Union             
  | StarR    (RReg a)                  -- ^ Kleene star       
  | Identity (Reg a)                   -- ^ Identity relation 
  | Relation (Symbol a) (Symbol a)     -- ^ (a:b)             
  | EmptyR                             -- ^ Empty language    
  deriving (Eq)

instance Eq a => Combinators (RReg a) where
  -- Union
  EmptyR <|> r2     = r2                                    -- [ r1 | [] ] = r1
  r1     <|> EmptyR = r1                                    -- [ [] | r2 ] = r2
  r1     <|> r2     = if r1 == r2 then r1 else UnionR r1 r2 -- [ r1 | r1 ] = r1

  -- Concatenation
  EmptyR  |> _      = EmptyR  -- [ [] r2 ] = []
  _       |> EmptyR = EmptyR  -- [ r1 [] ] = []
  r1      |> r2     = ProductR r1 r2

  -- Kleene's star
  star (StarR r1)   = star r1 -- [ r1* ]* = r1*
  star r1           = StarR r1

  -- Kleene's plus
  plus r1           = r1 |> star r1
  empty             = EmptyR

infixl 2 <*>
infixl 1 <.>

-- | Cross product operator
(<*>) :: Eq a => Reg a -> Reg a -> RReg a
(<*>) = Cross

-- | Composition operator
(<.>) :: Eq a => RReg a -> RReg a -> RReg a
(<.>) = Comp

-- | Identity relation
idR :: Eq a => Reg a -> RReg a
idR = Identity

-- | Relation
r :: Eq a => a -> a -> RReg a
r a b = Relation (S a) (S b)

instance Symbols RReg where
  symbols (Cross r1 r2)    = nub $ symbols r1 ++ symbols r2
  symbols (Comp r1 r2)     = nub $ symbols r1 ++ symbols r2
  symbols (ProductR r1 r2) = nub $ symbols r1 ++ symbols r2
  symbols (UnionR r1 r2)   = nub $ symbols r1 ++ symbols r2
  symbols (StarR r1)       = symbols r1
  symbols (Identity r1)    = symbols r1
  symbols (Relation a b)   = let sym (S c) = [c]
                                 sym  _    = []
                             in nub $ sym a ++ sym b
  symbols _                = []

instance Show a => Show (RReg a) where
 show (Cross r1 r2)    = "[ " ++ show r1 ++ " .x. " ++ show r2 ++ " ]"
 show (Comp r1 r2)     = "[ " ++ show r1 ++ " .o. " ++ show r2 ++ " ]"
 show (UnionR r1 r2)   = "[ " ++ show r1 ++ " | " ++ show r2 ++ " ]"
 show (ProductR r1 r2) = "[ " ++ show r1 ++ " " ++ show r2 ++ " ]"
 show (Identity r)     = show r
 show (StarR r)        = "[ " ++ show r ++ " ]*"
 show (Relation a b)   = "[ " ++ show a ++":"++show b ++" ]"
 show EmptyR           = "[]"
