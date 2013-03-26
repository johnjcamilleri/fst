{- |
Module      :  $Header$
Description :  Functions for constructing a simplified regular expression.
Maintainer  :  Markus Forsberg

Functions for constructing a simplified regular expression.
-}
module FST.RegTypes (
  -- * Type classes
  Combinators (
    (<|>), (|>), star, plus, empty
  ),
  Symbols (symbols),
  
  -- * Types
  Reg(..),
  
  -- * Combinators
  (<&>), (<->),
  complement, reversal, allFree,
  
  -- * Constructors
  s, eps, allS,
  allToSymbols,
    
  -- * Query functions
  acceptEps,
  ) where

import Data.List (nub)

-- | Data type for a regular expression.
data Reg a =
    Empty              -- ^ [ ]
  | Epsilon            -- ^ 0
  | All                -- ^ ?
  | Symbol a           -- ^ a
  | Reg a :|: Reg a    -- ^ [ r1 | r2 ]
  | Reg a :.: Reg a    -- ^ [ r1 r2 ]
  | Reg a :&: Reg a    -- ^ [ r1 & r2 ]
  | Complement (Reg a) -- ^ ~[ r1 ]
  | Star       (Reg a) -- ^ [ r2 ]*
  deriving (Eq)

infixl 4 <|>  -- Union
infixl 5  |>  -- Concatenation
infixl 3 <&>  -- Intersection
infixl 3 <->  -- Set minus

-- | Combinators. The regular expressions are simplified while combined.
class Combinators a where
  (<|>) :: a -> a -> a -- ^ Union
  (|>)  :: a -> a -> a -- ^ Concatenation
  star  :: a -> a      -- ^ Kleene's star
  plus  :: a -> a      -- ^ Kleene's plus
  empty :: a           -- ^ Empty language

instance Eq a => Combinators (Reg a) where
  Empty    <|> b        = b                    -- [ [] | r1 ] = r1
  a        <|> Empty    = a                    -- [ r1 | [] ] = r1
  _        <|> Star All = Star All
  Star All <|> _        = Star All

  a@(a1 :.: a2) <|> b@(b1 :.: b2)
    | a  == b   = a
    | a1 == b1  = a1          |> (a2 <|> b2)
    | a2 == b2  = (a1 <|> b1) |> a2
    | otherwise = a :|: b 
  a <|> b = if a == b then a else a :|: b -- [ r1 | r1 ] = r1

  Empty   |> _       = empty               -- [ [] r1 ] = []
  _       |> Empty   = empty               -- [ r1 [] ] = []
  Epsilon |> b       = b                   -- [ 0 r1 ]  = r1
  a       |> Epsilon = a                   -- [ r1 0 ]  = r1
  a       |> b       = a :.: b

  star (Star a)  = star a            -- [r1]**  = [r1]*
  star (Epsilon) = eps               -- [0]*    = 0
  star (Empty)   = eps               -- [ [] ]* = 0
  star a         = Star a

  plus a         = a |> star a

  empty = Empty

-- | Intersection 
(<&>) :: Eq a => Reg a -> Reg a -> Reg a
_        <&> Empty    = Empty                 -- [ r1 & [] ] = []
Empty    <&> _        = Empty                 -- [ [] & r1 ] = []
Star All <&> a        = a
a        <&> Star All = a
a        <&> b
 | a == b    = a                    -- [ r1 & r1 ] = r1
 | otherwise = a :&: b

-- | Minus. Definition A - B = A & ~B 
(<->) :: Eq a => Reg a -> Reg a -> Reg a
Empty <-> _ = empty                 -- [ [] - r1 ] = []
a <-> Empty = a                     -- [ r1 - [] ] = r1
a <-> b
 | a == b    = empty                -- [ r1 - r1 ] = []
 | otherwise = a <&> (complement b)

-- | Symbol
s :: a -> Reg a
s a = Symbol a
 
-- | Epsilon
eps :: Reg a
eps = Epsilon

-- | All symbol
allS :: Reg a
allS = All

-- | Complement
complement :: Eq a => Reg a -> Reg a
complement Empty          = star allS       -- ~[ [] ] = ?*
complement Epsilon        = plus allS       -- ~[ 0 ] = [? ?*]
complement (Star All)     = empty
complement (Complement a) = a
complement a              = Complement a

-- | Transform the 'all' symbol to union over alphabet. ? -> [a|..] with respect to an alphabet [a]
allToSymbols :: Eq a => [a] -> Reg a
allToSymbols [] = empty
allToSymbols ys = foldr1 (:|:) (map s ys)

-- | Construct a ?-free regular expression with respect to an alphabet [a]
allFree :: Eq a => Reg a -> [a] -> Reg a
allFree (a :|: b)      sigma  = allFree a sigma :|: allFree b sigma
allFree (a :.: b)      sigma  = allFree a sigma :.: allFree b sigma
allFree (a :&: b)      sigma  = allFree a sigma :&: allFree b sigma
allFree (Complement a) sigma  = Complement (allFree a sigma)
allFree (Star a)       sigma  = Star       (allFree a sigma)
allFree All            sigma  = allToSymbols sigma
allFree r              _      = r

-- | Reverse the language denoted by the regular expression.
reversal :: Eq a => Reg a -> Reg a
reversal (a :|: b)      = reversal a :|: reversal b
reversal (a :.: b)      = reversal b :.: reversal a
reversal (a :&: b)      = reversal a :&: reversal b
reversal (Complement a) = Complement (reversal a)
reversal (Star a)       = Star (reversal a)
reversal r              = r

-- | Examines if a regular expression accepts the empty string.
acceptEps :: Eq a => Reg a -> Bool
acceptEps Epsilon        = True
acceptEps (Star _)       = True
acceptEps (a :|: b)      = acceptEps a || acceptEps b
acceptEps (a :.: b)      = acceptEps a && acceptEps b
acceptEps (a :&: b)      = acceptEps a && acceptEps b
acceptEps (Complement a) = not (acceptEps a)
acceptEps _              = False

-- | Type class for the collection of symbols in an expression.
class Symbols f where
  symbols :: Eq a => f a -> [a] -- ^ Collect the symbols in a regular expression.

instance Symbols Reg where
  symbols (Symbol a)     = [a]
  symbols (a :.: b)      = nub (symbols a ++ symbols b)
  symbols (a :|: b)      = nub (symbols a ++ symbols b)
  symbols (a :&: b)      = nub (symbols a ++ symbols b)
  symbols (Complement a) = symbols a
  symbols (Star a)       = symbols a
  symbols _              = []

instance Show a => Show (Reg a) where
  show Empty          = "[0 - 0]"
  show Epsilon        = "0"
  show (Symbol a)     = show a
  show All            = "?"
  show (Complement a) = "~[" ++ show a ++ "]"
  show (Star a)       =  "[" ++ show a ++ "]* "
  show (a :|: b)      =  "[" ++ show a ++ " | " ++ show b ++ "]"
  show (a :.: b)      =  "[" ++ show a ++ " "   ++ show b ++ "]"
  show (a :&: b)      =  "[" ++ show a ++ " & " ++ show b ++ "]"
