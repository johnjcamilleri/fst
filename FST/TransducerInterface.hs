{-# LANGUAGE ScopedTypeVariables #-}
{- |
Main API for finite-state transducer library.
Importing this module gives you access to the folllowing functions.

/Regular expressions/

Functions for constructing a simplified regular expression.

> s          :: a -> Reg a              -- symbol
> eps        :: Reg a                   -- epsilon
> empty      :: Reg a                   -- empty set
> allS       :: Reg a                   -- all symbol
> star       :: Reg a -> Reg a          -- kleene’s star
> plus       :: Reg a -> Reg a          -- kleene’s plus
> complement :: Reg a -> Reg a          -- complement
> (<|>)      :: Reg a -> Reg a -> Reg a -- union
> (|>)       :: Reg a -> Reg a -> Reg a -- product
> (<&>)      :: Reg a -> Reg a -> Reg a -- intersection
> (<->)      :: Reg a -> Reg a -> Reg a -- set minus
> symbols    :: Reg a -> a              -- collect all symbols.

/Regular relations/

Functions for constructing a simplified regular relation.

> r       :: a -> a -> Reg a            -- relation
> empty   :: RReg a                     -- empty set
> idR     :: Reg a -> RReg a            -- identity
> star    :: RReg a -> RReg a           -- kleene’s star
> plus    :: RReg a -> RReg a           -- kleene’s plus
> (<|>)   :: RReg a -> RReg a -> RReg a -- union
> (|>)    :: RReg a -> RReg a -> RReg a -- product
> (<*>)   :: Reg a -> Reg a -> RReg a   -- cross product
> (<.>)   :: RReg a -> RReg a -> RReg a -- composition
> symbols :: RReg a -> a                -- collect all symbols

/Parsing regular relations/

Functions for parsing regular relations.

'parseProgram' takes a string containing a fstStudio program, and try
to parse it - if unsuccessful, it returns a error message. 'parseExp' parses a
string containing a regular relation.

> parseProgram :: String -> Either String (RReg String)
> parseExp     :: String -> Either String (RReg String)

/Construction and running/

Functions for constructing and running a nite state transducer.
The function 'compile' construct a deterministic, epsilonfree, minimal
transducer, and 'compileN' construct a epsilonfree, possibly non-deterministic,
non-minimal transducer. The 'Sigma' type provides a way to add symbols
that is not present in the regular relation. 'applyDown' and 'applyUp' are
used to run the transducer.

> type Sigma a = [a]
>
> compile         :: Ord a => RReg a -> Sigma a -> Transducer a
> compileN        :: Ord a => RReg a -> Sigma a -> Transducer a
> determinize     :: Ord a => Transducer a -> Transducer a
> minimize        :: Ord a => Transducer a -> Transducer a
> unionT          :: Ord a => Transducer a -> Transducer a -> Transducer a
> productT        :: Ord a => Transducer a -> Transducer a -> Transducer a
> starT           :: Ord a => Transducer a -> Transducer a
> compositionT    :: Ord a => Transducer a -> Transducer a -> Transducer a
> emptyTransducer :: Transducer a
> applyDown       :: Ord a => Transducer a -> [a] -> Maybe [[a]]
> applyUp         :: Ord a => Transducer a -> [a] -> Maybe [[a]]
> load            :: FilePath -> IO (Either String (Transducer String))
> save            :: FilePath -> Transducer String -> IO (Either String ())

/Transducer Information/

Functions for getting information about a built transducer.

type StateTy = Int

> states              :: Transducer a -> [StateTy]
> isFinal             :: Transducer a -> StateTy -> Bool
> initial             :: Transducer a -> StateTy
> finals              :: Transducer a -> [StateTy]
> transitonsU         :: Transducer a -> (StateTy,a) -> [(a,StateTy)]
> transitionsD        :: Transducer a -> (StateTy,a) -> [(a,StateTy)]
> showTransducer      :: Transducer a -> String
> numberOfStates      :: Transducer a -> Int
> numberOfTransitions :: Transducer a -> Int

-}
module FST.TransducerInterface (
  -- * Functions on regular expressions and relations
  module FST.RRegTypes,

  -- * Types
  Transducer,

  -- * Transducer-building functions
  compile, compileN, minimize, determinize,
  emptyTransducer,
  
  -- * Query functions on transducer
  numberOfStates, numberOfTransitions,
  transitions, showTransducer,

  -- * Transducer combinators
  unionT, productT, starT, compositionT,

  -- * File IO functions
  load, save, open, saveToFile,
  
  -- * Parse functions
  parseProgram, parseExp,

  -- * Run functions
  applyUp, applyDown,
  ) where

import Prelude hiding (catch)
import FST.Parse (parseProgram, parseExp) 
import FST.RRegTypes hiding (reversal)
import FST.RunTransducer
import FST.Transducer
import FST.TransducerTypes
import qualified FST.DeterministicT as D
import qualified FST.LBFT as L
import FST.ReversalT

import Control.Exception (IOException, catch, try)
import Control.Monad.Except

-- | Construct a deterministic, epsilon-free, minimal transducer
compile :: Ord a => RReg a -> Sigma a -> Transducer a
compile rreg sigma = minimize $ nullFirstState $ L.compileToTransducer rreg sigma

-- | Construct an epsilon-free, possibly non-deterministic, non-minimal transducer
compileN :: Ord a => RReg a -> Sigma a -> Transducer a
compileN = L.compileToTransducer 

-- | Make a transducer deterministic
determinize :: Ord a => Transducer a -> Transducer a
determinize = D.determinize 

-- | Make a transducer minimal
minimize :: Ord a => Transducer a -> Transducer a
minimize = D.determinize . reversal . D.determinize . reversal
{-# SPECIALIZE minimize :: Transducer String -> Transducer String #-}

-- | Return the number of states in a transducer
numberOfStates :: Ord a => Transducer a -> Int
numberOfStates = length . states

-- | Return the number of transitions in a transducer
numberOfTransitions :: Ord a => Transducer a -> Int
numberOfTransitions transducer = sum [ length (transitionList transducer s)
                                     | s <- states transducer]

-- | Load a transducer from file
load :: FilePath -> ExceptT String IO (Transducer String)
load = fmap read . open

-- | Save a transducer from file
save :: FilePath -> Transducer String -> ExceptT String IO ()
save file auto = saveToFile file (show auto)

-- | Open a file and return contents as string
open :: FilePath -> ExceptT String IO String
open file = ExceptT $ catch 
  (Right `fmap` readFile file) 
  (\(e :: IOException) -> return $ throwError $ "Error: Unable to open \"" ++ file ++ "\"")

-- | Save contents (as string) to a file
saveToFile :: FilePath -> String -> ExceptT String IO ()
saveToFile file str = ExceptT $ catch 
  (Right `fmap` writeFile file str) 
  (\(e :: IOException) -> return $ throwError $ "Error: Unable to save to \"" ++ file ++ "\"")

-- | The empty transucer
emptyTransducer :: Ord a => Transducer a
emptyTransducer = compile EmptyR []
