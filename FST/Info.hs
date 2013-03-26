{- |
Module      :  $Header$
Description :  Helper functions for the interactive shell
Maintainer  :  Markus Forsberg

Helper functions for the interactive shell
-}
module FST.Info where

import FST.TransducerInterface

import System.Console.Haskeline

-- | State in interactive shell
data Info = Info {
  transducer :: (Transducer String, Bool),
  expression :: (RReg String, Bool),
  input      :: ([String], Bool),
  outputs    :: ([String], Bool)
  } deriving (Show)

-- | Empty information
emptyInfo :: Info
emptyInfo = Info {
  transducer = (emptyTransducer, False),
  expression = (empty, False),
  input      = ([], False),
  outputs    = ([], False)
  }

-- | Is there a built transducer in the state?
transducerBuilt :: Info -> Bool
transducerBuilt = snd . transducer

-- | Is there a read expression in the state?
expressionRead :: Info -> Bool
expressionRead = snd . expression

-- | Is there an input in the state?
inputRead :: Info -> Bool
inputRead = snd . input

-- | Is there an output in the state?
outputsRead :: Info -> Bool
outputsRead = snd . outputs

-- | Set transducer in state
updateTransducer :: Transducer String -> Info -> Info
updateTransducer t info = info { transducer = (t, True) }

-- | Set expression in state
updateExpression :: RReg String -> Info -> Info
updateExpression r info = info { expression = (r, True) }

-- | Set input in state
updateInput :: [String] -> Info -> Info
updateInput inp info = info { input = (inp, True) }

-- | Set outputs in state
updateOutputs :: [String] -> Info -> Info
updateOutputs out info = info { outputs = (out, True) }

-- | Get transducer from state
getTransducer :: Info -> Transducer String
getTransducer = fst . transducer

-- | Get expression from state
getExpression :: Info -> RReg String
getExpression = fst . expression

-- | Get input from state
getInput :: Info -> [String]
getInput = fst . input

-- | Get outputs from state
getOutputs :: Info -> [String]
getOutputs = fst . outputs

-- | Dislay list of shell commands for user
help :: String
help = unlines [
  "List of Commands:",
  "r <reg exp>    : read a regular relation from standard input.",
  "b              : build a deterministic, minimal transducer.",
  "bn             : build a possibly non-deterministic, non-minimal transducer.",
  "m              : minimize loaded/built transducer.",
  "det            : determinize loaded/built transducer.",
  "s  <filename>  : save to file.",
  "l  <filename>  : load from file.",
  "l a | b        : load and union.",
  "l a b          : load and concatenate.",
  "l a *          : load and apply Kleene's star.",
  "l a .o. b      : load and compose.",
  "vt             : view loaded/built transducer.",
  "vr             : view typed/loaded regular relation.",
  "vi             : view loaded input.",
  "vo             : view produced output.",
  "d              : apply transducer down with loaded input.",
  "u              : apply transducer up with loaded input.",
  "d <symbols>    : apply transducer down with symbols.",
  "u <symbols>    : apply transducer up with symbols.",
  "c              : Clear memory.",
  "h              : display list of commands.",
  "q              : end session."
  ]

-- | Display welcome message
welcome :: IO ()
welcome = putStr $ unlines [
  "***********************************************************",
  "* Finite State Transducer Studio",
  "* Written purely in Haskell.",
  "* Version : 0.10",
  "* Updated : 17 March 2013",
  "* Author  : Markus Forsberg",
  "* With contributions by Baldur Blöndal & John J. Camilleri",
  "***********************************************************",
  "",
  "Type 'h' for help."
  ]
