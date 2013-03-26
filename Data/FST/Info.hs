{- |
State data structure for the interactive shell
-}
module Data.FST.Info where

import Data.FST.TransducerInterface

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

