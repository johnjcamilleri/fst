{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, GeneralizedNewtypeDeriving #-}

{- |
fstStudio takes a program consisting of regular relations that denotes
the relation between two regular languages and constructs a
transducer. If a regular expression, not a relation, is given, then it
is interpreted as the identity relation. The syntax is very similar to
Xerox's finite state transducer syntax with two fundamental
differences: a distinction is made between functions (definitions) and
strings, and fststudio allows functional definitions.

[@\"a\"@] A symbol. Example: @[\"b\"]@ denotes the language @{\"b\"}@.

[@a@] A variable. A symbol without quotes is a variable.

[@\"a\":\"b\"@] Describes a relation between the symbol @a@ and @b@.
This relation is ordered and @a@ is said to be a part of the /upper
language/ and @b@ is said to be part of the /lower language/.
Example: @[\"a\":\"b\"]@ denotes the relation @{(\"a\",\"b\")}@.

[@0@] Epsilon symbol. The epsilon symbol denotes the string with no
symbols.  Example: @[0]@ denotes the language @{\"\"}@.

[@?@] All symbol. The all symbol denotes the union of all symbols in
the alphabet. Example: @[?]@ and an alphabet @{a,b,c}@ denotes the
language @{\"a\",\"b\",\"c\"}@.

[@\"\"@] quotes cancel every special meaning of the symbols. Example:
@[\"? 0\"]@ denotes the language @{\"? 0\"}@.

[@\[A\]@] brackets are used to change the precedence of a regular
relation.

[@(A)@] parenthesis expresses optionality, and has the same meaning as
@[A|0]@.

[@A B@] Concatenation of the expressions or relations A and
B. Example: @[[a b] [c d]]@ denotes the language @{\"ac\", \"ad\", \"bc\",
\"bd\"}@

[@A^n@] Concatenation of @A@ /n/ times.  @A^0@ is defined as the empty
string. Example: @[a]^3@ describes the language @{\"aaa\"}@.

[@A|B@] Union of the languages or relations @A@ and @B@. Example: @[a|b]@
describes the language @{\"a\",\"b\"}@.

[@A & B@] Intersection of the languages @A@ and @B@.  Example: @[a b]
& [a]@ describes the language @{\"a\"}@.

[@A - B@] Minus of the languages @A@ and @B@, and has the same meaning as
@[A & B]@.  Example: @[a b] - [a]@ describes the language @{\"b\"}@.

[@~A@] Describes the complement of an expression, and has the same
meaning as @[?* - A]@.  Note that complement is always defined over
an alphabet. The expression @[A]@ is only unambiguous with respect to
an alphabet. Example: @[a]@ denotes the language that doesn't contain
the string @\"a\"@. If the alphabet is @{\"a\",\"b\"}@ then @[a]@
denotes the language @{\"\",\"b\",\"aa\",\"ba\",...}@.

[@A+@] Repetition (Kleenes plus).  A concatenated with itself an
arbitrary number of times, including zero times. Example: @[a]+@ denotes
the infinite language @{\"a\",\"aa\",\"aaa\",...}@

[@A*@] Kleene’s star: @[A+ | 0]@.  Example: @[a]*@ denotes the infinite
language @{\"\",\"a\",\"aa\",...}@

[@$A@] Containment.  The set of strings where @A@ appear at least once
as a substring. Containment is the same thing as @[?* A ?*]@.

[@A .x. B@] Cross product of the languages @A@ and @B@.  Example: @[[a b]
.x. c]@ describes the relations @{(\"a\",\"c\"), (\"b\",\"c\")}@.

[@A .o. B@] Composition of the relations @A@ and @B@.  Example: @[a:b c:d]
.o. [d:e]@ describes the relation @{(\"c\",\"e\")}@.

The precedence of the operators is as follows, where 4 is the highest
precedence:

  1. @.x.@ @.o.@

  2. @&@ @-@

  3. /Concatenation/

  4. @~@ @^@ @*@ @+@ @$@

A file containing a program must end with @.fst@, and an input file
mustend with @.dat@.  A program is a collection of functions defining
regular relations. A function with zero arguments is called a
definition or a macro.  A definition, or a macro, can for example look
like this:

> <digits> ::= "1" | "2" | "3" | "4" | "5" |
>              "6" | "7" | "8" | "9" | "0" ;

and a function can look like this:

> <swap,a,b> ::= b a ;

Note that strings are marked with quotes, and variables have no
quotes. Every program must contain a @\<main\>@ definition (a program
without one will result in a parse error).

> <main> ::= ... ;

The alphabet of a program is the symbols in the regular relation
defined in the program.

/Example program/

> <nickel>  ::= ["n" .x. "c"^5];
> <dime>    ::= ["d" .x. "c"^10];
> <quarter> ::= ["q" .x. "c"^25];
> <cent>    ::= ["c" .x. "c"];
> <money>   ::= [ <nickel> | <dime> | <quarter> | <cent>]*;
> <drink>   ::= ["c"^65 .x. "PLONK"];
> <main>    ::= [ <money> .o. <drink> ];

/Batch mode/

Usage: @fst FILE [Options]@.  FILE must end with @.fst@, which defines
an FstStudio program, or @.net@, which defines a saved transducer. If
no options are given, then input is taken from standard input, the
transducer is applied down, and the output, if any, is produced on
standard output.

[@-u@] Apply the transducer up

[@-d@] Apply the transducer down

[@-i FILE@] Take input from FILE

[@-o FILE@] Write output to FILE

/Interactive mode - list of commands/

[@r REG@] Read a regular relation from standard input. If a regular
expression is typed, then it is interpreted as the identity relation.

[@b@] Build an epsilon-free, deterministic, minimal transducer from a
loaded/typed regular relation.

[@bn@] Build an epsilon-free, possibly non-deterministic, non-minimal
transducer from a load/typed regular relation.

[@m@] Minimize a built transducer.

[@det@] Determinize a built transducer.

[@s FILE@] Save to @FILE@. If @FILE@ ends with @.net@, then the built
transducer is saved. Any other suffix saves the produced output in the
system to @FILE@, if any.

[@l FILE@] Load from @FILE@. @FILE@ must end with @.fst@, @.net@ or
@.dat@. If @FILE@ ends with @.fst@, then a FstStudio program is loaded
into FstStudio. If @FILE@ ends with @.net@, then a transducer is loaded
into FstStudio. If @FILE@ ends with @.dat@, then input is loaded into
FstStudio.

[@l a | b@] Load and union two transducers. a and b must either be a
file ending with @.net@ or the symbol @*@, which refers to the interior
transducer. The produced transducer is possibly non-deterministic and
non-minimal.

[@l a b@] Load and concatenate two transducers. a and b must either be
ale ending with @.net@ or the symbol @*@, which refers to the interior
transducer. The produced transducer is possibly non-deterministicand
non-minimal.

[@l a*@] Load and apply Kleene’s star on a transducer. a must either
be a file ending with @.net@ or the symbol @*@, which refers to the
interior transducer. The produced transducer is possibly
non-deterministicand non-minimal.

[@l a .o. b@] Load and compose two transducers. a and b must either be
a file ending with @.net@ or the symbol @*@, which refers to the
interior transducer. The produced transducer is possibly
non-deterministic andnon-minimal.

[@vt@] View loaded/built transducer.

[@vr@] View loaded/typed regular relation.

[@vi@] View loaded input.

[@vo@] View produced output.

[@d@] Apply transducer down with loaded input.

[@u@] Apply transducer up with loaded input.

[@d SYMBOLS@] Apply tranducer down with @SYMBOLS@.

[@u SYMBOLS@] Apply transducer up with @SYMBOLS@.

[@c@] Clear memory.

[@h@] List commands.

[@q@] End session.

-}
module Main where

import Data.FST.TransducerInterface
import Data.FST.RRegTypes
import Data.FST.Arguments
import Data.FST.Info

import Text.Printf

import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import System.Console.Haskeline

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      welcome
      runInputT defaultSettings (evalStateT loop emptyInfo)
    as -> do
      ret <- runErrorT (batchMode as)
      case ret of
        Left err -> putStrLn err
        Right _  -> return ()

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

-- | Run in batch mode with given arguments
batchMode :: [String] -> ErrorT String IO ()
batchMode cmdopt = do
  (file, cmd) <- (ErrorT . return . parseBatch) cmdopt

  -- Only accept .NET or .FST files
  when (not (isNET file) && not (isFST file)) $ 
    throwError "Input file must end with *.fst or *.net"

  transducer <- if isFST file
      then do str <- open file
              fmap (flip compile []) $ ErrorT $ return $ parseProgram str
      
      -- Load transducer directly from .NET files
      else load file
  
  let action     = if isUpB cmd then upB else downB
      inputFile  = inputB cmd
      outputFile = outputB cmd
  
  case inputFile of
    Just file -> do
      str <- open file
      case outputFile of
        Just f  -> saveToFile f str
        Nothing -> throwError $ action transducer str
    Nothing   -> liftIO $ interact (action transducer)

-- | Apply up in batch mode
upB :: Transducer String -> String -> String
upB transducer str =
  case applyUp transducer (words str) of
    Just xs -> unlines (map unwords xs)
    Nothing -> []

-- | Apply down in batch mode
downB :: Transducer String -> String -> String
downB transducer str =
  case applyDown transducer (words str) of
    Just xs -> unlines (map unwords xs)
    Nothing -> []

-- | Error when there is no built transducer
noTransducer :: String
noTransducer = "No transducer has been loaded/built."

-- | Error when there is no regular expression
noExpression :: String
noExpression = "No regular expression has been typed/loaded into fstStudio."

-- | Error when there is no loaded input
noInput :: String
noInput = "No input has been loaded."

-- | Error when no output has been produced
noOutputs :: String
noOutputs = "No outputs has been produced."

-- | Adds a new transducer to the environment and returns it
mkTransducer :: MonadState Info m => Transducer String -> m (Transducer String)
mkTransducer newTransducer = do
  modify (updateTransducer newTransducer)
  return newTransducer

-- | Main interactive-shell loop
loop :: StateT Info (InputT IO) ()
loop = do
  input <- lift $ getInputLine "> "
  let command = fmap (parseInteractive . words) input
  case command of
    Nothing   -> return ()
    Just Quit -> lift $ outputStrLn "Session ended."
    Just cmd  -> do
      -- Run a single command entered at the prompt
      result <- runErrorT $ runCmd cmd

      -- Print the resulting output or error message
      lift (either outputStrLn outputStrLn result)
      loop 

-- | Called for each user command
runCmd :: InteractiveCommand -> ErrorT String (StateT Info (InputT IO)) String
runCmd  BuildTransducer = do
  info <- get
  unless (expressionRead info) $ throwError noExpression
  
  let newTransducer = compile (getExpression info) []
  modify (updateTransducer newTransducer)
  return $ printf "Built a deterministic, minimal transducer with %d states and %d transitions." 
      (numberOfStates newTransducer) (numberOfTransitions newTransducer)
    
runCmd BuildNTransducer = do
  info <- get
  unless (expressionRead info) $ throwError noExpression

  newTransducer <- mkTransducer $ compileN (getExpression info) []

  return $ printf "Built a possibly non-deterministic, non-minimal transducer with %d states and %d transitions."
     (numberOfStates newTransducer) (numberOfTransitions newTransducer) 

runCmd Minimize = do
  info <- get
  unless (transducerBuilt info) $ throwError noTransducer 
  newTransducer <- mkTransducer $ minimize $ getTransducer info

  return $ 
    printf "Minimized loaded/built transducer resulting in a transducer with %d states and %d transitions."
      (numberOfStates newTransducer) (numberOfTransitions newTransducer) 

runCmd Determinize = do
  info <- get
  unless (transducerBuilt info) $ throwError noTransducer 
  newTransducer <- mkTransducer $ determinize $ getTransducer info
  return $
    printf "Determinized loaded/built transducer resulting in a transducer with %d states and %d transitions."
      (numberOfStates newTransducer) (numberOfTransitions newTransducer) 


runCmd ViewTransducer = do
  info <- get
  
  if transducerBuilt info
  then return $ showTransducer $ getTransducer info
  else throwError $ noTransducer

runCmd (Load file)
  | isFST file = do
    res <- liftIO $ runErrorT $ open file
    str <- ErrorT $ return res
    reg <- ErrorT $ return $ parseProgram str
    modify (updateExpression reg)
    return (printf "Loaded a regular relation from %s." file)
  | isNET file = do
    res    <- liftIO $ runErrorT $ load file
    transd <- ErrorT $ return res
    modify (updateTransducer transd)
    return (printf "Loaded transducer from file %s." file)
  | isDAT file = do
    res <- liftIO $ runErrorT $ open file
    str <- ErrorT $ return res
    modify $ updateInput $ words str
    return $ printf "Read input from file %s." file
  | otherwise =
    throwError $ "Unable to load from "++file++". The filename must end with *.fst, *.net or *.dat."
  
runCmd (LUnion file1 file2)
  | isNET file1 && isNET file2 = do
    res1 <- liftIO $ runErrorT $ load file1
    res2 <- liftIO $ runErrorT $ load file2
    t1 <- ErrorT $ return res1
    t2 <- ErrorT $ return res2
    modify $ updateTransducer (unionT t1 t2)
    return "Loaded and unified two transducers."
                                          
  | isNET  file1 && isTHIS file2 = unionWith file1
  | isTHIS file1 && isNET  file2 = unionWith file2
  | otherwise = return $ printf "Unable to union %s and %s." file1 file2 where
  unionWith file = do
    info <- get
    res <- liftIO $ runErrorT $ load file
    unless (transducerBuilt info) $ throwError "No interior transducer built."
    r1 <- ErrorT $ return res
    modify $ \info -> updateTransducer (unionT r1 (getTransducer info)) info
    return "Loaded a transducer, and unified it with the interior transducer."                

runCmd (LProduct file1 file2)
  | isNET file1  && isNET file2 = do
    res1 <- liftIO $ runErrorT $ load file1
    res2 <- liftIO $ runErrorT $ load file2
    t1 <- ErrorT (return res1)
    t2 <- ErrorT (return res2)
    modify (updateTransducer (productT t1 t2))
    return "Loaded and concatenated two transducers."
  | isNET file1 && isTHIS file2 = productWith file1
  | isTHIS file1 && isNET file2 = productWith file2
  | otherwise = return $ printf "Unable to concatenate %s and %s." file1 file2 where
  productWith file = do
    info <- get
    res <- liftIO $ runErrorT $ load file
    unless (transducerBuilt info) $ throwError "No interior transducer built."
    t1 <- ErrorT $ return res
    modify $ \info -> updateTransducer (productT t1 (getTransducer info)) info
    return "Loaded a transducer, and concatenated it with the interior transducer."

runCmd (LStar file)
  | isNET file = do
    res <- liftIO $ runErrorT $ load file
    t1 <- ErrorT (return res)
    modify $ updateTransducer (starT t1)
    return "Loaded a transducer, and applied Kleene's star."
  | isTHIS file = do
    info <- get
    unless (transducerBuilt info) $ throwError "No interior transducer built."
    modify $ updateTransducer (starT (getTransducer info))
    return "Applied Kleene's star on interior transducer."
  | otherwise = return $ printf "Unable to apply Kleene's star on %s." file

runCmd (LComposition file1 file2)
  | isNET file1  && isNET file2 = do
    res1 <- liftIO $ runErrorT $ load file1
    res2 <- liftIO $ runErrorT $ load file2
    t1 <- ErrorT (return res1)
    t2 <- ErrorT (return res2)
    modify $ updateTransducer (compositionT t1 t2)
    return "Loaded and composed two transducers."
  | isNET file1  && isTHIS file2 = composeWith file1
  | isTHIS file1 && isNET file2  = composeWith file2
  | otherwise = return $ printf "Unable to compose %s and %s." file1 file2 where
  composeWith file = do
    info <- get
    res <- liftIO $ runErrorT $ load file
    unless (transducerBuilt info) $ throwError "No interior transducer built."
    t1 <- ErrorT (return res)
    modify $ \info -> updateTransducer (compositionT t1 (getTransducer info)) info
    return "Loaded a transducer, and composed it with the interior transducer."

runCmd (Save file) = do
  info <- get
  case () of
    _ | isNET file -> do
        res <- liftIO $ runErrorT $ save file $ getTransducer info
        _ <- ErrorT (return res)
        return $ printf "Saved transducer to file %s." file
      | outputsRead info -> do
        res <- liftIO $ runErrorT $ saveToFile file $ unlines $ getOutputs info
        _ <- ErrorT (return res)
        return $ printf "Saved outputs to file %s." file
      | otherwise -> return noOutputs

runCmd (StdInReg f) = 
    case parseExp f of
      Left err  -> throwError err
      Right reg -> modify (updateExpression reg) >> return "Read a regular relation."

runCmd ViewReg = do
  info <- get
  if expressionRead info
  then return $ show (getExpression info)
  else throwError noExpression

runCmd Quit        = return "Session ended."
runCmd ClearMemory = modify (const emptyInfo) >> return ""
runCmd NoCommand   = throwError "Invalid Command. Type 'h' for help."
runCmd Help        = return help 
runCmd ViewInput   = do
  info <- get
  if inputRead info
  then return $ unwords $ getInput info
  else throwError noInput

runCmd ViewOutput = do
  info <- get
  if outputsRead info
  then return $ unlines $ getOutputs info
  else throwError noOutputs

runCmd ApplyUp = do
  info <- get
  case (transducerBuilt info, inputRead info) of
    (True, True) ->
      case applyUp (getTransducer info) (getInput info) of
        Just res -> do
          modify $ updateOutputs $ map unwords res
          return  "Input accepted. Type 'vo' to view outputs."
        Nothing -> throwError "Input rejected."
    (True, False) -> throwError noTransducer
    _ -> throwError noInput

runCmd ApplyDown = do
  info <- get
  case (transducerBuilt info, inputRead info) of
    (True, True) ->
      case applyDown (getTransducer info) (getInput info) of
      Just res -> do
        modify (updateOutputs (map unwords res))
        return "Input accepted. Type 'vo' to view outputs."
      Nothing -> throwError "Input rejected."
    (True, False) -> throwError noTransducer
    _ -> throwError noInput

runCmd (ApplyU inp) = do
  info <- get
  unless (transducerBuilt info) $ throwError noTransducer

  case applyUp (getTransducer info) inp of
    Just res -> do
      modify $ updateOutputs $ map unwords res
      return "Input accepted. Type 'vo' to view outputs."
    Nothing -> return "Input rejected."
  
runCmd (ApplyD inp) = do
  info <- get
  unless (transducerBuilt info) $ throwError noTransducer

  case applyDown (getTransducer info) inp of
    Just res -> do
       modify $ updateOutputs $ map unwords res
       return "Input accepted. Type 'vo' to view outputs."
    Nothing -> return "Input rejected."

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
