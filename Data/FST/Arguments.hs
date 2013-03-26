{- |
Helper functions for handling shell/command-line options
-}
module Data.FST.Arguments (

  -- * Commands ADT
  InteractiveCommand (..),
  BatchCommand (..),

  -- * Helper functions
  parseInteractive,
  isFST,
  isDAT,
  isNET,
  isTHIS,
  parseBatch,
  inputB,
  outputB,
  isUpB
  ) where

import System.Console.GetOpt 
import Data.List
import Data.Maybe

-- | ADT for a shell command
data InteractiveCommand =
  -- | Build an epsilon-free, deterministic, minimal transducer from a
  -- loaded/typed regular relation.
    BuildTransducer                
  -- | Build an epsilon-free, possibly non-deterministic, non-minimal
  -- transducer from a load/typed regular relation.
  | BuildNTransducer
  -- | Minimize a built transducer.
  | Minimize
  -- | Determinize a built transducer.
  | Determinize
  -- | Read a regular relation from standard input.
  | StdInReg String
  -- | Load from FILE.
  | Load FilePath
  -- | Load and union two transducers.
  | LUnion FilePath FilePath
  -- | Load and concatenate two transducers.
  | LProduct FilePath FilePath
  -- | Load and apply Kleene's star on a transducer.
  | LStar FilePath
  -- | Load and compose two transducers.
  | LComposition FilePath FilePath
  -- | Save to file.
  | Save FilePath
  -- | Apply transducer down with loaded input.
  | ApplyDown
  -- | Apply transducer up with loaded input.
  | ApplyUp
  -- | Apply tranducer down with given symbols.
  | ApplyD [String]
  -- | Apply tranducer up with given symbols.
  | ApplyU [String]
  -- | View loaded/typed regular relation.
  | ViewReg
  -- | View loaded input.
  | ViewInput
  -- | View prodeced output.
  | ViewOutput
  -- | View loaded/built transducer.
  | ViewTransducer
  -- | List commands.
  | Help
  -- | Clear loaded transducers/input/output.
  | ClearMemory
  -- | Quit the shell
  | Quit
  -- | Unparseable command
  | NoCommand
  deriving (Eq, Show)

-- | Parse input string into a command
parseInteractive :: [String] -> InteractiveCommand
parseInteractive ["b"]                   = BuildTransducer
parseInteractive ["bn"]                  = BuildNTransducer
parseInteractive ["m"]                   = Minimize
parseInteractive ["det"]                 = Determinize
parseInteractive ("r":xs)                = StdInReg (unwords xs)
parseInteractive ["d"]                   = ApplyDown
parseInteractive ["u"]                   = ApplyUp
parseInteractive ("d":xs)                = ApplyD xs
parseInteractive ("u":xs)                = ApplyU xs
parseInteractive ["l",file]              = Load file
parseInteractive ["l",file1,"|",file2]   = LUnion file1 file2
parseInteractive ["l",file1," ",file2]   = LProduct file1 file2
parseInteractive ["l",file, "*"]         = LStar file
parseInteractive ["l",file1,".o.",file2] = LComposition file1 file2
parseInteractive ["s",file]              = Save file
parseInteractive ["vt"]                  = ViewTransducer
parseInteractive ["vi"]                  = ViewInput
parseInteractive ["vo"]                  = ViewOutput
parseInteractive ["vr"]                  = ViewReg
parseInteractive ["h"]                   = Help
parseInteractive ["q"]                   = Quit
parseInteractive ["c"]                   = ClearMemory
parseInteractive _                       = NoCommand

-- | Does the file end with .fst?
isFST :: String -> Bool
isFST = isSuffixOf ".fst"

-- | Does the file end with .dat?
isDAT :: String -> Bool
isDAT = isSuffixOf ".dat" 

-- | Does the file end with .net?
isNET :: String -> Bool
isNET = isSuffixOf ".net" 

-- | Is the internal transducer being specified?
isTHIS :: String -> Bool
isTHIS = (== "*")

-- | Is apply up?
isApplyUp :: [String] -> Bool
isApplyUp = elem "-u"

-- | Batch command ADT
data BatchCommand =
  -- | Apply down
    DownB
  -- | Apply up
  | UpB
  -- | Invalid command
  | InvalidCommand
  -- | Take input from given file
  | Input String
  -- | Write output to file
  | Output String
  -- | Display help
  | HelpB
  deriving (Eq, Show)

-- | Information for parsing batch options
batchOptions :: [OptDescr BatchCommand]
batchOptions = [
  Option ['u'] ["up"]     (NoArg UpB)            "apply the transducer up (default is down)",
  Option ['d'] ["down"]   (NoArg DownB)          "apply the transducer down (default)",
  Option ['i'] ["input"]  (ReqArg Input "FILE")  "read input from FILE",
  Option ['o'] ["output"] (ReqArg Output "FILE") "write output to FILE"
  ]

-- | Parse batch commands
parseBatch :: [String] -> Either String (FilePath,[BatchCommand])
parseBatch cmdline = case getOpt Permute batchOptions cmdline of
  (o, [file], [])   -> Right (file, o)
  (_, _,      errs) -> Left (concat errs ++ usageInfo header batchOptions) where
    header = "Usage: fst [FILE.net or FILE.fst] [OPTIONS...]"

-- -- | Handle batch input command
inputB  :: [BatchCommand] -> Maybe FilePath
inputB  cs = listToMaybe [ file | Input  file <- cs ]

-- | Handle batch output command
outputB :: [BatchCommand] -> Maybe FilePath
outputB cs = listToMaybe [ file | Output file <- cs ]

-- | Is batch command apply up?
isUpB :: [BatchCommand] -> Bool
isUpB = elem UpB

{-
-----------------------------------------------------------------------------------------
-- and here a small and hopefully enlightening example:

data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test :: ArgOrder Flag -> [String] -> String
test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header options
   where header = "Usage: foobar [OPTION...] files..."

-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["-?o","--name","bar","--na=baz"])
--    ==> options=[Version, Output "stdout", Name "bar", Name "baz"]  args=[]
-- putStr (test Permute ["--ver","foo"])
--    ==> option `--ver' is ambiguous; could be one of:
--          -v      --verbose             verbosely list files
--          -V, -?  --version, --release  show version info
--        Usage: foobar [OPTION...] files...
--          -v        --verbose             verbosely list files
--          -V, -?    --version, --release  show version info
--          -o[FILE]  --output[=FILE]       use FILE for dump
--          -n USER   --name=USER           only dump USER's files
-----------------------------------------------------------------------------------------

test :: ArgOrder BatchCommand -> [String] -> String
test order cmdline = case getOpt order batchOptions cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header batchOptions
   where header = "Usage: fst [OPTION...] files..."
-}
