-- | Haskell version of drinks example

import FST.TransducerInterface
import Data.Maybe (fromJust)

{-
<nickel>  ::= ["n" .x. "c"^5];
<dime>    ::= ["d" .x. "c"^10];
<quarter> ::= ["q" .x. "c"^25];
<cent>    ::= ["c" .x. "c"];
<money>   ::= [ <nickel> | <dime> | <quarter> | <cent> ]*;
<drink>   ::= ["c"^65 .x. "PLONK"];
<main>    ::= [ <money> .o. <drink> ];
-}

-- | Emulation of ^ operator
times :: Int -> String -> Reg String
times n str = foldl (\a b -> a |> s b) (s (head ss)) (tail ss)
  where ss = replicate n str

nickel  = s "n" <*> (times 5 "c")
dime    = s "d" <*> (times 10 "c")
quarter = s "q" <*> (times 25 "c")
cent    = s "c" <*> s "c"
money   = star (nickel <|> dime <|> quarter <|> cent)
drink   = (times 65 "c") <*> s "PLONK"
main'   = money <.> drink

main :: IO ()
main = do
  let trans = compile main' []
  putStrLn $ unwords $ concat $ fromJust $ applyUp trans ["PLONK"]

-- | Parsed version of drinks transducer
parsed :: RReg String
parsed = case either of
  Right r -> r
  where
    either = parseProgram $ unlines [
      "<nickel>  ::= [\"n\" .x. \"c\"^5];",
      "<dime>    ::= [\"d\" .x. \"c\"^10];",
      "<quarter> ::= [\"q\" .x. \"c\"^25];",
      "<cent>    ::= [\"c\" .x. \"c\"];",
      "<money>   ::= [ <nickel> | <dime> | <quarter> | <cent> ]*;",
      "<drink>   ::= [\"c\"^65 .x. \"PLONK\"];",
      "<main>    ::= [ <money> .o. <drink> ];"
      ]
