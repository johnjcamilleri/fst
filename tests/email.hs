-- | Email address recogniser using transducer interface API

import Data.FST.TransducerInterface
import Data.Maybe (fromJust)

charsToUnion :: [Char] -> Reg String
charsToUnion cs = foldl (\a b -> a <|> s [b]) (s [head cs]) (tail cs)

digit :: Reg String
digit = charsToUnion ['0'..'9']

digit1 :: Reg String
digit1 = charsToUnion ['1'..'9']

letter :: Reg String
letter = charsToUnion ['a'..'z']

punct :: Reg String
punct = charsToUnion "!#$%&'*+-/=?^_{|}~"

hyphen :: Reg String
hyphen = s "-" ;

-- Local part
local :: RReg String
local = idR $ plus local_sym
  where local_sym = digit <|> letter <|> punct

-- Should be limited to 64 chars, but this is impossible to compile:
-- local = idR $ (plus local_sym) <-> over65
--   where local_sym = digit <|> letter <|> punct
--         over65 = foldl (\a b -> a <|> b) local_sym (replicate 65 local_sym)

-- IP address
ip_segment :: Reg String
ip_segment =
      digit
  <|> (digit1 |> digit)
  <|> (s "1" |> digit |> digit)
  <|> (s "2" |> d0to5 |> digit)
  <|> (s "2" |> s "5" |> (digit <-> d6to9))
  where
    d0to5 = s "0" <|> s "1" <|> s "2" <|> s "3" <|> s "4"
    d6to9 = s "6" <|> s "7" <|> s "8" <|> s "9"

ip :: RReg String
ip = idR $ ip_segment |> dot |> ip_segment |> dot |> ip_segment |> dot |> ip_segment
  where dot = s "."

-- DNS names
hostname :: Reg String
hostname = plus (digit <|> letter <|> hyphen)

subdomain :: RReg String
subdomain = idR hostname

-- | Emulation of optionality (?)
option :: RReg String -> RReg String
option rr = rr <|> idR eps

domain :: RReg String
domain =
      ((star (subdomain |> dot)) |> idR hostname)
  <|> option ((student <|> staff) |> dot) |> "Chalmers University" `r` "chalmers"
  <|> "Apple Inc." `r` "apple"
  <|> option gmail |> ("Google" `r` "google")
  where
    student = "Student" `r` "student"
    staff = "Academic Staff" `r` "staff"
    gmail = "Google Mail" `r` "mail" |> dot
    dot = "at" `r` "."

-- Some known TLDs
tld_se = "Sweden" `r` "se"
tld_mt = "Malta" `r` "com.mt"
tld_uk = "UK" `r` "co.uk"
tld_com = idR (s "com")
tld_net = idR (s "net")
tld_org = idR (s "org")
tld = tld_se <|> tld_mt <|> tld_uk <|> tld_com <|> tld_net <|> tld_org

-- | An email address using either DNS names or IP address
email :: RReg String
email =
      local |> at |> domain |> dot |> tld
  <|> local |> at |> ip
  where
    at = idR (s "@")
    dot = "|" `r` "."

-- | Main test function
main :: IO ()
main = do
  let trans = compile email [] :: Transducer String
      tests = map words [test1, test2, test3, test4, test5, test6]
  sequence_ $ map (handle . applyUp trans) tests
  where
    handle :: Maybe [[String]] -> IO ()
    handle (Just outs) = putStrLn $ unwords $ concat outs
    handle Nothing = putStrLn "Input rejected."

-- Test cases
test1 = "j o h n @ 1 9 2 . 1 6 8 . 0 . 1"
test2 = "j o h n { 8 6 } @ chalmers . se"
test3 = "j - ! @ student . chalmers . se"
test4 = "a b c d @ staff . chalmers . se"
test5 = "# $ % ^ @ mail . google . com.mt"
test6 = "b a l d u r @ apple . co.uk"
