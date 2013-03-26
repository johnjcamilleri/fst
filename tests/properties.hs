{-# LANGUAGE ViewPatterns #-}

import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe

import Test.QuickCheck

import FST.LBFA
import FST.Automaton

import FST.RegTypes
import FST.RRegTypes (idR)
import FST.TransducerTypes
import FST.TransducerInterface (compile)
import FST.RunTransducer

instance Arbitrary a => Arbitrary (Reg a) where
  arbitrary = oneof [return Empty,
                     return Epsilon,
                     return All,
                     liftM2 (:|:) arbitrary arbitrary,
                     liftM2 (:.:) arbitrary arbitrary,
                     liftM2 (:&:) arbitrary arbitrary,
                     liftM Symbol arbitrary,
                     liftM Complement arbitrary,
                     liftM Star arbitrary]

-- | A datatype that only accepts the empty string.
newtype EmptyString a = EmptyString { unEmpty :: Reg a } deriving Show

instance Arbitrary a => Arbitrary (EmptyString a) where
  arbitrary =
    fmap EmptyString $
    oneof [return Epsilon,
           liftM  Star arbitrary,
           liftM2 (:|:) arbitrary (fmap unEmpty arbitrary),
           liftM2 (:|:) (fmap unEmpty arbitrary) arbitrary,
           liftM2 (:&:) (fmap unEmpty arbitrary) (fmap unEmpty arbitrary),
           liftM2 (:.:) (fmap unEmpty arbitrary) (fmap unEmpty arbitrary),
           fmap (Complement . Complement) (fmap unEmpty arbitrary)] 
                     
prop_empty :: Eq a => EmptyString a -> Bool
prop_empty (EmptyString xs) = acceptEps xs

instance Arbitrary a => Arbitrary (Symbol a) where
  arbitrary = frequency [(4, S `fmap` arbitrary)
                        ,(1, return Eps        )] 


newtype Language = Language { unLanguage :: (Reg Char, [String]) } deriving Show

instance Arbitrary Language where
  arbitrary = sized gLanguage 
    
gLanguage :: Int -> Gen Language
gLanguage 0 = Language `fmap` oneof
              -- Return empty language ε
              [return (Epsilon, [""]),
               
              -- Return one of two symbols: a or b
               elements "ab" >>= \ch -> return (Symbol ch, [[ch]])]
gLanguage n = Language `fmap` oneof
               -- Generate A* ≅ {ε, A, A} out of arbitrary A
              [do (reg, lang) <- fmap unLanguage subGen
                  let matches = concat [[""], lang, (++) <$> lang <*> lang]
                  return (Star reg, nub matches),
               
               -- Sequence A B out of arbitrary A and B
               do (reg1, lang1) <- fmap unLanguage subGen
                  (reg2, lang2) <- fmap unLanguage subGen
                  let matches = (++) <$> lang1 <*> lang2
                  return (reg1 :.: reg2, nub matches)]
  where subGen :: Gen Language
        subGen = gLanguage (n `div` 4)

-- If a regular expression generates a (sub)language, then compiling it and 
-- running it on all strings of that languages should always match.
prop_language (Language (reg, inputs)) =
  and [ maybe False (elem input) appliedUp && maybe False (elem input) appliedDown
      | input <- inputs
      , let appliedUp   = applyUp   (compile (idR reg) "ab") input
      , let appliedDown = applyDown (compile (idR reg) "ab") input ] 

-- | Run the test suite
main :: IO ()
main = do
  putStrLn "Running tests"
  quickCheck (prop_empty :: EmptyString String -> Bool)
  quickCheck prop_language

