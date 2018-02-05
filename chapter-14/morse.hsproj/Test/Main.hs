module Test.Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck


-- lists of legal symbols

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse


-- setup generators

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse


propThereAndBackAgain :: Property
propThereAndBackAgain = forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck propThereAndBackAgain