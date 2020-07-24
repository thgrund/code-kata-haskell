module Main where 

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Char as Char

type WordNum = String

main :: IO()
main = do
    print $ magic 5
    print $ magic 10
    print $ magic 17
    print $ magic 32
    print $ magic 1943
    print $ magic 13954
    print $ magic 1234045234
    print $ magic 123495324324320

capitalized :: Maybe String -> Maybe String
capitalized ms1 = do
    s1 <- ms1
    return $ Char.toUpper (head s1) : map Char.toLower (tail s1)

magic :: (Integral a, Ord a) => a -> Maybe String
magic x 
    | x == 0                = Just "Zero is four, " +++ zeroresult 
    | calc == Just "four"   = calc +++ Just " is magic."
    | otherwise             = capitalized $ calc +++ Just " is " +++ testMagic (lengthresult) +++ Just ", " +++ magic (lengthresult)
    where
        calc = testMagic x
        lengthresult = eliminateMaybe (lengthMaybe calc)
        zeroresult = magic (eliminateMaybe $ lengthMaybe $ Just "Zero") 

eliminateMaybe :: (Integral a, Ord a) => Maybe a -> a
eliminateMaybe (Just a) = a
eliminateMaybe Nothing  = -1

testMagic :: (Integral a, Ord a) => a -> Maybe String
testMagic x = concatL $ ones x x []

(+++) :: Monad m => m [a] -> m [a] -> m[a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

concatL :: [Maybe String] -> Maybe String
concatL [] = Just ""
concatL [x] = x 
concatL (x:xs) = x +++ Just " " +++ concatL xs

dash :: Maybe String -> Maybe String -> Maybe String 
dash s1 s2 = if s2 == Just "" then s2 else s1 +++ s2

lengthInt :: (Integral a, Ord a) => a -> Int
lengthInt x = length $ show $ fromIntegral x 

lengthMaybe :: Maybe String -> Maybe Int
lengthMaybe ms = do
     s <- ms
     return $ length s

-- TODO: avoid !!
groupOfThree :: (Integral a, Ord a) => a -> a -> Maybe String
groupOfThree x y = M.lookup ( (reverse [1 .. ((lengthInt y - 1) `div` 3)]) !! ((((lengthInt x) - 1) `div` 3)-1)) namedNumbers
  where
    namedNumbers = M.fromList [
        (1, "thousand"),
        (2, "million"),
        (3, "trillion"),
        (4, "quadrillion")]

ones :: (Integral a, Ord a) => a -> a -> [Maybe String] -> [Maybe String]
ones n m xs
  | n == 0          = xs
  | n > 0 && n < 20 = (M.lookup n onsies) : xs 
  | n < 100         = ((M.lookup (n `div` 10 * 10) tens) +++ ( dash (Just "-") (M.lookup (n `mod` 10) onsies))) : xs
  | n < 1000        = (M.lookup (n `div` 100) onsies) : (M.lookup 100 tens) : (ones (n - n `div` 100 * 100) m xs )
  | n >= 1000       = (ones (n `div` 1000) m xs) ++ (groupOfThree n m) : (ones (n `mod` 1000) m xs)
  | otherwise       = xs
  where
    onsies = M.fromList [
        (0, ""),
        (1,"one"),
        (2, "two"),
        (3,"three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"), 
        (8, "eight"),
        (9, "nine"),
        (10, "ten"),
        (11, "eleven"), 
        (12, "twelve"),
        (13, "thirteen"),
        (14, "fourteen"), 
        (15, "fifteen"),
        (16, "sixteen"),
        (17, "seventeen"),
        (18, "eighteen"),
        (19, "ninteen")]
    tens = M.fromList [(20, "twenty"), 
        (30, "thirty"), 
        (40, "forty"), 
        (50, "fifty"), 
        (60, "sixty"),
        (70, "seventy"), 
        (80, "eighty"), 
        (90, "ninety"),
        (100, "hundred")]
