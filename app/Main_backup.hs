module Main_Backup where

import Data.Char (toUpper)

data Status
  = Error
  | NoInput
  | NotFound
  | OK
  deriving (Eq, Show)

type Output = ((String, String), Status)

makeOutput :: String -> String -> Status -> Output
makeOutput left right status = ((left, right), status)

test :: String
test = "# hello # *there* \n buddy *again*"

extract_ :: Output -> (Char -> Bool) -> String -> Output
extract_ o p [] = makeOutput left right NotFound
  where
    strs = fst o
    left = fst strs
    right = snd strs
extract_ o p (x : xs)
  | p x = makeOutput left right OK
  | otherwise = extract_ (makeOutput (left ++ [x]) right OK) p xs
  where
    strs = fst o
    left = fst strs
    right = xs

extract :: (Char -> Bool) -> String -> Output
extract p [] = makeOutput "" "" NoInput
extract p str = extract_ (makeOutput "" "" OK) p str

between :: String -> [Char] -> Output
between str cs =
  if snd e == OK
    then extract p (snd . fst $ e)
    else e
  where
    p = (`elem` cs)
    e = extract p str

multiBetween_ :: String -> [Char] -> [String] -> [String]
multiBetween_ str cs o = do
  if snd e == OK
    then multiBetween_ right cs (o ++ [left])
    else o
  where
    e = between str cs
    strs = fst e
    left = fst strs
    right = snd strs

multiBetween :: String -> [Char] -> [String]
multiBetween str cs = multiBetween_ str cs []

bolds :: String -> [String]
bolds str = do
  let b = multiBetween str ['*']
  map (\x -> "<b>" ++ x ++ "</b>") b

main :: IO ()
main = do
  print $ extract (== 'y') test
  print $ bolds test