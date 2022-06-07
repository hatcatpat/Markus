module Main where

data Type
  = Basic
  | Bold
  | Italic
  | Header Int
  deriving (Show, Eq)

class Convertable a where
  chars :: a -> [Char]
  convert :: a -> (String -> String)

instance Convertable Type where
  chars Basic = []
  chars Bold = ['*']
  chars Italic = ['/']
  chars (Header _) = ['#']
  convert Basic = id
  convert Bold = \str -> "<b>" ++ str ++ "</b>"
  convert Italic = \str -> "<i>" ++ str ++ "</i>"
  convert (Header a) = \str -> "<h" ++ show a ++ ">" ++ str ++ "</h" ++ show a ++ ">"

getHeaderDepth :: Type -> Int
getHeaderDepth (Header h) = h
getHeaderDepth x = -1

isHeader :: Type -> Bool
isHeader (Header _) = True
isHeader _ = False

getType :: Char -> Type
getType x
  | x `elem` chars Bold = Bold
  | x `elem` chars Italic = Italic
  | x `elem` chars (Header 1) = Header 1
  | otherwise = Basic

parse :: String -> [(String, Type)]
parse = iter [] ("", Basic)
  where
    append o i
      | null (fst i) = o
      | otherwise = o ++ [i]
    iter o ("", _) "" = o
    iter o i "" = append o i
    iter o i (x : xs)
      | x == '\n' && h = iter (append o (s, t)) ("", Basic) xs
      | t' == t || (h && h') =
        case t of
          Basic -> iter o (s ++ [x], t) xs
          Header n ->
            if null s
              then iter o (s, Header (n + 1)) xs
              else iter o (s ++ [x], t) xs
          _ -> iter (append o i) ("", Basic) xs
      | otherwise =
        if t == Basic
          then iter (append o i) ("", t') xs
          else iter o (s ++ [x], t) xs
      where
        s = fst i
        t = snd i
        t' = getType x
        h = isHeader t
        h' = isHeader t'

compile :: (String, Type) -> String
compile (x, t) = convert t x

compileAll :: [(String, Type)] -> String
-- compileAll xs = "<pre>\n" ++ c ++ "\n</pre>"
compileAll xs = c
  where
    c = concatMap compile xs

main :: IO ()
main = do
  p <- parse <$> readFile "input.mks"
  let debug = (unlines . map show) p
  print debug
  writeFile "test.html" (compileAll p)
  writeFile "ast" debug