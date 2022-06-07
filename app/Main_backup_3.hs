module Main where

import Control.Monad (forM_)

data WholeLineIdens = Header Int | Title deriving (Show, Eq)

data Iden
  = Basic
  | Bold
  | Italic
  | NewLine
  | WholeLine WholeLineIdens
  | -- | Header Int
    -- | Title
    Error String
  deriving (Show, Eq)

type IdenString = (Iden, String)

class Idenable a where
  chars :: a -> String
  toHTML :: a -> String -> String

instance Idenable Iden where
  chars Bold = ['*']
  chars Italic = ['/']
  chars (WholeLine (Header _)) = ['#']
  chars (WholeLine Title) = ['@']
  chars NewLine = ['\n']
  -- chars Title = ['@']
  chars _ = []

  toHTML Bold str = "<b>" ++ str ++ "</b>"
  toHTML Italic str = "<i>" ++ str ++ "</i>"
  toHTML (Header a) str = "<h" ++ show a ++ ">" ++ str ++ "</h" ++ show a ++ ">"
  toHTML NewLine _ = "<br/>"
  toHTML Title str = "<title>" ++ str ++ "</title>"
  toHTML _ str = str

isIden :: Iden -> (Char -> Bool)
isIden t x = x `elem` chars t

getIdenType :: Char -> Iden
getIdenType x
  | isIden Bold x = Bold
  | isIden Italic x = Italic
  | isIden (Header 1) x = Header 1
  | isIden NewLine x = NewLine
  | otherwise = Basic

splitParagraphs :: String -> [String]
splitParagraphs = iter [] ""
  where
    append acc str rest = iter (acc ++ [str]) "" rest

    iter acc str ('\n' : ('\n' : ('\n' : xs))) = append acc (str ++ ['\n']) xs
    iter acc str (x : xs) = iter acc (str ++ [x]) xs
    iter acc str [] = acc ++ [str ++ ['\n']]

lexer :: String -> [[IdenString]]
lexer str = map (iter [] (Basic, "")) (splitParagraphs str)
  where
    pushIden acc (Basic, "") = acc
    pushIden acc cur = acc ++ [cur]

    iter acc (Basic, "") [] = acc
    iter acc cur [] = acc ++ [cur]
    iter acc (typ, str) (x : rest) =
      case (typ, typ') of
        (Basic, Basic) -> iter acc (typ, str ++ [x]) rest
        (NewLine, NewLine) -> iter acc (typ, str ++ [x]) rest
        (Header h, Header _) -> iter acc (Header $ h + 1, str ++ [x]) rest
        _ -> iter (pushIden acc (typ, str)) (typ', [x]) rest
      where
        typ' = getIdenType x

parser :: [[IdenString]] -> [[IdenString]]
parser = map (iter [])
  where
    forwardIter typ acc [] = ((Error "Unmatched type", show typ), acc)
    forwardIter typ acc (x : rest) =
      case (typ, typ') of
        (Header _, NewLine) -> ((typ, concatMap snd acc), rest)
        (Header _, Header _) -> forwardIter typ (acc ++ [x]) rest
        _ ->
          if typ == typ'
            then ((typ, concatMap snd acc), rest)
            else forwardIter typ (acc ++ [x]) rest
      where
        typ' = fst x

    iter acc [] = acc
    iter acc ((typ, str) : rest) =
      if typ == Basic || typ == NewLine
        then iter (acc ++ [(typ, str)]) rest
        else iter acc' rest'
      where
        fi = forwardIter typ [] rest
        acc' = acc ++ [fst fi]
        rest' = snd fi

checkParser :: [[IdenString]] -> IO ()
checkParser p =
  forM_ (zip [0 ..] p) f
  where
    f (i, p) = do
      let d = getLoc p
      if null d
        then return ()
        else print $ "Error in paragraph " ++ show (i + 1) ++ ", at char " ++ (show . snd . head $ d)
    getLoc =
      dropWhile fst
        . scanl
          ( \(_, loc) (typ, str) ->
              case typ of
                Error _ -> (False, loc)
                _ -> (True, loc + length str)
          )
          (True, 0)

compile :: [[IdenString]] -> String
compile = concatMap (iter "" "")
  where
    endParagraph acc str = acc ++ str ++ "</p>"

    iter acc "" [] = acc
    iter acc str [] = endParagraph acc str
    iter acc str ((typ, str') : rest) =
      if null str
        then case typ of
          Header _ -> iter (acc ++ toHTML typ str') "" rest
          _ -> iter acc ("<p>" ++ toHTML typ str') rest
        else case typ of
          Header _ -> iter (endParagraph acc str ++ toHTML typ str') "" rest
          _ -> iter acc (str ++ toHTML typ str') rest

repeatIO :: Int -> a -> a
repeatIO 1 f = f
repeatIO n f = repeatIO (n - 1) f

main :: IO ()
main = do
  t <- readFile "input.mks"
  let l = lexer t
  let p = parser l
  let c = compile p
  print t
  print . splitParagraphs $ t
  repeatIO 2 (putChar '\n')
  print l
  repeatIO 2 (putChar '\n')
  print p
  repeatIO 2 (putChar '\n')
  checkParser p
  repeatIO 2 (putChar '\n')
  print c
  writeFile "test.html" ("<html><body>" ++ c ++ "</body></html>")