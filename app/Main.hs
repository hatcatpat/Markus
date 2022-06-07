module Main where

import Control.Monad (forM_)

(+++) :: String -> String -> String
(+++) a b = a ++ " " ++ b

data WholeLineIdens = Header {headerDepth :: Int} | Title deriving (Show, Eq)

data ListElemTypes = Ordered | Unordered deriving (Show, Eq)

data LinkTypes = Image | HyperLink deriving (Show, Eq)

data Iden
  = Basic
  | Bold
  | Italic
  | NewLine
  | Tab {depth :: Int}
  | WholeLine {wholeLineType :: WholeLineIdens}
  | LinkOpen {linkType :: LinkTypes}
  | LinkClose
  | LinkJoin
  | Exc
  | ListElem {listType :: ListElemTypes, depth :: Int}
  | List {listType :: ListElemTypes, depth :: Int, listElems :: [(Iden, String)]}
  | Link {linkType :: LinkTypes, url :: String}
  | Error {error :: String}
  deriving (Show, Eq)

class Idenable a where
  chars :: a -> String
  toHTML :: a -> String -> String

instance Idenable Iden where
  chars Bold = ['*']
  chars Italic = ['/']
  chars (WholeLine (Header _)) = ['|']
  chars (WholeLine Title) = ['@']
  chars NewLine = ['\n']
  chars (Tab _) = ['\t']
  chars (LinkOpen HyperLink) = ['[']
  chars Exc = ['!']
  chars LinkClose = [']']
  chars (ListElem Ordered _) = ['#']
  chars (ListElem Unordered _) = ['-']
  chars _ = []

  toHTML Bold str = "<b>" ++ str ++ "</b>"
  toHTML Italic str = "<i>" ++ str ++ "</i>"
  toHTML NewLine _ = "<br/>"
  toHTML (WholeLine (Header a)) str = "<h" ++ show a ++ ">" ++ str ++ "</h" ++ show a ++ ">"
  toHTML (WholeLine Title) str = "<title>" ++ str ++ "</title>"
  toHTML (Link t "") str = toHTML (Link t str) str
  toHTML (Link HyperLink url) str = "<a href=\'" ++ url ++ "\'>" ++ str ++ "</a>"
  toHTML (Link Image url) str = "<img src=\'" ++ url ++ "\' alt=\'" ++ str ++ "\' />"
  toHTML (ListElem _ _) str = ""
  toHTML (List o d e) _ = makeList (List o d e)
  toHTML (Tab 0) str = "&ensp;" ++ str
  toHTML (Tab n) str = concat (replicate (n + 1) "&ensp;") ++ str
  toHTML _ str = str

isIden :: Iden -> (Char -> Bool)
isIden t x = x `elem` chars t

getIdenType :: Char -> Iden
getIdenType x
  | isIden Bold x = Bold
  | isIden Italic x = Italic
  | isIden (WholeLine (Header 1)) x = WholeLine $ Header 1
  | isIden (WholeLine Title) x = WholeLine Title
  | isIden NewLine x = NewLine
  | isIden (Tab 0) x = Tab 0
  | isIden (LinkOpen HyperLink) x = LinkOpen HyperLink
  | isIden Exc x = Exc
  | isIden LinkClose x = LinkClose
  | isIden (ListElem Unordered 0) x = ListElem Unordered 0
  | isIden (ListElem Ordered 0) x = ListElem Ordered 0
  | otherwise = Basic

makeList :: Iden -> String
makeList (List o _ e) =
  "<" ++ listTag o ++ ">"
    ++ iter "" "" e
    ++ ("</" ++ listTag o ++ ">")
  where
    listTag Ordered = "ol"
    listTag Unordered = "ul"
    makeListElem str = "<li>" ++ str ++ "</li>"

    iter final acc [] = final ++ makeListElem acc
    iter final acc ((typ, str) : xs) =
      case typ of
        ListElem _ _ -> iter (if null acc then final else final ++ makeListElem acc) "" xs
        _ -> iter final (acc ++ toHTML typ str) xs
makeList x = ""

splitParagraphs :: String -> [String]
splitParagraphs = iter [] ""
  where
    append acc str rest = iter (acc ++ [str]) "" rest

    iter acc str ('\n' : '\n' : xs) = append acc str xs
    iter acc str (x : xs) = iter acc (str ++ [x]) xs
    iter acc str [] = acc ++ [str ++ ['\n']]

lexer :: String -> [[(Iden, String)]]
lexer str =
  map
    ( iter [] (Basic, "")
        . (\str -> if last str == '\n' then init str else str)
    )
    (splitParagraphs str)
  where
    pushIden acc (Basic, "") = acc
    pushIden acc cur = acc ++ [cur]

    iter acc (Basic, "") [] = acc
    iter acc cur [] = acc ++ [cur]
    iter acc (typ, str) (x : rest) =
      case (typ, typ') of
        (Basic, Basic) -> iter acc (typ, str ++ [x]) rest
        (NewLine, NewLine) -> iter acc (typ, str ++ [x]) rest
        (WholeLine (Header h), WholeLine (Header _)) ->
          iter acc (WholeLine (Header $ h + 1), str ++ [x]) rest
        (LinkClose, LinkOpen _) -> iter acc (LinkJoin, str ++ [x]) rest
        (Exc, LinkOpen _) -> iter acc (LinkOpen Image, str ++ [x]) rest
        (Tab d, Tab _) -> iter acc (Tab $ d + 1, str ++ [x]) rest
        (Tab d1, ListElem o d2) -> iter acc (ListElem o (d1 + d2 + 1), str ++ [x]) rest
        _ -> iter (pushIden acc (typ, str)) (typ', [x]) rest
      where
        typ' = getIdenType x

parser :: [[(Iden, String)]] -> [[(Iden, String)]]
parser = map (iter [])
  where
    forwardIterFinish typ acc rest = ((typ, concatMap snd acc), rest)
    forwardIterContinue cur acc x rest = forwardIter cur (acc ++ [x]) rest

    -- TABS
    forwardIter (Tab d, _) acc [] = forwardIterFinish (Tab d) acc []
    -- WHOLE LINES
    forwardIter (WholeLine typ, _) acc [] = forwardIterFinish (WholeLine typ) acc []
    -- LISTS
    forwardIter (List o d _, _) acc [] = ((List o d acc, ""), [])
    forwardIter (ListElem o d, str) acc rest =
      forwardIter (List o d [], "") (acc ++ [(ListElem o d, str)]) rest
    forwardIter (List o d _, _) acc ((typ', str') : rest) =
      case typ' of
        ListElem o' d' | d < d' -> forwardIter (List o d [], "") acc' rest'
          where
            fi = forwardIter (List o' d' [], "") [] rest
            acc' = acc ++ [fst fi]
            rest' = snd fi
        ListElem o' d' | d > d' -> ((List o d acc, ""), (typ', str') : rest)
        _ -> forwardIter (List o d [], "") (acc ++ [(typ', str')]) rest
    -- LINKS
    forwardIter (LinkClose, str) acc rest =
      ((Error $ "Unexpected Link" +++ show LinkClose, str), acc ++ rest)
    forwardIter (LinkJoin, str) acc rest =
      ((Error $ "Unexpected Link" +++ show LinkJoin, str), acc ++ rest)
    forwardIter (LinkOpen t, str) acc rest = forwardIter (Link t "", str) acc rest
    forwardIter (Link t url, str) acc ((typ', str') : rest) =
      case typ' of
        LinkOpen t' -> ((Error $ "Unexpected Link" +++ show (LinkOpen t'), str), acc ++ rest)
        LinkClose -> forwardIterFinish (Link t url) acc rest
        LinkJoin | null url -> forwardIterContinue (Link t $ concatMap snd acc, str) [] (typ', "") rest
        _ -> forwardIterContinue (Link t url, str) acc (typ', str') rest
    -- GENERAL
    forwardIter (typ, str) acc [] = ((Error $ "Unmatched type" +++ show typ, str), acc)
    forwardIter (typ, str) acc ((typ', str') : rest) =
      case (typ, typ') of
        (WholeLine _, NewLine) -> forwardIterFinish typ acc rest
        (WholeLine _, WholeLine _) -> forwardIterContinue (typ, str) acc (typ', str') rest
        _ ->
          if typ == typ'
            then forwardIterFinish typ acc rest
            else forwardIterContinue (typ, str) acc (typ', str') rest

    iter acc [] = acc
    iter acc ((typ, str) : rest) =
      if typ == Basic || typ == NewLine
        then iter (acc ++ [(typ, str)]) rest
        else iter acc' rest'
      where
        fi = forwardIter (typ, str) [] rest
        acc' = acc ++ [fst fi]
        rest' = snd fi

checkParser :: [[(Iden, String)]] -> IO ()
checkParser p = forM_ (zip [0 ..] p) f
  where
    f (i, p) = iter i p 0
      where
        iter _ [] acc = return ()
        iter i ((Error msg, str) : xs) acc = do
          print $
            "Error:" +++ msg ++ ", in paragraph" +++ show (i + 1) ++ ", char" +++ show acc
          iter i xs (acc + length str)
        iter i ((typ, str) : xs) acc = iter i xs (acc + length str)

compile :: [[(Iden, String)]] -> String
compile = concatMap (iter "" "")
  where
    endParagraph acc str = acc ++ str ++ "</p>"

    iter acc "" [] = acc
    iter acc str [] = endParagraph acc str
    iter acc str ((typ, str') : rest) =
      if null str
        then case typ of
          WholeLine _ -> iter (acc ++ toHTML typ str') "" rest
          List {} -> iter (acc ++ toHTML typ str') "" rest
          _ -> iter acc ("<p>" ++ toHTML typ str') rest
        else case typ of
          WholeLine _ -> iter (endParagraph acc str ++ toHTML typ str') "" rest
          List {} -> iter (endParagraph acc str ++ toHTML typ str') "" rest
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
  -- repeatIO 2 (putChar '\n')
  -- print c
  writeFile "test.html" ("<html><body>" ++ c ++ "</body></html>")