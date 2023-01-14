module UnicodeGen where

import Data.List
import Control.Monad

import Control.Exception
import Data.Function

import System.Directory

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

import Debug.Trace (trace)

type UnicodeRep = String
type EscapeCode = String
type LatexRep = String
type Description = String

main :: IO ()
main = do
  checkUnique
  writeFile "unicode.el" genEmacsScript
  writeFile "unicode.vim" genVimScript
  writeFile ".XCompose" genXCompose
  writeFile "DefaultKeyBindings.dict" genDict
  writeFile "latex-unicode.sed" genSedScript
  writeFile "latex-unicode-escape.sed" genSedEscapeScript
  writeFile "latex-unicode-unescape.sed" genSedUnescapeScript
  writeFile "latex-unicode-mark.sed" genSedMarkScript
  writeFile "latex-demo.tex" genLatexDemo
  writeFile "unicode-input.txt" genInputReference
  writeFile "unicode-init.coffee" genAtomInitScript
  writeFile "unicode-keymap.cson" genAtomKeymapScript
  writeFile "unicode-latex-completions.json" genAtomLatexCompletions
  writeFile "daraisinput.plist" genMacPlist
  writeFile "unicodetest.html" genUnicodeTestHtml
  -- writeFile "daraisinput.sublime-completions" genSublimeScript
  putStrLn $ unwords
    [ "unicode files generated:"
    , "unicode.el"
    , "unicode.vim"
    , "latex-unicode.sed"
    , "latex-demo.tex"
    , "unicode-input.txt"
    , "unicode-init.coffee"
    , "unicode-keymap.cson"
    , "daraisinput.plist"
    , "unicodetest.html"
    -- , "daraisinput.sublime-completions"
    ]

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  if null xs
    then []
    else take n xs : groupsOf n (drop n xs)

data LatexMode = T | M
  deriving (Eq,Ord,Show)
data LatexRepM = L LatexMode LatexRep
  deriving (Eq,Ord,Show)

data Code = Code
  { unicodeRep :: UnicodeRep
  , escapeCode :: EscapeCode
  , latexRep :: LatexRepM
  , description :: Description
  } deriving (Eq,Ord,Show)

code :: UnicodeRep -> EscapeCode -> Code
code u e = Code u e (L M "") ""

lmcode :: UnicodeRep -> EscapeCode -> LatexRep -> Code
lmcode u e l = Code u e (L M (l ++ " ")) ""

lmcodet :: UnicodeRep -> EscapeCode -> LatexRep -> Code
lmcodet u e l = Code u e (L M l) ""

ltcode :: UnicodeRep -> EscapeCode -> LatexRep -> Code
ltcode u e l = Code u e (L T (l ++ " ")) ""

ltcodet :: UnicodeRep -> EscapeCode -> LatexRep -> Code
ltcodet u e l = Code u e (L T l) ""

duplicates :: (Ord a) => [a] -> [a]
duplicates xs =
  let results = foldl' (\ m x -> Map.insertWith (+) x 1 m) Map.empty xs
  in Map.keys $ Map.filter (\ n -> n > 1) results

checkUnique :: IO ()
checkUnique = do
  let escapes = map escapeCode codes
      dups = duplicates escapes
      escapes' = map (convertUpper . escapeCode) codes
      dups' = duplicates escapes'
  when (length dups > 0) $
    throwIO $ AssertionFailed $ "duplicates!\n" ++ show dups
  when (length dups' > 0) $
    throwIO $ AssertionFailed $ "duplicates'!\n" ++ show dups'

convertUpper :: String -> String
convertUpper = concatMap convertChar
  where
    convertChar 'A' = "aaa"
    convertChar 'B' = "bbb"
    convertChar 'C' = "ccc"
    convertChar 'D' = "ddd"
    convertChar 'E' = "eee"
    convertChar 'F' = "fff"
    convertChar 'G' = "ggg"
    convertChar 'H' = "hhh"
    convertChar 'I' = "iii"
    convertChar 'J' = "jjj"
    convertChar 'K' = "kkk"
    convertChar 'L' = "lll"
    convertChar 'M' = "mmm"
    convertChar 'N' = "nnn"
    convertChar 'O' = "ooo"
    convertChar 'P' = "ppp"
    convertChar 'Q' = "qqq"
    convertChar 'R' = "rrr"
    convertChar 'S' = "sss"
    convertChar 'T' = "ttt"
    convertChar 'U' = "uuu"
    convertChar 'V' = "vvv"
    convertChar 'W' = "www"
    convertChar 'X' = "xxx"
    convertChar 'Y' = "yyy"
    convertChar 'Z' = "zzz"
    convertChar c = [c]


vimEscape :: String -> String
vimEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '|' = "\\|"
    escapeChar c = [c]

emacsEscape :: String -> String
emacsEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar '\"' = "\\\""
    escapeChar c = [c]

xComposeEscapeChar :: Char -> String
xComposeEscapeChar c = case c of
  ' ' -> wrap "space"
  '_' -> wrap "underscore"
  '-' -> wrap "minus"
  ',' -> wrap "comma"
  ':' -> wrap "colon"
  '.' -> wrap "period"
  '"' -> wrap "quotedbl"
  '(' -> wrap "parenleft"
  ')' -> wrap "parenright"
  '[' -> wrap "bracketleft"
  ']' -> wrap "bracketright"
  '{' -> wrap "braceleft"
  '}' -> wrap "braceright"
  '@' -> wrap "at"
  '*' -> wrap "asterisk"
  '/' -> wrap "slash"
  '\'' -> wrap "apostrophe"
  '\\' -> wrap "backslash"
  '&' -> wrap "ampersand"
  '#' -> wrap "numbersign"
  '`' -> wrap "grave"
  '^' -> wrap "asciicircum"
  '<' -> wrap "less"
  '=' -> wrap "equal"
  '>' -> wrap "greater"
  '|' -> wrap "bar"
  '~' -> wrap "asciitilde"
  '$' -> wrap "dollar"
  c -> wrap [c]
  where
    wrap s = "<" ++ s ++ "> "

dictEscape :: String -> String
dictEscape = concatMap dictEscapeChar

dictEscapeChar :: Char -> String
dictEscapeChar '"' = "\\\""
dictEscapeChar '\\' = "\\\\"
dictEscapeChar '^' = "\\\\^"
dictEscapeChar c = [c]

sedEscape :: String -> String
sedEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '/'  = "\\/"
    escapeChar '.'  = "\\."
    escapeChar '^'  = "\\^"
    escapeChar '$'  = "\\$"
    escapeChar '*'  = "\\*"
    escapeChar '+'  = "\\+"
    escapeChar '?'  = "\\?"
    escapeChar '('  = "\\("
    escapeChar ')'  = "\\)"
    escapeChar '['  = "\\["
    escapeChar ']'  = "\\]"
    escapeChar '{'  = "\\{"
    escapeChar '}'  = "\\}"
    escapeChar '\\' = "\\\\"
    escapeChar '|'  = "\\|"
    escapeChar c    = [c]

latexEscape :: String -> String
latexEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '&' = "\\&"
    escapeChar '%' = "\\%"
    escapeChar '$' = "\\$"
    escapeChar '#' = "\\#"
    escapeChar '_' = "\\_"
    escapeChar '{' = "\\{"
    escapeChar '}' = "\\}"
    escapeChar '~' = "\\textasciitilde "
    escapeChar '^' = "\\textasciicircum "
    escapeChar '\\' = "\\textbackslash "
    escapeChar '-' = "{-}"
    escapeChar c = [c]

jsonEscape :: String -> String
jsonEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = [c]

csonEscape :: String -> String
csonEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar '\'' = "\\'"
    escapeChar c = [c]

sublimeEscape :: String -> String
sublimeEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = [c]


xmlEscape :: String -> String
xmlEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '\'' = "&apos;"
    escapeChar '"' = "&quot;"
    escapeChar c = [c]

shellQuoteEscape :: String -> String
shellQuoteEscape = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar '`' = "\\`"
    escapeChar '\'' = "'\"'\"'"
    escapeChar c = [c]

genVimScript :: String
genVimScript = do
  code <- codes
  command code
  where
    command :: Code -> String
    command (Code u e _ _) = "noremap! " ++ vimEscape ("\\" ++ e) ++ " " ++ vimEscape u ++ "\n"

genEmacsScript :: String
genEmacsScript = concat $ intersperse "\n"
  [ "(quail-define-package"
  , " \"darais\""
  , " \"UTF-32\""
  , " \"DaraisInput\""
  , " t"
  , " \"David Darais's personal input codes\""
  , " nil t nil nil nil nil nil nil nil nil t)"
  , "(quail-define-rules"
  , do
      code <- codes
      command code
    ++ ")"
  ]
  where
    command :: Code -> String
    command (Code u e _ _) = " (\"" ++ emacsEscape ("\\" ++ e) ++ "\" [\"" ++ emacsEscape u ++ "\"])" ++ "\n"

genXCompose :: String
genXCompose = do
  code <- codes
  command code
    where
      command :: Code -> String
      command (Code u e _ _) =
        "<Multi_key> " ++ concatMap xComposeEscapeChar e ++ confirmation e ++ ": \"" ++ u ++ "\"\n"
      -- At least WinCompose is eager, so overlapping prefixes must be
      -- distinguished by confirming the shorter prefix with a space
      confirmation :: String -> String
      confirmation code =
        if any (\(Code _ c _ _) -> (code `isPrefixOf` c) && (code /= c)) codes
          then "<space> "
          else ""

data Trie
  = InsertText UnicodeRep
  | Node (Map.Map Char Trie)

-- For ("and", '꘍'), builds the Trie {'a': {'n': {'d': '꘍'}}}.
singletonTrie :: EscapeCode -> UnicodeRep -> Trie
singletonTrie [] r = InsertText r
singletonTrie (c:cs) r = Node (Map.singleton c (singletonTrie cs r))

mergeTries :: Trie -> Trie -> Trie
mergeTries (InsertText a) (InsertText b) = error (a <> " and " <> b <> " have the same input string")
-- When a command has the same prefix as a longer command, we indicate we want
-- the shorter one by pressing '<SPACE>'.
mergeTries (InsertText r) t@(Node _) = mergeTries (singletonTrie " " r) t
mergeTries t@(Node _) (InsertText r) = mergeTries t (singletonTrie " " r)
mergeTries (Node m1) (Node m2) = Node (Map.unionWith mergeTries m1 m2)

insertCode :: Code -> Trie -> Trie
insertCode c = go (escapeCode c) (unicodeRep c)
  where
    go :: EscapeCode -> UnicodeRep -> Trie -> Trie
    go [] r _ = error ("Empty input for output " <> r)
    go (c:cs) r (InsertText r') = error "Working trie should be a Node"
    go (c:cs) r (Node m) = Node (Map.insertWith mergeTries c (singletonTrie cs r) m)

indent :: String -> String
indent = unlines . (("  " <>) <$>) . lines

trieToString :: Trie -> String
trieToString (InsertText r) = "(\"insertText:\", \"" <> dictEscape r <> "\")"
trieToString (Node m) = "{\n" <> indent (intercalate "\n" (map entryToString (Map.assocs m))) <> "}"
  where
    entryToString :: (Char, Trie) -> String
    entryToString (c, t) = "\"" <> dictEscapeChar c <> "\" = " <> trieToString t <> ";"

genDict :: String
genDict =
  let trie = foldr insertCode (Node mempty) codes in
  trieToString (Node (Map.singleton '§' trie))

genSedScript :: String
genSedScript = do
  code <- codes
  command code
    where
      command :: Code -> String
      command (Code u e (L _ l) _) =
        if l == ""
           then ""
           else "s/" ++ sedEscape u ++ "/" ++ sedEscape l ++ "/g\n"

genAtomLatexCompletions :: String
genAtomLatexCompletions =
  let first : rest = codes
  in
  concat $ intersperse "\n"
  [ "{"
  , entry first
  , do code <- rest
       concat $
         [ ",\n"
         , entry code
         ]
  , "}"
  ]
  where
    entry :: Code -> String
    entry (Code u e _ _) = concat
      [ "  \""
      , jsonEscape e
      , "\": \""
      , jsonEscape u
      , "\""
      ]


genAtomInitScript :: String
genAtomInitScript = do
  code <- codes
  command code
  where
    command :: Code -> String
    command (Code u e _ _) = concat
      [ "atom.commands.add 'atom-text-editor', 'custom:insert-"
      , csonEscape u
      , "': -> atom.workspace.getActiveTextEditor()?.insertText('"
      , csonEscape u
      , "')\n"
      ]

genAtomKeymapScript :: String
genAtomKeymapScript = concat $ intersperse "\n"
  [ "'atom-text-editor':"
  , do code <- codes
       command code
  ]
  where
    command :: Code -> String
    command (Code u e _ _) = concat
      [ "  '\\\\ "
      , csonEscape $ intersperse ' ' e
      , "': 'custom:insert-"
      , csonEscape u
      , "'\n"
      ]

genMacPlist :: String
genMacPlist =
    concat $ intersperse "\n"
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
    , "<plist version=\"1.0\">"
    , "<array>"
    , concat $ intersperse "\n" $ do
        code <- codes
        return $ command code
    , "</array>"
    , "</plist>"
    ]
  where
    command :: Code -> String
    command (Code u e _ _) = concat $ intersperse "\n"
      [ "  <dict>"
      , "    <key>phrase</key>"
      , "    <string>" ++ xmlEscape u ++ "</string>"
      , "    <key>shortcut</key>"
      , "    <string>\\" ++ xmlEscape e ++ "</string>"
      , "  </dict>"
      ]

genSublimeScript :: String
genSublimeScript = concat $ intersperse "\n"
  [ "{ \"scope\": \"text - source\""
  , ", \"completions\": [ \"daraisinput\""
  , concat $ intersperse "\n" $ do
      code <- codes
      return $ command code
  , "  ]"
  , "}"
  ]
  where
    command :: Code -> String
    command (Code u e _ _) = concat $ intersperse "\n"
      [ "  , { \"trigger\": \"\\\\" ++ sublimeEscape e ++ "\""
      , "    , \"contents\": \"" ++ sublimeEscape u ++ "\""
      , "    }"
      ]

quoteL :: String
quoteL = "⧘"

quoteR :: String
quoteR = "⧙"

quoteH :: String
quoteH = "⁂"

genSedEscapeScript :: String
genSedEscapeScript = do
  code <- codes
  command code
    where
      command :: Code -> String
      command (Code u e l _) = concat
        [ "s/" 
        , quoteL 
        , sedEscape u 
        , quoteR 
        , "/" 
        , quoteL 
        , sedEscape e 
        , quoteR 
        , "/g\n"
        ]

genSedUnescapeScript :: String
genSedUnescapeScript = do
  code <- codes
  command code
  where
    command :: Code -> String
    command (Code u e l _) = concat
      [ "s/" 
      , quoteL 
      , sedEscape e 
      , quoteR 
      , "/" 
      , sedEscape u 
      , "/g\n"
      ]


-- :L
-- s/(⸨⁂[^⸨⸩⁂⧘⧙]*)([↑⇈])([^⸨⸩⁂]*⁂⸩)/\1⧘\2⧙\3/g
-- tL
genSedMarkScript :: String
genSedMarkScript = concat
  [ ""
  -- , ":MB\n"
  -- , "s/(⸨⁂[^⸨⸩⁂⧘⧙]*)([^0-~[:space:]()⸨⸩⁂⧘⧙])([^⸨⸩⁂]*⁂⸩)/\\1⧘\\2⧙\\3/g\n"
  -- -- , Set.toList $ Set.fromList $ do 
  -- --         code <- codes
  -- --         let s = command3 code
  -- --         filter (\ c -> Char.ord c >= 128) s
  -- -- --       f1 = if ']' `Set.member` chars then (\ s -> "]" ++ s) else id
  -- -- --       f2 = if '[' `Set.member` chars then (\ s -> "[" ++ s) else id
  -- -- --       f3 = if '-' `Set.member` chars then (\ s -> s ++ "-") else id
  -- -- --       chars' = chars `Set.difference` Set.fromList "[]-"
  -- -- --   in foldr (.) id [f1,f2,f3] $ Set.toList chars'
  -- -- , "])([^⸨⸩⁂]*⁂⸩)/\\1⧘\\2⧙\\3/g\n"
  -- , "tMB\n"
  -- , "s/⸨⁂|⁂⸩//g\n"
  , "/^" ++ quoteH ++ "⁅/,/^" ++ quoteH ++ "⁆/{\n"
  , "  /^" ++ quoteH ++ "⁅|" ++ quoteH ++ "⁆/d\n"
  , do code <- codes
       command2 code
  , "}\n"
  , "/^" ++ quoteH ++ "/{\n"
  , do code <- codes
       command1 code
  , "}\n"
  , concat
      [ "s/^"
      , quoteH
      , "\\ //\n"
      ]
  , concat
      [ "s/^"
      , quoteH
      , "//\n"
      ]
  ]
  where
    command3 :: Code -> String
    command3 (Code u e l _) =
      if u `elem` [quoteH,quoteL,quoteR,"\\","⸨","⸩","\n","\r"]
         then ""
         else u
    command2 :: Code -> String
    command2 (Code u e l _) =
      if u == quoteH || u == quoteL || u == quoteR || u == "\\"
         then ""
         else concat
           [ "  s/" 
           , sedEscape u 
           , "/" 
           , quoteL 
           , sedEscape u 
           , quoteR 
           , "/g\n"
           ]

    command1 :: Code -> String
    command1 (Code u e l _) =
      if u == quoteH || u == quoteL || u == quoteR || u == "\\"
         then ""
         else concat
           [ "  s/" 
           , sedEscape u 
           , "/" 
           , quoteL 
           , sedEscape u 
           , quoteR 
           , "/g\n"
           ]

genLatexDemo :: String
genLatexDemo = concat
  [ "\\begin{longtable}{ll}\n"
  , middle
  , "\\end{longtable}"
  ]
  where
    middle :: String
    middle = do
      code <- codes
      command code
        where
          command :: Code -> String
          command (Code u e (L m l) _) =
            if l == ""
              then ""
              else case m of
                T -> concat ["\\texttt{",latexEscape e,"}&",l,"\\\\\n"]
                M -> concat ["\\texttt{",latexEscape e,"}&${}",l,"{}$\\\\\n"]

genInputReference :: String
genInputReference = do
  code <- codes
  command code
    where
      command :: Code -> String
      command (Code u e l _) =
        skip pad maxCodeWidth u ++ "  " ++ e ++ "\n"
      maxCodeWidth :: Int
      maxCodeWidth = maximum $ map (length . unicodeRep) codes
      pad :: Int -> String -> String
      pad i s = let padding = max 0 (i - length s) in s ++ replicate padding ' '
      skip f y x = x

genUnicodeTestHtml :: String
genUnicodeTestHtml = concat $ intersperse "\n" $
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<link rel=\"stylesheet\" type=\"text/css\" href=\"unicodetest.css\">"
  , "<title>unicode test</title>"
  , "</head>"
  , "<body>"
  , "<table>"
  , table
  , "</table>"
  , "</body>"
  , "</html>"
  ]
  where
    table = concat $ intersperse "\n" $ do
      code <- codes
      return $ row code
    row (Code u e l _) = concat $ intersperse "\n" $
      [ "<tr>"
      , concat $ intersperse "\n" $ do
          n <- [1..8]
          return $ concat $ intersperse "\n" $
            [ "<td>"
            , "<div class=\"test" ++ show n ++ "\">"
            , xmlEscape u ++ "  " ++ xmlEscape e
            , "</div>"
            , "</td>"
            ]
      , "</tr>"
      ]

codes :: [Code]
codes =
  -- Backslash
  [ code "\\"  "\\"

  -- Arrows
  -- - normal
  , lmcode "↑" "u" "\\uparrow"
  , lmcode "→" "r" "\\rightarrow"
  , lmcode "↓" "d" "\\downarrow"
  , lmcode "←" "l" "\\leftarrow"
  , lmcode "↗" "ur" "\\nearrow"
  , lmcode "↘" "dr" "\\searrow"
  , code "↙" "dl"
  , code "↖" "ul"
  , code "↕" "ud-"
  , lmcode "↔" "rl-" "\\leftrightarrow"
  , code "⇈" "uu"
  , lmcode "⇉" "rr" "\\rightrightarrows"
  , code "⇊" "dd"
  , code "⇇" "ll"
  , code "⇅" "ud"
  , code "⇵" "du"
  , lmcode "⇄" "rl" "\\rightleftarrows"
  , code "⇆" "lr"
  -- - open
  , lmcode "⇑" "u=" "\\Uparrow"
  , lmcode "⇒" "r=" "\\Rightarrow"
  , lmcode "⇓" "d=" "\\Downarrow"
  , lmcode "⇐" "l=" "\\Leftarrow"
  , code "⇕" "ud="
  , lmcode "⇔" "rl=" "\\Leftrightarrow"
  , code "⇏" "r=/"
  , code "⇍" "l=/"
  , code "⇎" "rl=/"
  , code "⇗" "ur="
  , code "⇘" "dr="
  , code "⇙" "dl="
  , code "⇖" "ul="
  , lmcode "⇛" "r==" "\\Rrightarrow"
  , lmcode "⇚" "l==" "\\Lleftarrow"
  -- - long
  , lmcode "⟹" "impr" "\\implies"
  , lmcode "⟸" "impl" "\\impliedby"
  , lmcode "⟺" "iff" "\\iff"
  -- - closed
  , code "⇧" "u|" -- "\\upwhitearrow"
  , code "⇨" "r|" -- "\\rightwhitearrow"
  , code "⇩" "d|" -- "\\downwhitearrow"
  , code "⇦" "l|" -- "\\leftwhitearrow"
  , code "⬄" "rl|"
  , lmcode "⇰"  "r|=" "\\Mapsto"
  -- - partial
  , lmcode "⇀" "r\\\\" "\\rightharpoonup"
  -- - maps
  , code "↥" "u|-"
  , lmcode "↦" "r|-" "\\mapsto"
  , code "↧" "d|-"
  , code "↤" "l|-"
  -- hook
  , lmcode "↪" "rh" "\\hookrightarrow"
  , lmcode "↩" "lh" "\\hookleftarrow"
  , code "↬" "rc"
  , code "↫" "lc"
  -- - ending bar
  , code "⇥" "r>|" -- "\\RightArrowBar"
  , code "⇤" "l>|" -- "\\LeftArrowBar"
  -- - double
  , code "↟" "u>>"
  , code "↠" "r>>" -- "\\twoheadrightarrow"
  , code "↡" "d>>" -- "\\twoheaduparrow"
  , code "↞" "l>>" -- "\\twoheadleftarrow"
  -- - triangle
  , lmcode "⇾" "r|>" "\\rightarrowtriangle"
  , lmcode "⇽" "l|>" "\\leftarrowtriangle"
  , lmcode "⇿" "rl|>" "\\leftrightarrowtriangle"
  -- - squiggle
  , lmcode "↝" "r~" "\\rightsquigarrow"
  , code "↜" "l~"
  , code "↭" "rl~"
  -- tail
  , lmcode "↣" "rv" "\\rightarrowtail"
  , lmcode "↢" "lv" "\\leftarrowtail"
  -- - double squiggle
  , code "⇝" "r~~" -- "\\rightsquigarrow"
  , code "⇜" "l~~" -- "\\leftsquigarrow"
  -- - dotted
  , code "⇡" "u."
  , lmcode "⇢" "r." "\\dashrightarrow"
  , code "⇣" "d."
  , lmcode "⇠" "l." "\\dashleftarrow"
  -- - stroke
  , code "↛" "r/"
  , code "↚" "l/"
  , code "↮" "rl/"
  -- - lolli
  , lmcode "⊸" "ro" "\\multimap"
  , code "⟜" "lo"
  , code "⧟" "rlo"
  -- - zagged
  , lmcode "↯" "zd" "\\lightning"
  , code "↻" "cw"
  , code "↺" "ccw"

  -- Brackets
  -- - paren
  , lmcode "⟮" "(" "\\lgroup"
  , lmcode "⟯" ")" "\\rgroup"
  , code "⸨" "(("
  , code "⸩" "))"
  , code "⦇" "(|" -- "\\limg"
  , code "⦈" ")|" -- "\\rimg"
  , code "⦅" "c(" -- "\\Lparen"
  , code "⦆" "c)" -- "\\Rparen"
  , code "❪" "b("
  , code "❫" "b)"
  , code "❨" "B("
  , code "❩" "B)"
  , code "⏜" "u)"
  , code "⏝" "d)"
  , code "⌢" "fr"
  , code "⌣" "sm"
  -- - angle
  , lmcode "⟨" "<" "\\langle"
  , lmcode "⟩" ">" "\\rangle"
  , code "⟪" "<<" -- "\\lang"
  , code "⟫" ">>" -- "\\rang"
  , code "⦉" "<|" -- "\\lblot"
  , code "⦊" ">|" -- "\\rblot"
  , code "⦑" "<." -- "\\langledot"
  , code "⦒" ">." -- "\\rangledot"
  , code "❬" "b<"
  , code "❭" "b>"
  , code "«" "<\""
  , code "»" ">\""
  , code "‹" "<'"
  , code "›" ">'"
  , code "⦓" "(<"
  , code "⦔" ")>"
  , code "⦕" "((>"
  , code "⦖" "))<"
  , code "⸦" "u<"
  , code "⸧" "u>"
  , code "᚜" "f<"
  , code "᚛" "f>"
  , code "⸜" "_<"
  , code "⸝" "_>"
  , code "⸌" "^<"
  , code "⸍" "^>"
  -- , code "⸂" "^L<"
  -- , code "⸃" "^L>"
  -- , code "⸄" "^D<"
  -- , code "⸅" "^D>"
  -- , code "⸉" "^S<"
  -- , code "⸊" "^S>"
  -- - square
  , code "⦗" "["
  , code "⦘" "]"
  , lmcode "⟦" "[[" "\\llbracket"
  , lmcode "⟧" "]]" "\\rrbracket"
  , code "⟬" "[|"
  , code "⟭" "]|"
  , code "⁅" "e["
  , code "⁆" "e]"
  , lmcode "⌈" "tL" "\\lceil"
  , lmcode "⌉" "tR" "\\rceil"
  , lmcode "⌊" "bL" "\\lfloor"
  , lmcode "⌋" "bR" "\\rfloor"
  , code "⸢" "tl"
  , code "⸣" "tr"
  , code "⸤" "bl"
  , code "⸥" "br"
  -- - curly
  , code "⦃" "{|"
  , code "⦄" "}|"
  , code "⎨" "{"
  , code "⎬" "}"
  , code "⧘" "z{"
  , code "⧙" "z}"
  , code "⧚" "z{{"
  , code "⧛" "z}}"
  , lmcode "❴" "b{" "\\{"
  , lmcode "❵" "b}" "\\}"
  , lmcode "⟅" "s{" "\\lbag"
  , lmcode "⟆" "s}" "\\rbag"

  -- Orders
  -- standard
  , code "≮" "</"
  , lmcode "≤" "<=" "\\leq"
  , lmcode "≥" ">=" "\\geq"
  , lmcode "⋚" "<=>=" "\\lesseqgtr"
  , code "≲" "<~"
  , code "≳" ">~"
  , code "⩻" "<?"
  , code "⩼" ">?"
  , code "⪥" "><"
  , code "⪤" "><x"
  , lmcode "≪" "<2" "\\ll"
  , lmcode "≫" ">2" "\\gg"
  , code "⋘" "<3"
  , code "⋙" ">3"
  , code "≫=" ">>="
  , code "≪=" "<<="
  , code "⊻" "xo|"
  , code "⊼" "nand"
  , code "⊽" "nor"
  -- standard thick (h_)
  , code "⪡" "h<"
  , code "⪢" "h>"
  , code "⩔" "hor"
  , code "⩓" "hand"
  -- double thick (H_)
  , code "⩕" "Hand"
  , code "⩖" "Hor"
  -- standard circ (_o)
  , code "⩹" "<o"
  , code "⩺" ">o"
  -- standard dot (d_)
  , code "⋖" "d<"
  , code "⋗" "d>"
  , code "⩿" "d<="
  , code "⪀" "d>="
  , code "⟇" "dor"
  , code "⟑" "dand"
  -- standard split (m_)
  , code "⪪" "m<"
  , code "⪫" "m>"
  , code "⪬" "m<="
  , code "⪭" "m>="
  , code "⩤" "m<|"
  , code "⩥" "m>|"
  , code "⩚" "mand"
  , code "⩛" "mor"
  -- standard curvy (e_)
  , lmcode "≺" "e<" "\\prec"
  , code "≻" "e>" -- "\\suc"
  , lmcode "⪯" "e<-" "\\preceq"
  , code "⪰" "e>-" -- "\\suceq"
  , lmcode "≼" "e<=" "\\preccurlyeq"
  , code "≽" "e>=" -- "\\succurlyeq"
  , lmcode "⋎" "eor" "\\curlyvee"
  , lmcode "⋏" "eand" "\\curlywedge"
  , code "⪻" "e<<"
  , code "⪼" "e>>"
  -- standard  triangle (t_)
  , lmcode "⊴" "t<=" "\\trianglelefteq"
  , lmcode "⊵" "t>=" "\\trianglerighteq"
  , code "⊲" "t<"
  , code "⊳" "t>"
  , code "⧎" "t<>"
  , code "⧏" "t<|"
  , code "⧐" "t>|"
  , lmcode "⋬" "t<=/" "\\ntrianglelefteq"
  , lmcode "⋭" "t>=/" "\\ntrianglerighteq"
  -- standard closed triangle (ct_)
  , code "⪦" "ct<"
  , code "⪧" "ct>"
  , code "⪨" "ct<="
  , code "⪩" "ct>="
  , code "⌔" "ctor"
  -- standard y (y_)
  , code "⧼" "y<" -- "\\lcurvyangle"
  , code "⧽" "y>" -- "\\rcurvyangle"
  -- - squigly order (Y_)
  , code "⊰" "Y<"
  , code "⊱" "Y>"
  -- subset (s_)
  , lmcode "⊂" "s<" "\\subset"
  , lmcode "⊃" "s>" "\\supset"
  , lmcode "⊆" "s<=" "\\subseteq"
  , lmcode "⊇" "s>=" "\\supseteq"
  , lmcode "⊈" "s<=/" "\\not\\subseteq"
  , lmcode "⊉" "s>=/" "\\not\\supseteq"
  , code "⊊" "s</="
  , code "⊋" "s>/="
  , lmcode "⊄" "s</" "\\not\\subset"
  , lmcode "⊅" "s>/" "\\not\\supset"
  , lmcode "∪" "su" "\\cup"
  , lmcode "∩" "si" "\\cap"
  , lmcode "⋃" "sU" "\\bigcup"
  , lmcode "⋂" "sI" "\\bigcap"
  , lmcode "⊎" "su+" "\\uplus"
  , code "⨄" "sU+"
  -- subset dot (ds_)
  , code "⪽" "s<."
  , code "⪾" "s>."
  , code "⊍" "su."
  , code "⩀" "si."
  -- subset closed (cs_)
  , code "⫏" "s<|"
  , code "⫐" "s>|"
  , code "⫑" "s<=|"
  , code "⫒" "s>=|"
  , code "⩌" "su|"
  , code "⩍" "si|"
  -- subset thick (hs_)
  , code "⋐" "hs<"
  , code "⋑" "hs>"
  , code "⋓" "hsu"
  , code "⋒" "hsi"
  -- partial order (q_)
  , lmcode "⊑" "q<=" "\\sqsubseteq"
  , lmcode "⊒" "q>=" "\\sqsupseteq"
  , lmcode "⊏" "q<" "\\sqsubset"
  , lmcode "⊐" "q>" "\\sqsupset"
  , code "⋢" "q<=/" -- "\\nsqsubseteq"
  , code "⋣" "q>=/" -- "\\nsqsupseteq"
  , code "⋤" "q</=" -- "\\sqsubsetneq"
  , code "⋥" "q>/=" -- "\\sqsupsetneq"
  , lmcode "⊔" "j" "\\sqcup"
  , lmcode "⊓" "m" "\\sqcap"
  , lmcode "⨆" "J" "\\bigsqcup"
  , lmcode "⨅" "M" "\\bigsqcap"
  -- partial order thick (h_)
  , code "⩏" "hj"
  , code "⩎" "hm"
  , code "⩊" "w"
  , lmcode "∑" "sum" "\\sum"
  , lmcode "∏" "prod" "\\prod"

  -- Quotes
  , code "‘" "'<"
  , code "’" "'>"
  , code "‛" "'`"
  , lmcodet "“" "\"<" "``"
  , lmcodet "”" "\">" "''"
  , lmcode "„" ",," ",\\!\\!,"
  , code "‟" "\"`"
  , lmcodet "–" "--" "--"
  , lmcodet "—" "---" "---"
  , lmcode "′" "'" "^{\\prime}"
  , lmcode "″" "''" "^{\\prime\\prime}"
  , lmcode "‴" "'''" "^{\\prime\\prime\\prime}"
  , code "‵" "`"
  , code "‶" "``"
  , code "‷" "```"

  -- Operators
  , lmcode "⋅" "." "\\cdotp"
  , code "∙" ".bu"
  , lmcode "∶" ":" ":"
  , code "⁝" ":3"
  , code "⁞" ":4"
  , code "ː" "t:"
  , code "꞉" "s:"
  , lmcode "∴" "ther" "\\therefore"
  , lmcode "∵" "bec" "\\because"
  , lmcode "∷" "::" "::"
  , lmcode "‥" ".." ".."
  , lmcode "…" "..." "\\ldots"
  , lmcode "⋯" "c..." "\\cdots"
  , lmcode "⋮" "v..." "\\vdots"
  , code "⁖" ".:"
  , code "⁘" ".:."
  , code "⁙" ":.:"
  , code "⸭" ".+"
  , lmcode "∘" "o" "\\circ"
  , code "⦂" "o:"
  , code "⨟" "o;"
  , code "⍪" ",|"
  , code "⌄" "d^"
  , code "˙" ".^"
  , code "꘍" "and,"
  , code "∣" "|"
  , code "∤" "|/"
  , code "¦" "|:"
  , code "⫾" "w|"
  , lmcode "‖" "||" "\\|"
  , lmcode "∥" "par" "\\parallel"
  , lmcode "÷" "%" "\\div"
  , lmcode "٪" "%%" "\\%"
  , code "∕" "/"
  , lmcode "∖" "set\\\\" "\\setminus"
  , lmcode "⫽" "//" "\\sslash"
  , lmcode "⅋" "&" "\\bindnasrepma"
  , code "⫻" "///"
  , lmcode "∅" "O/" "\\varnothing"
  , lmcode "⋆" "*" "\\star"
  , code "☆" "star"
  , lmcode "★" "starb" "\\bigstar"
  , code "⋇" "**"
  , lmcode "♯" "#" "\\sharp"
  , code "𝄪" "##"
  , lmcode "♭" "b" "\\flat"
  , code "𝄫" "bb"
  , lmcode "♮" "n" "\\natural"
  , code "⋕" "=||" -- "\\hash"
  , code "¿" "d?"
  , code "¡" "d!"
  , code "⁇" "??"
  , code "‼" "!!"
  , code "⁈" "?!"
  , code "⁉" "!?"
  , lmcode "⧺" "++" "\\mathbin{{+}\\mspace{-8mu}{+}}"
  , code "⧻" "+++"
  , lmcode "∔" "+." "\\dotplus"
  , code "⨭" "+("
  , code "⨮" "+)"
  , lmcode "±" "+-" "\\pm"
  , code "∸" "-."
  , lmcode "×" "x" "\\times"
  , code "⨰" "x."
  , code "⨱" "x-"
  , code "⨴" "x("
  , code "⨵" "x)"
  , code "⨯" "xx"
  , code "⨳" "XX"
  , code "✖" "xb"
  , code "⤨" "swap"
  , lmcode "⊕" "o+" "\\oplus"
  , lmcode "⊖" "o-" "\\ominus"
  , lmcode "⊗" "ox" "\\otimes"
  , lmcode "⊘" "o/" "\\oslash"
  , lmcode "⊙" "o." "\\odot"
  , lmcode "⊚" "oo" "\\circledcirc"
  , lmcode "⊛" "o*" "\\circledast"
  , code "⊜" "o="
  , lmcode "⊝" "o--" "\\circleddash"
  , code "⎊" "ot"
  , lmcode "⦶" "o|" "\\obar"
  , code "⦷" "o||"
  , lmcode "⦸" "o\\\\" "\\obslash"
  , code "⦹" "obot"
  , code "⦼" "o%"
  , lmcode "⧀" "o<" "\\olessthan"
  , lmcode "⧁" "o>" "\\ogreaterthan"
  , code "⋄" ".di"
  , lmcode "◇" "di" "\\diamond"
  , code "⟐" "di."
  , lmcode "□" "s" "\\square"
  , lmcode "⊞" "s+" "\\boxplus"
  , lmcode "⊟" "s-" "\\boxminus"
  , lmcode "⊠" "sx" "\\boxtimes"
  , lmcode "⊡" "s." "\\boxdot"
  , code "⟎" "sand"
  , code "⟏" "sor"
  , lmcode "⧄" "s/" "\\boxslash"
  , lmcode "⧅" "s\\\\" "\\boxslash"
  , lmcode "⧆" "s*" "\\boxast"
  , lmcode "⧇" "so" "\\boxcircle"
  , lmcode "⧈" "ss" "\\boxbox"
  , code "⧉" "sss"
  , lmcode "⎅" "s|" "\\boxbar"
  , lmcode "○" "O" "\\bigcirc"
  , lmcode "∎" "qed" "\\blacksquare"
  , code "⌿" "-/" -- "\\notslash"
  , code "∿" "sin"
  , code "∾" "link"
  , lmcode "⋈" "bow" "\\bowtie"
  , code "⧖" "hour"
  , lmcode "⋉" "bowl" "\\ltimes"
  , lmcode "⋊" "bowr" "\\rtimes"
  , lmcode "∞" "inf" "\\infty"
  , lmcode "∝" "inf<" "\\propto"
  , lmcode "△" "tru" "\\vartriangle"
  , lmcode "▷" "trr" "\\vartriangleright"
  , lmcode "▽" "trd" "\\triangledown"
  , lmcode "◁" "trl" "\\vartriangleleft"
  , code "▲" "trub"
  , code "▶" "trrb"
  , code "▼" "trdb"
  , code "◀" "trlb"
  , code "⨹" "t+"
  , code "⨺" "t-"
  , code "⨻" "tx"
  , code "⟁" "tt"
  , code "◬" "t."
  , code "⟡" "cd"
  , code "⟢" "cd>"
  , code "⟣" "cd<"
  , code "⟤" "sq>"
  , code "⟥" "sq<"
  , code "⌑" "loz"
  , code "⟠" "d<>"
  , code "⌁" "zap"
  , lmcode "†" "dag" "\\dagger"
  , code "‡" "ddag"
  , code "⁂" "***"
  , code "∫" "int"
  , code "¢" "cent"
  , code "⧂" "Oo"
  , code "⌌" "+br"
  , code "⌍" "+bl"
  , code "⌎" "+tr"
  , code "⌏" "+tl"
  , code "⌜" "ctl"
  , code "⌝" "ctr"
  , code "⌞" "cbl"
  , code "⌟" "cbr"
  , code "⌲" ">-"
  , code "⚖" "scales"
  , lmcode "√" "root" "\\sqrt"

  -- Logic
  , lmcode "∈" "in" "\\in"
  , code "⋵" "in."
  , code "⋳" "in|"
  , code "⋹" "in="
  , code "⋲" "in-"
  , code "⋺" "in-l"
  , lmcode "∉" "in/" "\\notin"
  , lmcode "∋" "inl" "\\ni"
  , code "⋻" "in|l"
  , code "⟒" "inu"
  , code "⫙" "ind"
  , code "∊" "ele"
  , code "∍" "elel"
  , code "⋴" "ele|"
  , code "⋼" "ele|l"
  , code "⋿" "mem"
  , lmcode "∀" "all" "\\forall"
  , lmcode "∃" "ex" "\\exists"
  , lmcode "∄" "ex/" "\\nexists"
  , lmcode "⊢" "|-" "\\vdash"
  , lmcode "⊣" "-|" "\\dashv"
  , lmcode "⊨" "|=" "\\models"
  , lmcode "∨" "or" "\\vee"
  , lmcode "∧" "and" "\\wedge"
  , code "⩙" "andor"
  , lmcode "⊥" "bot" "\\bot"
  , code "⫫" "bot="
  , lmcode "⊤" "top" "\\top"
  , lmcode "¬" "not" "\\neg"
  , code "⊻" "xor"
  , code "∁" "comp"


  -- Equality
  , lmcode "≡" "==" "\\equiv"
  , code "≢" "==/" -- "\\nequiv"
  , code "≣" "==="
  , code "⩵" "=2"
  , code "⩶" "=3"
  , lmcode "≃" "~=" "\\simeq"
  , lmcode "≈" "~~" "\\approx"
  , code "≉" "~~/"
  , lmcode "∼" "~" "\\sim"
  , code "≁" "~/"
  , lmcode "≔" ":=" "\\coloneqq"
  , code "≕" "=:"
  , lmcode "⩴" "::=" "\\Coloneqq"
  , lmcode "≠" "=/" "\\neq"
  , lmcode "≟" "=?" "\\mathrel{\\overset{?}{=}}"
  , lmcode "≗" "=o" "\\mathrel{\\overset{\\circ}{=}}"
  , lmcode "⩮" "=*" "\\mathrel{\\overset{*}{=}}"
  , lmcode "⩦" "=." "\\mathrel{\\underset{\\cdotp}{=}}"
  , code "⧣" "=//"
  , code "≌" "=s"
  , code "≛" "=star"
  , code "⩯" "~~^"
  , lmcode "≜" "=t" "\\triangleq"
  , code "≝" "=def"
  , code "≍" "eqv"
  , code "≭" "eqv/"
  , code "≎" "=O"
  , code "█" "block"

  -- Subscripts
  , lmcode "₊" "_+" "_+"
  , lmcode "₋" "_-" "_-"
  , lmcode "₌" "_=" "_="
  , lmcode "₍" "_(" "("
  , lmcode "₎" "_)" ")"

  -- Superscripts
  , lmcode "⁺" "^+" "^+"
  , lmcode "⁻" "^-" "^-"
  , lmcode "⁼" "^=" "^="
  , lmcode "⁽" "^(" "^("
  , lmcode "⁾" "^)" "^)"
  , code "˚" "^deg"
  , code "⑅" "^bow"
  , code "ᐜ" "^uu"
  , code "ᐞ" "^tr"
  , code "ᐟ" "^/"
  , code "ᐠ" "^\\"
  , code "ᐡ" "^un"
  , code "ᐢ" "^in"
  , code "ᐣ" "^su"
  , code "ᐥ" "^//"
  , code "ᐦ" "^||"
  , code "ᐪ" "^top"
  , code "ᐧ" "^."

  -- Combining Subscripts
  -- Combining Superscripts

  , code "֮" "^^nu"
  , code "̂" "^^^"
  , code "᷾" "^^<"
  , code "͐" "^^>"
  , code "⃖" "^^<-"
  , code "⃗" "^^->"
  , code "̃" "^^~"
  , code "̑" "^^("
  , code "̆" "^^)"
  , code "͌" "^^~~"
  , code "̄" "^^-"
  , code "̅" "^^--"
  , code "̇" "^^."
  , code "̈" "^^.."
  , code "̊" "^^o"
  , code "̉" "^^,"
  , code "̌" "^^v"
  , code "̐" "^^(."

  -- Markup
  , code "¶" "PP"
  , code "␣" "_"
  , code "‗" "__"
  , code "﹍" "___"
  , code "﹏" "~_"
  , code "‿" "utie"
  , code "⁔" "iutie"
  , code "␠" "sp"
  , lmcode "§" "SS" "\\S"

  -- Bullets
  , lmcode "•" "bu" "\\bullet"
  , lmcode "◦" "obu" "\\circ"
  , code "⁃" "-bu"
  , code "‣" "tbu"
  , code "⁌" "lbu"
  , code "⁍" "rbu"
  , code "◘" "ibu"
  , code "❥" "hbu"
  , code "☙" "hlbu"
  , code "❧" "hrbu"
  , code "⦿" "buo"
  , code "⦾" "obuo"

  -- OK
  , lmcode "✓" "check" "\\checkmark"
  , code "✗" "X" -- "\\ballotx"
  , code "☐" "bal"
  , code "☑" "balc"
  , code "☒" "balx"

  -- emoji
  -- based off of https://github.com/dysfunc/ascii-emoji
  -- inspiration for more: https://en.wikipedia.org/wiki/List_of_emoticons
  , code "ಠ_ಠ" "disapprove"
  , code "﹙╯°□°）╯︵┻━┻" "flip"
  , code "┬─┬ノ﹙゜-゜ノ﹚" "putback"
  , code "┬─┬⃰͡ ﹙ᵔᵕᵔ͜ ﹚" "dust"
  , code "┻━┻︵ヽ﹙`Д´﹚ﾉ︵┻━┻" "doubleflip"
  , code "ლ﹙｀ー´ლ﹚" "fisticuffs"
  , code "¯\\_﹙ツ﹚_/¯" "shrug"
  , code "¯\\﹙°_o﹚/¯" "meh"
  , code "﹙╬ಠ益ಠ﹚" "angry"
  , code "ฅ^•ﻌ•^ฅ" "meow"
  , code "﹙^_^）o自自o（^_^﹚" "cheers"
  , code "ಥ_ಥ" "crying"
  , code "ಥ﹏ಥ" "breakdown"
  , code "٩◔̯◔۶" "disagree"
  , code "ᕙ﹙⇀‸↼‶﹚ᕗ" "flex"
  , code "¯\\_﹙⊙︿⊙﹚_/¯" "sadconfused"
  , code "щ（ﾟДﾟщ）" "ytho"
  , code "ᕕ﹙ᐛ﹚ᕗ" "strut"
  , code "t﹙-_-t﹚" "fkit"
  , code "눈_눈" "sleepy"
  , code "ヾ﹙´〇`﹚ﾉ♪♪♪" "opera"
  , code "[¬º-°]¬" "zombie"
  , code "﹙☞ﾟヮﾟ﹚☞" "point"
  , code "💩" "poo"

  -- Greek Normal
  , lmcode "Α" "Alpha"    "A"
  , lmcode "Β" "Beta"     "B"
  , lmcode "Γ" "Gamma"    "\\Gamma"
  , lmcode "Δ" "Delta"    "\\Delta"
  , lmcode "Ε" "Epsilon"  "E"
  , lmcode "Ζ" "Zeta"     "Z"
  , lmcode "Η" "Eta"      "H"
  , lmcode "Θ" "Theta"    "\\Theta"
  , lmcode "Ι" "Iota"     "I"
  , lmcode "Κ" "Kappa"    "K"
  , lmcode "Λ" "Lambda"   "\\Lambda"
  , lmcode "Μ" "Mu"       "M"
  , lmcode "Ν" "Nu"       "N"
  , lmcode "Ξ" "Xi"       "\\Xi"
  , lmcode "Ο" "Omicron"  "O"
  , lmcode "Π" "Pi"       "\\Pi"
  , lmcode "Ρ" "Rho"      "P"
  , lmcode "ϴ" "varSigma" "\\varSigma"
  , lmcode "Σ" "Sigma"    "\\Sigma"
  , lmcode "Τ" "Tau"      "T"
  , lmcode "Υ" "Upsilon"  "Y"
  , lmcode "Φ" "Phi"      "\\Phi"
  , lmcode "Χ" "Chi"      "X"
  , lmcode "Ψ" "Psi"      "\\Psi"
  , lmcode "Ω" "Omega"    "\\Omega"
  , lmcode "∇" "Nabla"    "\\nabla"

  , lmcode "α" "alpha"    "\\alpha"
  , lmcode "β" "beta"     "\\beta"
  , lmcode "γ" "gamma"    "\\gamma"
  , lmcode "δ" "delta"    "\\delta"
  , lmcode "ε" "epsilon"  "\\epsilon"
  , lmcode "ζ" "zeta"     "\\zeta"
  , lmcode "η" "eta"      "\\eta"
  , lmcode "θ" "theta"    "\\theta"
  , lmcode "ι" "iota"     "\\iota"
  , lmcode "κ" "kappa"    "\\kappa"
  , lmcode "λ" "lambda"   "\\lambda"
  , lmcode "μ" "mu"       "\\mu"
  , lmcode "ν" "nu"       "\\nu"
  , lmcode "ξ" "xi"       "\\xi"
  , lmcode "ο" "omicron"  "o"
  , lmcode "π" "pi"       "\\pi"
  , lmcode "ρ" "rho"      "\\rho"
  , lmcode "ς" "varsigma" "\\varsigma"
  , lmcode "σ" "sigma"    "\\sigma"
  , lmcode "τ" "tau"      "\\tau"
  , lmcode "υ" "upsilon"  "\\upsilon"
  , lmcode "φ" "phi"      "\\phi"
  , lmcode "χ" "chi"      "\\chi"
  , lmcode "ψ" "psi"      "\\psi"
  , lmcode "ω" "omega"    "\\omega"
  , lmcode "∂" "nabla"    "\\partial"

  , lmcode "ϵ" "varepsilon" "\\varepsilon"
  , lmcode "ϑ" "vartheta"   "\\vartheta"
  , lmcode "ϰ" "varkappa"   "\\varkappa"
  , lmcode "ϕ" "varphi"     "\\varphi"
  , lmcode "ϱ" "varrho"     "\\varrho"
  , lmcode "ϖ" "varpi"      "\\varpi"
  , code "ƛ" "lambda/"

  -- Greek Bold
  , lmcode "𝚨" "bdAlpha"    "\\boldsymbol{A}"
  , lmcode "𝚩" "bdBeta"     "\\boldsymbol{B}"
  , lmcode "𝚪" "bdGamma"    "\\boldsymbol{\\Gamma}"
  , lmcode "𝚫" "bdDelta"    "\\boldsymbol{\\Delta}"
  , lmcode "𝚬" "bdEpsilon"  "\\boldsymbol{E}"
  , lmcode "𝚭" "bdZeta"     "\\boldsymbol{Z}"
  , lmcode "𝚮" "bdEta"      "\\boldsymbol{H}"
  , lmcode "𝚯" "bdTheta"    "\\boldsymbol{\\Theta}"
  , lmcode "𝚰" "bdIota"     "\\boldsymbol{I}"
  , lmcode "𝚱" "bdKappa"    "\\boldsymbol{K}"
  , lmcode "𝚲" "bdLambda"   "\\boldsymbol{\\Lambda}"
  , lmcode "𝚳" "bdMu"       "\\boldsymbol{M}"
  , lmcode "𝚴" "bdNu"       "\\boldsymbol{N}"
  , lmcode "𝚵" "bdXi"       "\\boldsymbol{\\Xi}"
  , lmcode "𝚶" "bdOmicron"  "\\boldsymbol{O}"
  , lmcode "𝚷" "bdPi"       "\\boldsymbol{\\Pi}"
  , lmcode "𝚸" "bdRho"      "\\boldsymbol{P}"
  , lmcode "𝚹" "bdvarSigma" "\\boldsymbol{\\varSigma}"
  , lmcode "𝚺" "bdSigma"    "\\boldsymbol{\\Sigma}"
  , lmcode "𝚻" "bdTau"      "\\boldsymbol{T}"
  , lmcode "𝚼" "bdUpsilon"  "\\boldsymbol{Y}"
  , lmcode "𝚽" "bdPhi"      "\\boldsymbol{\\Phi}"
  , lmcode "𝚾" "bdChi"      "\\boldsymbol{X}"
  , lmcode "𝚿" "bdPsi"      "\\boldsymbol{\\Psi}"
  , lmcode "𝛀" "bdOmega"    "\\boldsymbol{\\Omega}"
  , lmcode "𝛁" "bdNabla"    "\\boldsymbol{\\nabla}"

  , lmcode "𝛂" "bdalpha"    "\\boldsymbol{\\alpha}"
  , lmcode "𝛃" "bdbeta"     "\\boldsymbol{\\beta}"
  , lmcode "𝛄" "bdgamma"    "\\boldsymbol{\\gamma}"
  , lmcode "𝛅" "bddelta"    "\\boldsymbol{\\delta}"
  , lmcode "𝛆" "bdepsilon"  "\\boldsymbol{\\epsilon}"
  , lmcode "𝛇" "bdzeta"     "\\boldsymbol{\\zeta}"
  , lmcode "𝛈" "bdldeta"    "\\boldsymbol{\\eta}"
  , lmcode "𝛉" "bdtheta"    "\\boldsymbol{\\theta}"
  , lmcode "𝛊" "bdiota"     "\\boldsymbol{\\iota}"
  , lmcode "𝛋" "bdkappa"    "\\boldsymbol{\\kappa}"
  , lmcode "𝛌" "bdlambda"   "\\boldsymbol{\\lambda}"
  , lmcode "𝛍" "bdmu"       "\\boldsymbol{\\mu}"
  , lmcode "𝛎" "bdnu"       "\\boldsymbol{\\nu}"
  , lmcode "𝛏" "bdxi"       "\\boldsymbol{\\xi}"
  , lmcode "𝛐" "bdomicron"  "\\boldsymbol{o}"
  , lmcode "𝛑" "bdpi"       "\\boldsymbol{\\pi}"
  , lmcode "𝛒" "bdrho"      "\\boldsymbol{\\rho}"
  , lmcode "𝛓" "bdvarsigma" "\\boldsymbol{\\varsigma}"
  , lmcode "𝛔" "bdsigma"    "\\boldsymbol{\\sigma}"
  , lmcode "𝛕" "bdtau"      "\\boldsymbol{\\tau}"
  , lmcode "𝛖" "bdupsilon"  "\\boldsymbol{\\upsilon}"
  , lmcode "𝛗" "bdphi"      "\\boldsymbol{\\phi}"
  , lmcode "𝛘" "bdchi"      "\\boldsymbol{\\chi}"
  , lmcode "𝛙" "bdpsi"      "\\boldsymbol{\\psi}"
  , lmcode "𝛚" "bdomega"    "\\boldsymbol{\\omega}"
  , lmcode "𝛛" "bdnabla"    "\\boldsymbol{\\partial}"

  , lmcode "𝛜" "bdvarepsilon" "\\boldsymbol{\\varepsilon}"
  , lmcode "𝛝" "bdvartheta"   "\\boldsymbol{\\vartheta}"
  , lmcode "𝛞" "bdvarkappa"   "\\boldsymbol{\\varkappa}"
  , lmcode "𝛟" "bdvarphi"     "\\boldsymbol{\\varphi}"
  , lmcode "𝛠" "bdvarrho"     "\\boldsymbol{\\varrho}"
  , lmcode "𝛡" "bdvarpi"      "\\boldsymbol{\\varpi}"

  -- Greek Italic
  , code "𝛢" "itAlpha"
  , code "𝛣" "itBeta"
  , code "𝛤" "itGamma"
  , code "𝛥" "itDelta"
  , code "𝛦" "itEpsilon"
  , code "𝛧" "itZeta"
  , code "𝛨" "itEta"
  , code "𝛩" "itTheta"
  , code "𝛪" "itIota"
  , code "𝛫" "itKappa"
  , code "𝛬" "itLambda"
  , code "𝛭" "itMu"
  , code "𝛮" "itNu"
  , code "𝛯" "itXi"
  , code "𝛰" "itOmicron"
  , code "𝛱" "itPi"
  , code "𝛲" "itRho"
  , code "𝛳" "itvarSigma"
  , code "𝛴" "itSigma"
  , code "𝛵" "itTau"
  , code "𝛶" "itUpsilon"
  , code "𝛷" "itPhi"
  , code "𝛸" "itChi"
  , code "𝛹" "itPsi"
  , code "𝛺" "itOmega"
  , code "𝛻" "itNabla"

  , code "𝛼" "italpha"
  , code "𝛽" "itbeta"
  , code "𝛾" "itgamma"
  , code "𝛿" "itdelta"
  , code "𝜀" "itepsilon"
  , code "𝜁" "itzeta"
  , code "𝜂" "iteta"
  , code "𝜃" "ittheta"
  , code "𝜄" "itiota"
  , code "𝜅" "itkappa"
  , code "𝜆" "itlambda"
  , code "𝜇" "itmu"
  , code "𝜈" "itnu"
  , code "𝜉" "itxi"
  , code "𝜊" "itomicron"
  , code "𝜋" "itpi"
  , code "𝜌" "itrho"
  , code "𝜍" "itvarsigma"
  , code "𝜎" "itsigma"
  , code "𝜏" "ittau"
  , code "𝜐" "itupsilon"
  , code "𝜑" "itphi"
  , code "𝜒" "itchi"
  , code "𝜓" "itpsi"
  , code "𝜔" "itomega"
  , code "𝜕" "itnabla"

  , code "𝜖" "itvarepsilon"
  , code "𝜗" "itvartheta"
  , code "𝜘" "itvarkappa"
  , code "𝜙" "itvarphi"
  , code "𝜚" "itvarrho"
  , code "𝜛" "itvarpi"

  -- Greek Bold Italic
  , code "𝜜" "bditAlpha"
  , code "𝜝" "bditBeta"
  , code "𝜞" "bditGamma"
  , code "𝜟" "bditDelta"
  , code "𝜠" "bditEpsilon"
  , code "𝜡" "bditZeta"
  , code "𝜢" "bditEta"
  , code "𝜣" "bditTheta"
  , code "𝜤" "bditIota"
  , code "𝜥" "bditKappa"
  , code "𝜦" "bditLambda"
  , code "𝜧" "bditMu"
  , code "𝜨" "bditNu"
  , code "𝜩" "bditXi"
  , code "𝜪" "bditOmicron"
  , code "𝜫" "bditPi"
  , code "𝜬" "bditRho"
  , code "𝜭" "bditvarSigma"
  , code "𝜮" "bditSigma"
  , code "𝜯" "bditTau"
  , code "𝜰" "bditUpsilon"
  , code "𝜱" "bditPhi"
  , code "𝜲" "bditChi"
  , code "𝜳" "bditPsi"
  , code "𝜴" "bditOmega"
  , code "𝜵" "bditNabla"

  , code "𝜶" "bditalpha"
  , code "𝜷" "bditbeta"
  , code "𝜸" "bditgamma"
  , code "𝜹" "bditdelta"
  , code "𝜺" "bditepsilon"
  , code "𝜻" "bditzeta"
  , code "𝜼" "bditeta"
  , code "𝜽" "bdittheta"
  , code "𝜾" "bditiota"
  , code "𝜿" "bditkappa"
  , code "𝝀" "bditlambda"
  , code "𝝁" "bditmu"
  , code "𝝂" "bditnu"
  , code "𝝃" "bditxi"
  , code "𝝄" "bditomicron"
  , code "𝝅" "bditpi"
  , code "𝝆" "bditrho"
  , code "𝝇" "bditvarsigma"
  , code "𝝈" "bditsigma"
  , code "𝝉" "bdittau"
  , code "𝝊" "bditupsilon"
  , code "𝝋" "bditphi"
  , code "𝝌" "bditchi"
  , code "𝝍" "bditpsi"
  , code "𝝎" "bditomega"
  , code "𝝏" "bditnabla"

  , code "𝝐" "bditvarepsilon"
  , code "𝝑" "bditvartheta"
  , code "𝝒" "bditvarkappa"
  , code "𝝓" "bditvarphi"
  , code "𝝔" "bditvarrho"
  , code "𝝕" "bditvarpi"

  -- Greek Subscript
  , lmcode "ᵦ" "_beta"  "_\\beta"
  , lmcode "ᵧ" "_gamma" "_\\gamma"
  , lmcode "ᵨ" "_rho"   "_\\rho"
  , lmcode "ᵩ" "_phi"   "_\\phi"
  , lmcode "ᵪ" "_chi"   "_\\chi"

  -- Greek Superscript
  , lmcode "ᵅ" "^alpha"   "^\\alpha"
  , lmcode "ᵝ" "^beta"    "^\\beta"
  , lmcode "ᵞ" "^gamma"   "^\\gamma"
  , lmcode "ᵟ" "^delta"   "^\\delta"
  , lmcode "ᵋ" "^epsilon" "^\\epsilon"
  , lmcode "ᶿ" "^theta"   "^\\theta"
  , lmcode "ᶥ" "^iota"    "^\\iota"
  , lmcode "ᶲ" "^Phi"     "^\\Phi"
  , lmcode "ᵠ" "^phi"     "^\\phi"
  , lmcode "ᵡ" "^chi"     "^\\chi"

  -- Greek Blackboard Bold
  , code "ℾ" "bbGamma"
  , code "ℽ" "bbgamma"
  , code "ℿ" "bbPi"
  , code "ℼ" "bbpi"
  , code "⅀" "bbSigma"

  -- Numbers Bold
  , lmcodet "𝟎" "bd0" "{\\textbf{0}}"
  , lmcodet "𝟏" "bd1" "{\\textbf{1}}"
  , lmcodet "𝟐" "bd2" "{\\textbf{2}}"
  , lmcodet "𝟑" "bd3" "{\\textbf{3}}"
  , lmcodet "𝟒" "bd4" "{\\textbf{4}}"
  , lmcodet "𝟓" "bd5" "{\\textbf{5}}"
  , lmcodet "𝟔" "bd6" "{\\textbf{6}}"
  , lmcodet "𝟕" "bd7" "{\\textbf{7}}"
  , lmcodet "𝟖" "bd8" "{\\textbf{8}}"
  , lmcodet "𝟗" "bd9" "{\\textbf{9}}"

  -- Numbers Blackboard Bold
  , lmcodet "𝟘" "bb0" "{\\mathbb{0}}"
  , lmcodet "𝟙" "bb1" "{\\mathbb{1}}"
  , lmcodet "𝟚" "bb2" "{\\mathbb{2}}"
  , lmcodet "𝟛" "bb3" "{\\mathbb{3}}"
  , lmcodet "𝟜" "bb4" "{\\mathbb{4}}"
  , lmcodet "𝟝" "bb5" "{\\mathbb{5}}"
  , lmcodet "𝟞" "bb6" "{\\mathbb{6}}"
  , lmcodet "𝟟" "bb7" "{\\mathbb{7}}"
  , lmcodet "𝟠" "bb8" "{\\mathbb{8}}"
  , lmcodet "𝟡" "bb9" "{\\mathbb{9}}"

  -- Numbers Subscript
  , lmcode "₀" "_0" "_0"
  , lmcode "₁" "_1" "_1"
  , lmcode "₂" "_2" "_2"
  , lmcode "₃" "_3" "_3"
  , lmcode "₄" "_4" "_4"
  , lmcode "₅" "_5" "_5"
  , lmcode "₆" "_6" "_6"
  , lmcode "₇" "_7" "_7"
  , lmcode "₈" "_8" "_8"
  , lmcode "₉" "_9" "_9"

  -- Numbers Superscript
  , lmcode "⁰" "^0" "^0"
  , lmcode "¹" "^1" "^1"
  , lmcode "²" "^2" "^2"
  , lmcode "³" "^3" "^3"
  , lmcode "⁴" "^4" "^4"
  , lmcode "⁵" "^5" "^5"
  , lmcode "⁶" "^6" "^6"
  , lmcode "⁷" "^7" "^7"
  , lmcode "⁸" "^8" "^8"
  , lmcode "⁹" "^9" "^9"

  -- Fractions
  , lmcode "½" "1/2"  "\\nicefrac{1}{2}"
  , lmcode "↉" "0/3"  "\\nicefrac{0}{3}"
  , lmcode "⅓" "1/3"  "\\nicefrac{1}{3}"
  , lmcode "⅔" "2/3"  "\\nicefrac{2}{3}"
  , lmcode "¼" "1/4"  "\\nicefrac{1}{4}"
  , lmcode "¾" "3/4"  "\\nicefrac{3}{4}"
  , lmcode "⅕" "1/5"  "\\nicefrac{1}{5}"
  , lmcode "⅖" "2/5"  "\\nicefrac{2}{5}"
  , lmcode "⅗" "3/5"  "\\nicefrac{3}{5}"
  , lmcode "⅘" "4/5"  "\\nicefrac{4}{5}"
  , lmcode "⅙" "1/6"  "\\nicefrac{1}{6}"
  , lmcode "⅚" "5/6"  "\\nicefrac{5}{6}"
  , lmcode "⅐" "1/7"  "\\nicefrac{1}{7}"
  , lmcode "⅛" "1/8"  "\\nicefrac{1}{8}"
  , lmcode "⅜" "3/8"  "\\nicefrac{3}{8}"
  , lmcode "⅝" "5/8"  "\\nicefrac{5}{8}"
  , lmcode "⅞" "7/8"  "\\nicefrac{7}{8}"
  , lmcode "⅑" "1/9"  "\\nicefrac{1}{9}"
  , lmcode "⅒" "1/10" "\\nicefrac{1}{10}"

  -- Weierstrass p
  , lmcode "℘" "wp" "\\wp"
  -- ell
  , lmcode "ℓ" "ell" "\\ell"
  , lmcode "㏑" "ln" "\\ln"
  , lmcode "㏒" "log" "\\log"

  -- Roman Bold
  , lmcodet "𝐀" "bdA" "{\\textbf{A}}"
  , lmcodet "𝐁" "bdB" "{\\textbf{B}}"
  , lmcodet "𝐂" "bdC" "{\\textbf{C}}"
  , lmcodet "𝐃" "bdD" "{\\textbf{D}}"
  , lmcodet "𝐄" "bdE" "{\\textbf{E}}"
  , lmcodet "𝐅" "bdF" "{\\textbf{F}}"
  , lmcodet "𝐆" "bdG" "{\\textbf{G}}"
  , lmcodet "𝐇" "bdH" "{\\textbf{H}}"
  , lmcodet "𝐈" "bdI" "{\\textbf{I}}"
  , lmcodet "𝐉" "bdJ" "{\\textbf{J}}"
  , lmcodet "𝐊" "bdK" "{\\textbf{K}}"
  , lmcodet "𝐋" "bdL" "{\\textbf{L}}"
  , lmcodet "𝐌" "bdM" "{\\textbf{M}}"
  , lmcodet "𝐍" "bdN" "{\\textbf{N}}"
  , lmcodet "𝐎" "bdO" "{\\textbf{O}}"
  , lmcodet "𝐏" "bdP" "{\\textbf{P}}"
  , lmcodet "𝐐" "bdQ" "{\\textbf{Q}}"
  , lmcodet "𝐑" "bdR" "{\\textbf{R}}"
  , lmcodet "𝐒" "bdS" "{\\textbf{S}}"
  , lmcodet "𝐓" "bdT" "{\\textbf{T}}"
  , lmcodet "𝐔" "bdU" "{\\textbf{U}}"
  , lmcodet "𝐕" "bdV" "{\\textbf{V}}"
  , lmcodet "𝐖" "bdW" "{\\textbf{W}}"
  , lmcodet "𝐗" "bdX" "{\\textbf{X}}"
  , lmcodet "𝐘" "bdY" "{\\textbf{Y}}"
  , lmcodet "𝐙" "bdZ" "{\\textbf{Z}}"
  , lmcodet "𝐚" "bda" "{\\textbf{a}}"
  , lmcodet "𝐛" "bdb" "{\\textbf{b}}"
  , lmcodet "𝐜" "bdc" "{\\textbf{c}}"
  , lmcodet "𝐝" "bdd" "{\\textbf{d}}"
  , lmcodet "𝐞" "bde" "{\\textbf{e}}"
  , lmcodet "𝐟" "bdf" "{\\textbf{f}}"
  , lmcodet "𝐠" "bdg" "{\\textbf{g}}"
  , lmcodet "𝐡" "bdh" "{\\textbf{h}}"
  , lmcodet "𝐢" "bdi" "{\\textbf{i}}"
  , lmcodet "𝐣" "bdj" "{\\textbf{j}}"
  , lmcodet "𝐤" "bdk" "{\\textbf{k}}"
  , lmcodet "𝐥" "bdl" "{\\textbf{l}}"
  , lmcodet "𝐦" "bdm" "{\\textbf{m}}"
  , lmcodet "𝐧" "bdn" "{\\textbf{n}}"
  , lmcodet "𝐨" "bdo" "{\\textbf{o}}"
  , lmcodet "𝐩" "bdp" "{\\textbf{p}}"
  , lmcodet "𝐪" "bdq" "{\\textbf{q}}"
  , lmcodet "𝐫" "bdr" "{\\textbf{r}}"
  , lmcodet "𝐬" "bds" "{\\textbf{s}}"
  , lmcodet "𝐭" "bdt" "{\\textbf{t}}"
  , lmcodet "𝐮" "bdu" "{\\textbf{u}}"
  , lmcodet "𝐯" "bdv" "{\\textbf{v}}"
  , lmcodet "𝐰" "bdw" "{\\textbf{w}}"
  , lmcodet "𝐱" "bdx" "{\\textbf{x}}"
  , lmcodet "𝐲" "bdy" "{\\textbf{y}}"
  , lmcodet "𝐳" "bdz" "{\\textbf{z}}"

  -- Roman Itallic
  , lmcodet "𝐴" "itA" "{\\textit{A}}"
  , lmcodet "𝐵" "itB" "{\\textit{B}}"
  , lmcodet "𝐶" "itC" "{\\textit{C}}"
  , lmcodet "𝐷" "itD" "{\\textit{D}}"
  , lmcodet "𝐸" "itE" "{\\textit{E}}"
  , lmcodet "𝐹" "itF" "{\\textit{F}}"
  , lmcodet "𝐺" "itG" "{\\textit{G}}"
  , lmcodet "𝐻" "itH" "{\\textit{H}}"
  , lmcodet "𝐼" "itI" "{\\textit{I}}"
  , lmcodet "𝐽" "itJ" "{\\textit{J}}"
  , lmcodet "𝐾" "itK" "{\\textit{K}}"
  , lmcodet "𝐿" "itL" "{\\textit{L}}"
  , lmcodet "𝑀" "itM" "{\\textit{M}}"
  , lmcodet "𝑁" "itN" "{\\textit{N}}"
  , lmcodet "𝑂" "itO" "{\\textit{O}}"
  , lmcodet "𝑃" "itP" "{\\textit{P}}"
  , lmcodet "𝑄" "itQ" "{\\textit{Q}}"
  , lmcodet "𝑅" "itR" "{\\textit{R}}"
  , lmcodet "𝑆" "itS" "{\\textit{S}}"
  , lmcodet "𝑇" "itT" "{\\textit{T}}"
  , lmcodet "𝑈" "itU" "{\\textit{U}}"
  , lmcodet "𝑉" "itV" "{\\textit{V}}"
  , lmcodet "𝑊" "itW" "{\\textit{W}}"
  , lmcodet "𝑋" "itX" "{\\textit{X}}"
  , lmcodet "𝑌" "itY" "{\\textit{Y}}"
  , lmcodet "𝑍" "itZ" "{\\textit{Z}}"
  , lmcodet "𝑎" "ita" "{\\textit{a}}"
  , lmcodet "𝑏" "itb" "{\\textit{b}}"
  , lmcodet "𝑐" "itc" "{\\textit{c}}"
  , lmcodet "𝑑" "itd" "{\\textit{d}}"
  , lmcodet "𝑒" "ite" "{\\textit{e}}"
  , lmcodet "𝑓" "itf" "{\\textit{f}}"
  , lmcodet "𝑔" "itg" "{\\textit{g}}"
  , lmcodet "ℎ" "ith" "{\\textit{h}}"
  , lmcodet "𝑖" "iti" "{\\textit{i}}"
  , lmcodet "𝑗" "itj" "{\\textit{j}}"
  , lmcodet "𝑘" "itk" "{\\textit{k}}"
  , lmcodet "𝑙" "itl" "{\\textit{l}}"
  , lmcodet "𝑚" "itm" "{\\textit{m}}"
  , lmcodet "𝑛" "itn" "{\\textit{n}}"
  , lmcodet "𝑜" "ito" "{\\textit{o}}"
  , lmcodet "𝑝" "itp" "{\\textit{p}}"
  , lmcodet "𝑞" "itq" "{\\textit{q}}"
  , lmcodet "𝑟" "itr" "{\\textit{r}}"
  , lmcodet "𝑠" "its" "{\\textit{s}}"
  , lmcodet "𝑡" "itt" "{\\textit{t}}"
  , lmcodet "𝑢" "itu" "{\\textit{u}}"
  , lmcodet "𝑣" "itv" "{\\textit{v}}"
  , lmcodet "𝑤" "itw" "{\\textit{w}}"
  , lmcodet "𝑥" "itx" "{\\textit{x}}"
  , lmcodet "𝑦" "ity" "{\\textit{y}}"
  , lmcodet "𝑧" "itz" "{\\textit{z}}"

  -- Roman Bold Itallic
  , lmcodet "𝑨" "bditA" "{\\textbf{\\textit{A}}}"
  , lmcodet "𝑩" "bditB" "{\\textbf{\\textit{B}}}"
  , lmcodet "𝑪" "bditC" "{\\textbf{\\textit{C}}}"
  , lmcodet "𝑫" "bditD" "{\\textbf{\\textit{D}}}"
  , lmcodet "𝑬" "bditE" "{\\textbf{\\textit{E}}}"
  , lmcodet "𝑭" "bditF" "{\\textbf{\\textit{F}}}"
  , lmcodet "𝑮" "bditG" "{\\textbf{\\textit{G}}}"
  , lmcodet "𝑯" "bditH" "{\\textbf{\\textit{H}}}"
  , lmcodet "𝑰" "bditI" "{\\textbf{\\textit{I}}}"
  , lmcodet "𝑱" "bditJ" "{\\textbf{\\textit{J}}}"
  , lmcodet "𝑲" "bditK" "{\\textbf{\\textit{K}}}"
  , lmcodet "𝑳" "bditL" "{\\textbf{\\textit{L}}}"
  , lmcodet "𝑴" "bditM" "{\\textbf{\\textit{M}}}"
  , lmcodet "𝑵" "bditN" "{\\textbf{\\textit{N}}}"
  , lmcodet "𝑶" "bditO" "{\\textbf{\\textit{O}}}"
  , lmcodet "𝑷" "bditP" "{\\textbf{\\textit{P}}}"
  , lmcodet "𝑸" "bditQ" "{\\textbf{\\textit{Q}}}"
  , lmcodet "𝑹" "bditR" "{\\textbf{\\textit{R}}}"
  , lmcodet "𝑺" "bditS" "{\\textbf{\\textit{S}}}"
  , lmcodet "𝑻" "bditT" "{\\textbf{\\textit{T}}}"
  , lmcodet "𝑼" "bditU" "{\\textbf{\\textit{U}}}"
  , lmcodet "𝑽" "bditV" "{\\textbf{\\textit{V}}}"
  , lmcodet "𝑾" "bditW" "{\\textbf{\\textit{W}}}"
  , lmcodet "𝑿" "bditX" "{\\textbf{\\textit{X}}}"
  , lmcodet "𝒀" "bditY" "{\\textbf{\\textit{Y}}}"
  , lmcodet "𝒁" "bditZ" "{\\textbf{\\textit{Z}}}"
  , lmcodet "𝒂" "bdita" "{\\textbf{\\textit{a}}}"
  , lmcodet "𝒃" "bditb" "{\\textbf{\\textit{b}}}"
  , lmcodet "𝒄" "bditc" "{\\textbf{\\textit{c}}}"
  , lmcodet "𝒅" "bditd" "{\\textbf{\\textit{d}}}"
  , lmcodet "𝒆" "bdite" "{\\textbf{\\textit{e}}}"
  , lmcodet "𝒇" "bditf" "{\\textbf{\\textit{f}}}"
  , lmcodet "𝒈" "bditg" "{\\textbf{\\textit{g}}}"
  , lmcodet "𝒉" "bdith" "{\\textbf{\\textit{h}}}"
  , lmcodet "𝒊" "bditi" "{\\textbf{\\textit{i}}}"
  , lmcodet "𝒋" "bditj" "{\\textbf{\\textit{j}}}"
  , lmcodet "𝒌" "bditk" "{\\textbf{\\textit{k}}}"
  , lmcodet "𝒍" "bditl" "{\\textbf{\\textit{l}}}"
  , lmcodet "𝒎" "bditm" "{\\textbf{\\textit{m}}}"
  , lmcodet "𝒏" "bditn" "{\\textbf{\\textit{n}}}"
  , lmcodet "𝒐" "bdito" "{\\textbf{\\textit{o}}}"
  , lmcodet "𝒑" "bditp" "{\\textbf{\\textit{p}}}"
  , lmcodet "𝒒" "bditq" "{\\textbf{\\textit{q}}}"
  , lmcodet "𝒓" "bditr" "{\\textbf{\\textit{r}}}"
  , lmcodet "𝒔" "bdits" "{\\textbf{\\textit{s}}}"
  , lmcodet "𝒕" "bditt" "{\\textbf{\\textit{t}}}"
  , lmcodet "𝒖" "bditu" "{\\textbf{\\textit{u}}}"
  , lmcodet "𝒗" "bditv" "{\\textbf{\\textit{v}}}"
  , lmcodet "𝒘" "bditw" "{\\textbf{\\textit{w}}}"
  , lmcodet "𝒙" "bditx" "{\\textbf{\\textit{x}}}"
  , lmcodet "𝒚" "bdity" "{\\textbf{\\textit{y}}}"
  , lmcodet "𝒛" "bditz" "{\\textbf{\\textit{z}}}"

  -- Roman Caligraphy
  , lmcodet "𝒜" "calA" "{\\mathcal{A}}"
  , lmcodet "ℬ" "calB" "{\\mathcal{B}}"
  , lmcodet "𝒞" "calC" "{\\mathcal{C}}"
  , lmcodet "𝒟" "calD" "{\\mathcal{D}}"
  , lmcodet "ℰ" "calE" "{\\mathcal{E}}"
  , lmcodet "ℱ" "calF" "{\\mathcal{F}}"
  , lmcodet "𝒢" "calG" "{\\mathcal{G}}"
  , lmcodet "ℋ" "calH" "{\\mathcal{H}}"
  , lmcodet "ℐ" "calI" "{\\mathcal{I}}"
  , lmcodet "𝒥" "calJ" "{\\mathcal{J}}"
  , lmcodet "𝒦" "calK" "{\\mathcal{K}}"
  , lmcodet "ℒ" "calL" "{\\mathcal{L}}"
  , lmcodet "ℳ" "calM" "{\\mathcal{M}}"
  , lmcodet "𝒩" "calN" "{\\mathcal{N}}"
  , lmcodet "𝒪" "calO" "{\\mathcal{O}}"
  , lmcodet "𝒫" "calP" "{\\mathcal{P}}"
  , lmcodet "𝒬" "calQ" "{\\mathcal{Q}}"
  , lmcodet "ℛ" "calR" "{\\mathcal{R}}"
  , lmcodet "𝒮" "calS" "{\\mathcal{S}}"
  , lmcodet "𝒯" "calT" "{\\mathcal{T}}"
  , lmcodet "𝒰" "calU" "{\\mathcal{U}}"
  , lmcodet "𝒱" "calV" "{\\mathcal{V}}"
  , lmcodet "𝒲" "calW" "{\\mathcal{W}}"
  , lmcodet "𝒳" "calX" "{\\mathcal{X}}"
  , lmcodet "𝒴" "calY" "{\\mathcal{Y}}"
  , lmcodet "𝒵" "calZ" "{\\mathcal{Z}}"
  , lmcodet "𝒶" "cala" "{\\mathcal{a}}"
  , lmcodet "𝒷" "calb" "{\\mathcal{b}}"
  , lmcodet "𝒸" "calc" "{\\mathcal{c}}"
  , lmcodet "𝒹" "cald" "{\\mathcal{d}}"
  , lmcodet "ℯ" "cale" "{\\mathcal{e}}"
  , lmcodet "𝒻" "calf" "{\\mathcal{f}}"
  , lmcodet "ℊ" "calg" "{\\mathcal{g}}"
  , lmcodet "𝒽" "calh" "{\\mathcal{h}}"
  , lmcodet "𝒾" "cali" "{\\mathcal{i}}"
  , lmcodet "𝒿" "calj" "{\\mathcal{j}}"
  , lmcodet "𝓀" "calk" "{\\mathcal{k}}"
  , lmcodet "𝓁" "call" "{\\mathcal{l}}"
  , lmcodet "𝓂" "calm" "{\\mathcal{m}}"
  , lmcodet "𝓃" "caln" "{\\mathcal{n}}"
  , lmcodet "ℴ" "calo" "{\\mathcal{o}}"
  , lmcodet "𝓅" "calp" "{\\mathcal{p}}"
  , lmcodet "𝓆" "calq" "{\\mathcal{q}}"
  , lmcodet "𝓇" "calr" "{\\mathcal{r}}"
  , lmcodet "𝓈" "cals" "{\\mathcal{s}}"
  , lmcodet "𝓉" "calt" "{\\mathcal{t}}"
  , lmcodet "𝓊" "calu" "{\\mathcal{u}}"
  , lmcodet "𝓋" "calv" "{\\mathcal{v}}"
  , lmcodet "𝓌" "calw" "{\\mathcal{w}}"
  , lmcodet "𝓍" "calx" "{\\mathcal{x}}"
  , lmcodet "𝓎" "caly" "{\\mathcal{y}}"
  , lmcodet "𝓏" "calz" "{\\mathcal{z}}"

  -- Roman Bold Caligraphy
  , code "𝓐" "bdcalA"
  , code "𝓑" "bdcalB"
  , code "𝓒" "bdcalC"
  , code "𝓓" "bdcalD"
  , code "𝓔" "bdcalE"
  , code "𝓕" "bdcalF"
  , code "𝓖" "bdcalG"
  , code "𝓗" "bdcalH"
  , code "𝓘" "bdcalI"
  , code "𝓙" "bdcalJ"
  , code "𝓚" "bdcalK"
  , code "𝓛" "bdcalL"
  , code "𝓜" "bdcalM"
  , code "𝓝" "bdcalN"
  , code "𝓞" "bdcalO"
  , code "𝓟" "bdcalP"
  , code "𝓠" "bdcalQ"
  , code "𝓡" "bdcalR"
  , code "𝓢" "bdcalS"
  , code "𝓣" "bdcalT"
  , code "𝓤" "bdcalU"
  , code "𝓥" "bdcalV"
  , code "𝓦" "bdcalW"
  , code "𝓧" "bdcalX"
  , code "𝓨" "bdcalY"
  , code "𝓩" "bdcalZ"
  , code "𝓪" "bdcala"
  , code "𝓫" "bdcalb"
  , code "𝓬" "bdcalc"
  , code "𝓭" "bdcald"
  , code "𝓮" "bdcale"
  , code "𝓯" "bdcalf"
  , code "𝓰" "bdcalg"
  , code "𝓱" "bdcalh"
  , code "𝓲" "bdcali"
  , code "𝓳" "bdcalj"
  , code "𝓴" "bdcalk"
  , code "𝓵" "bdcall"
  , code "𝓶" "bdcalm"
  , code "𝓷" "bdcaln"
  , code "𝓸" "bdcalo"
  , code "𝓹" "bdcalp"
  , code "𝓺" "bdcalq"
  , code "𝓻" "bdcalr"
  , code "𝓼" "bdcals"
  , code "𝓽" "bdcalt"
  , code "𝓾" "bdcalu"
  , code "𝓿" "bdcalv"
  , code "𝔀" "bdcalw"
  , code "𝔁" "bdcalx"
  , code "𝔂" "bdcaly"
  , code "𝔃" "bdcalz"

  -- Roman Fraktur
  , lmcode "𝔄" "frakA" "{\\mathfrak{A}}"
  , lmcode "𝔅" "frakB" "{\\mathfrak{B}}"
  , lmcode "ℭ" "frakC" "{\\mathfrak{C}}"
  , lmcode "𝔇" "frakD" "{\\mathfrak{D}}"
  , lmcode "𝔈" "frakE" "{\\mathfrak{E}}"
  , lmcode "𝔉" "frakF" "{\\mathfrak{F}}"
  , lmcode "𝔊" "frakG" "{\\mathfrak{G}}"
  , lmcode "ℌ" "frakH" "{\\mathfrak{H}}"
  , lmcode "ℑ" "frakI" "{\\mathfrak{I}}"
  , lmcode "𝔍" "frakJ" "{\\mathfrak{J}}"
  , lmcode "𝔎" "frakK" "{\\mathfrak{K}}"
  , lmcode "𝔏" "frakL" "{\\mathfrak{L}}"
  , lmcode "𝔐" "frakM" "{\\mathfrak{M}}"
  , lmcode "𝔑" "frakN" "{\\mathfrak{N}}"
  , lmcode "𝔒" "frakO" "{\\mathfrak{O}}"
  , lmcode "𝔓" "frakP" "{\\mathfrak{P}}"
  , lmcode "𝔔" "frakQ" "{\\mathfrak{Q}}"
  , lmcode "ℜ" "frakR" "{\\mathfrak{R}}"
  , lmcode "𝔖" "frakS" "{\\mathfrak{S}}"
  , lmcode "𝔗" "frakT" "{\\mathfrak{T}}"
  , lmcode "𝔘" "frakU" "{\\mathfrak{U}}"
  , lmcode "𝔙" "frakV" "{\\mathfrak{V}}"
  , lmcode "𝔚" "frakW" "{\\mathfrak{W}}"
  , lmcode "𝔛" "frakX" "{\\mathfrak{X}}"
  , lmcode "𝔜" "frakY" "{\\mathfrak{Y}}"
  , lmcode "ℨ" "frakZ" "{\\mathfrak{Z}}"
  , lmcode "𝔞" "fraka" "{\\mathfrak{a}}"
  , lmcode "𝔟" "frakb" "{\\mathfrak{b}}"
  , lmcode "𝔠" "frakc" "{\\mathfrak{c}}"
  , lmcode "𝔡" "frakd" "{\\mathfrak{d}}"
  , lmcode "𝔢" "frake" "{\\mathfrak{e}}"
  , lmcode "𝔣" "frakf" "{\\mathfrak{f}}"
  , lmcode "𝔤" "frakg" "{\\mathfrak{g}}"
  , lmcode "𝔥" "frakh" "{\\mathfrak{h}}"
  , lmcode "𝔦" "fraki" "{\\mathfrak{i}}"
  , lmcode "𝔧" "frakj" "{\\mathfrak{j}}"
  , lmcode "𝔨" "frakk" "{\\mathfrak{k}}"
  , lmcode "𝔩" "frakl" "{\\mathfrak{l}}"
  , lmcode "𝔪" "frakm" "{\\mathfrak{m}}"
  , lmcode "𝔫" "frakn" "{\\mathfrak{n}}"
  , lmcode "𝔬" "frako" "{\\mathfrak{o}}"
  , lmcode "𝔭" "frakp" "{\\mathfrak{p}}"
  , lmcode "𝔮" "frakq" "{\\mathfrak{q}}"
  , lmcode "𝔯" "frakr" "{\\mathfrak{r}}"
  , lmcode "𝔰" "fraks" "{\\mathfrak{s}}"
  , lmcode "𝔱" "frakt" "{\\mathfrak{t}}"
  , lmcode "𝔲" "fraku" "{\\mathfrak{u}}"
  , lmcode "𝔳" "frakv" "{\\mathfrak{v}}"
  , lmcode "𝔴" "frakw" "{\\mathfrak{w}}"
  , lmcode "𝔵" "frakx" "{\\mathfrak{x}}"
  , lmcode "𝔶" "fraky" "{\\mathfrak{y}}"
  , lmcode "𝔷" "frakz" "{\\mathfrak{z}}"

  -- Roman Bold Fraktur
  , code "𝕬" "bdfrakA"
  , code "𝕭" "bdfrakB"
  , code "𝕮" "bdfrakC"
  , code "𝕯" "bdfrakD"
  , code "𝕰" "bdfrakE"
  , code "𝕱" "bdfrakF"
  , code "𝕲" "bdfrakG"
  , code "𝕳" "bdfrakH"
  , code "𝕴" "bdfrakI"
  , code "𝕵" "bdfrakJ"
  , code "𝕶" "bdfrakK"
  , code "𝕷" "bdfrakL"
  , code "𝕸" "bdfrakM"
  , code "𝕹" "bdfrakN"
  , code "𝕺" "bdfrakO"
  , code "𝕻" "bdfrakP"
  , code "𝕼" "bdfrakQ"
  , code "𝕽" "bdfrakR"
  , code "𝕾" "bdfrakS"
  , code "𝕿" "bdfrakT"
  , code "𝖀" "bdfrakU"
  , code "𝖁" "bdfrakV"
  , code "𝖂" "bdfrakW"
  , code "𝖃" "bdfrakX"
  , code "𝖄" "bdfrakY"
  , code "𝖅" "bdfrakZ"
  , code "𝖆" "bdfraka"
  , code "𝖇" "bdfrakb"
  , code "𝖈" "bdfrakc"
  , code "𝖉" "bdfrakd"
  , code "𝖊" "bdfrake"
  , code "𝖋" "bdfrakf"
  , code "𝖌" "bdfrakg"
  , code "𝖍" "bdfrakh"
  , code "𝖎" "bdfraki"
  , code "𝖏" "bdfrakj"
  , code "𝖐" "bdfrakk"
  , code "𝖑" "bdfrakl"
  , code "𝖒" "bdfrakm"
  , code "𝖓" "bdfrakn"
  , code "𝖔" "bdfrako"
  , code "𝖕" "bdfrakp"
  , code "𝖖" "bdfrakq"
  , code "𝖗" "bdfrakr"
  , code "𝖘" "bdfraks"
  , code "𝖙" "bdfrakt"
  , code "𝖚" "bdfraku"
  , code "𝖛" "bdfrakv"
  , code "𝖜" "bdfrakw"
  , code "𝖝" "bdfrakx"
  , code "𝖞" "bdfraky"
  , code "𝖟" "bdfrakz"

  -- Roman Blackboard Bold
  , lmcodet "𝔸" "bbA" "{\\mathbb{A}}"
  , lmcodet "𝔹" "bbB" "{\\mathbb{B}}"
  , lmcodet "ℂ" "bbC" "{\\mathbb{C}}"
  , lmcodet "𝔻" "bbD" "{\\mathbb{D}}"
  , lmcodet "𝔼" "bbE" "{\\mathbb{E}}"
  , lmcodet "𝔽" "bbF" "{\\mathbb{F}}"
  , lmcodet "𝔾" "bbG" "{\\mathbb{G}}"
  , lmcodet "ℍ" "bbH" "{\\mathbb{H}}"
  , lmcodet "𝕀" "bbI" "{\\mathbb{I}}"
  , lmcodet "𝕁" "bbJ" "{\\mathbb{J}}"
  , lmcodet "𝕂" "bbK" "{\\mathbb{K}}"
  , lmcodet "𝕃" "bbL" "{\\mathbb{L}}"
  , lmcodet "𝕄" "bbM" "{\\mathbb{M}}"
  , lmcodet "ℕ" "bbN" "{\\mathbb{N}}"
  , lmcodet "𝕆" "bbO" "{\\mathbb{O}}"
  , lmcodet "ℙ" "bbP" "{\\mathbb{P}}"
  , lmcodet "ℚ" "bbQ" "{\\mathbb{Q}}"
  , lmcodet "ℝ" "bbR" "{\\mathbb{R}}"
  , lmcodet "𝕊" "bbS" "{\\mathbb{S}}"
  , lmcodet "𝕋" "bbT" "{\\mathbb{T}}"
  , lmcodet "𝕌" "bbU" "{\\mathbb{U}}"
  , lmcodet "𝕍" "bbV" "{\\mathbb{V}}"
  , lmcodet "𝕎" "bbW" "{\\mathbb{W}}"
  , lmcodet "𝕏" "bbX" "{\\mathbb{X}}"
  , lmcodet "𝕐" "bbY" "{\\mathbb{Y}}"
  , lmcodet "ℤ" "bbZ" "{\\mathbb{Z}}"
  , lmcodet "𝕒" "bba" "{\\mathbb{a}}"
  , lmcodet "𝕓" "bbb" "{\\mathbb{b}}"
  , lmcodet "𝕔" "bbc" "{\\mathbb{c}}"
  , lmcodet "𝕕" "bbd" "{\\mathbb{d}}"
  , lmcodet "𝕖" "bbe" "{\\mathbb{e}}"
  , lmcodet "𝕗" "bbf" "{\\mathbb{f}}"
  , lmcodet "𝕘" "bbg" "{\\mathbb{g}}"
  , lmcodet "𝕙" "bbh" "{\\mathbb{h}}"
  , lmcodet "𝕚" "bbi" "{\\mathbb{i}}"
  , lmcodet "𝕛" "bbj" "{\\mathbb{j}}"
  , lmcodet "𝕜" "bbk" "{\\mathbb{k}}"
  , lmcodet "𝕝" "bbl" "{\\mathbb{l}}"
  , lmcodet "𝕞" "bbm" "{\\mathbb{m}}"
  , lmcodet "𝕟" "bbn" "{\\mathbb{n}}"
  , lmcodet "𝕠" "bbo" "{\\mathbb{o}}"
  , lmcodet "𝕡" "bbp" "{\\mathbb{p}}"
  , lmcodet "𝕢" "bbq" "{\\mathbb{q}}"
  , lmcodet "𝕣" "bbr" "{\\mathbb{r}}"
  , lmcodet "𝕤" "bbs" "{\\mathbb{s}}"
  , lmcodet "𝕥" "bbt" "{\\mathbb{t}}"
  , lmcodet "𝕦" "bbu" "{\\mathbb{u}}"
  , lmcodet "𝕧" "bbv" "{\\mathbb{v}}"
  , lmcodet "𝕨" "bbw" "{\\mathbb{w}}"
  , lmcodet "𝕩" "bbx" "{\\mathbb{x}}"
  , lmcodet "𝕪" "bby" "{\\mathbb{y}}"
  , lmcodet "𝕫" "bbz" "{\\mathbb{z}}"

  -- Roman Subscripts
  , lmcodet "ₐ" "_a" "_a"
  , lmcodet "ₑ" "_e" "_e"
  , lmcodet "ₕ" "_h" "_h"
  , lmcodet "ᵢ" "_i" "_i"
  , lmcodet "ⱼ" "_j" "_j"
  , lmcodet "ₖ" "_k" "_k"
  , lmcodet "ₗ" "_l" "_l"
  , lmcodet "ₘ" "_m" "_m"
  , lmcodet "ₙ" "_n" "_n"
  , lmcodet "ₒ" "_o" "_o"
  , lmcodet "ₚ" "_p" "_p"
  , lmcodet "ᵣ" "_r" "_r"
  , lmcodet "ₛ" "_s" "_s"
  , lmcodet "ₜ" "_t" "_t"
  , lmcodet "ᵤ" "_u" "_u"
  , lmcodet "ᵥ" "_v" "_v"
  , lmcodet "ₓ" "_x" "_x"

  -- Roman Superscripts
  , lmcodet "ᴬ" "^A" "^A"
  , lmcodet "ᴮ" "^B" "^B"
  , lmcodet "ᴰ" "^D" "^D"
  , lmcodet "ᴱ" "^E" "^E"
  , lmcodet "ᴳ" "^G" "^G"
  , lmcodet "ᴴ" "^H" "^H"
  , lmcodet "ᴵ" "^I" "^I"
  , lmcodet "ᴶ" "^J" "^J"
  , lmcodet "ᴷ" "^K" "^K"
  , lmcodet "ᴸ" "^L" "^L"
  , lmcodet "ᴹ" "^M" "^M"
  , lmcodet "ᴺ" "^N" "^N"
  , lmcodet "ᴼ" "^O" "^O"
  , lmcodet "ᴾ" "^P" "^P"
  , lmcodet "ᴿ" "^R" "^R"
  , lmcodet "ᵀ" "^T" "^T"
  , lmcodet "ᵁ" "^U" "^U"
  , lmcodet "ⱽ" "^V" "^V"
  , lmcodet "ᵂ" "^W" "^W"
  , lmcodet "ᵃ" "^a" "^a"
  , lmcodet "ᵇ" "^b" "^b"
  , lmcodet "ᶜ" "^c" "^c"
  , lmcodet "ᵈ" "^d" "^d"
  , lmcodet "ᵉ" "^e" "^e"
  , lmcodet "ᶠ" "^f" "^f"
  , lmcodet "ᵍ" "^g" "^g"
  , lmcodet "ʰ" "^h" "^h"
  , lmcodet "ⁱ" "^i" "^i"
  , lmcodet "ʲ" "^j" "^j"
  , lmcodet "ᵏ" "^k" "^k"
  , lmcodet "ˡ" "^l" "^l"
  , lmcodet "ᵐ" "^m" "^m"
  , lmcodet "ⁿ" "^n" "^n"
  , lmcodet "ᵒ" "^o" "^o"
  , lmcodet "ᵖ" "^p" "^p"
  , lmcodet "ᶝ" "^q" "^q"
  , lmcodet "ʳ" "^r" "^r"
  , lmcodet "ˢ" "^s" "^s"
  , lmcodet "ᵗ" "^t" "^t"
  , lmcodet "ᵘ" "^u" "^u"
  , lmcodet "ᵛ" "^v" "^v"
  , lmcodet "ʷ" "^w" "^w"
  , lmcodet "ˣ" "^x" "^x"
  , lmcodet "ʸ" "^y" "^y"
  , lmcodet "ᶻ" "^z" "^z"

  , lmcodet "™" "^tm" "\\texttrademark"

  -- Roman Small Upper Case
  , lmcodet "ᴀ" "sca" "{\\textsc{a}}"
  , lmcodet "ʙ" "scb" "{\\textsc{b}}"
  , lmcodet "ᴄ" "scc" "{\\textsc{c}}"
  , lmcodet "ᴅ" "scd" "{\\textsc{d}}"
  , lmcodet "ᴇ" "sce" "{\\textsc{e}}"
  , lmcodet "ꜰ" "scf" "{\\textsc{f}}"
  , lmcodet "ɢ" "scg" "{\\textsc{g}}"
  , lmcodet "ʜ" "sch" "{\\textsc{h}}"
  , lmcodet "ɪ" "sci" "{\\textsc{i}}"
  , lmcodet "ᴊ" "scj" "{\\textsc{j}}"
  , lmcodet "ᴋ" "sck" "{\\textsc{k}}"
  , lmcodet "ʟ" "scl" "{\\textsc{l}}"
  , lmcodet "ᴍ" "scm" "{\\textsc{m}}"
  , lmcodet "ɴ" "scn" "{\\textsc{n}}"
  , lmcodet "ᴏ" "sco" "{\\textsc{o}}"
  , lmcodet "ᴘ" "scp" "{\\textsc{p}}"
  , lmcodet "ʀ" "scr" "{\\textsc{r}}"
  , lmcodet "ꜱ" "scs" "{\\textsc{s}}"
  , lmcodet "ᴛ" "sct" "{\\textsc{t}}"
  , lmcodet "ᴜ" "scu" "{\\textsc{u}}"
  , lmcodet "ᴠ" "scv" "{\\textsc{v}}"
  , lmcodet "ᴡ" "scw" "{\\textsc{w}}"
  , lmcodet "ʏ" "scy" "{\\textsc{y}}"
  , lmcodet "ᴢ" "scz" "{\\textsc{z}}"

  -- circled

  , lmcodet "⓪" "wc0" "\\circled{0}"
  , lmcodet "①" "wc1" "\\circled{1}"
  , lmcodet "②" "wc2" "\\circled{2}"
  , lmcodet "③" "wc3" "\\circled{3}"
  , lmcodet "④" "wc4" "\\circled{4}"
  , lmcodet "⑤" "wc5" "\\circled{5}"
  , lmcodet "⑥" "wc6" "\\circled{6}"
  , lmcodet "⑦" "wc7" "\\circled{7}"
  , lmcodet "⑧" "wc8" "\\circled{8}"
  , lmcodet "⑨" "wc9" "\\circled{9}"

  , lmcodet "Ⓐ" "wcA" "\\circled{A}"
  , lmcodet "Ⓑ" "wcB" "\\circled{B}"
  , lmcodet "Ⓒ" "wcC" "\\circled{C}"
  , lmcodet "Ⓓ" "wcD" "\\circled{D}"
  , lmcodet "Ⓔ" "wcE" "\\circled{E}"
  , lmcodet "Ⓕ" "wcF" "\\circled{F}"
  , lmcodet "Ⓖ" "wcG" "\\circled{G}"
  , lmcodet "Ⓗ" "wcH" "\\circled{H}"
  , lmcodet "Ⓘ" "wcI" "\\circled{I}"
  , lmcodet "Ⓙ" "wcJ" "\\circled{J}"
  , lmcodet "Ⓚ" "wcK" "\\circled{K}"
  , lmcodet "Ⓛ" "wcL" "\\circled{L}"
  , lmcodet "Ⓜ" "wcM" "\\circled{M}"
  , lmcodet "Ⓝ" "wcN" "\\circled{N}"
  , lmcodet "Ⓞ" "wcO" "\\circled{O}"
  , lmcodet "Ⓟ" "wcP" "\\circled{P}"
  , lmcodet "Ⓠ" "wcQ" "\\circled{Q}"
  , lmcodet "Ⓡ" "wcR" "\\circled{R}"
  , lmcodet "Ⓢ" "wcS" "\\circled{S}"
  , lmcodet "Ⓣ" "wcT" "\\circled{T}"
  , lmcodet "Ⓤ" "wcU" "\\circled{U}"
  , lmcodet "Ⓥ" "wcV" "\\circled{V}"
  , lmcodet "Ⓦ" "wcW" "\\circled{W}"
  , lmcodet "Ⓧ" "wcX" "\\circled{X}"
  , lmcodet "Ⓨ" "wcY" "\\circled{Y}"
  , lmcodet "Ⓩ" "wcZ" "\\circled{Z}"

  , lmcodet "ⓐ" "wca" "\\circled{a}"
  , lmcodet "ⓑ" "wcb" "\\circled{b}"
  , lmcodet "ⓒ" "wcc" "\\circled{c}"
  , lmcodet "ⓓ" "wcd" "\\circled{d}"
  , lmcodet "ⓔ" "wce" "\\circled{e}"
  , lmcodet "ⓕ" "wcf" "\\circled{f}"
  , lmcodet "ⓖ" "wcg" "\\circled{g}"
  , lmcodet "ⓗ" "wch" "\\circled{h}"
  , lmcodet "ⓘ" "wci" "\\circled{i}"
  , lmcodet "ⓙ" "wcj" "\\circled{j}"
  , lmcodet "ⓚ" "wck" "\\circled{k}"
  , lmcodet "ⓛ" "wcl" "\\circled{l}"
  , lmcodet "ⓜ" "wcm" "\\circled{m}"
  , lmcodet "ⓝ" "wcn" "\\circled{n}"
  , lmcodet "ⓞ" "wco" "\\circled{o}"
  , lmcodet "ⓟ" "wcp" "\\circled{p}"
  , lmcodet "ⓠ" "wcq" "\\circled{q}"
  , lmcodet "ⓡ" "wcr" "\\circled{r}"
  , lmcodet "ⓢ" "wcs" "\\circled{s}"
  , lmcodet "ⓣ" "wct" "\\circled{t}"
  , lmcodet "ⓤ" "wcu" "\\circled{u}"
  , lmcodet "ⓥ" "wcv" "\\circled{v}"
  , lmcodet "ⓦ" "wcw" "\\circled{w}"
  , lmcodet "ⓧ" "wcx" "\\circled{x}"
  , lmcodet "ⓨ" "wcy" "\\circled{y}"
  , lmcodet "ⓩ" "wcz" "\\circled{z}"

  -- Roman Accents and Gylphs
  , ltcodet "À" "A`"  "\\`A"
  , ltcodet "Á" "A'"  "\\'A"
  , ltcodet "Ä" "A.." "\\\"A"
  , ltcodet "Â" "A^"  "\\^A"
  , ltcodet "Ç" "C,"  "\\,C"
  , ltcodet "È" "E`"  "\\`E"
  , ltcodet "É" "E'"  "\\'E"
  , ltcodet "Ê" "E^"  "\\^E"
  , ltcodet "Í" "I'" "\\'I"
  , ltcodet "Ì" "I`" "\\`I"
  , ltcodet "Ï" "I.." "\\\"I"
  , ltcodet "Ô" "O^"  "\\^O"
  , ltcodet "Ö" "O.." "\\\"O"
  , ltcodet "Ú" "U'" "\\'U"
  , ltcodet "Ù" "U`"  "\\'U"
  , ltcodet "Û" "U^"  "\\^U"
  , ltcodet "Ü" "U.." "\\\"U"
  , ltcodet "à" "a`"  "\\`a"
  , ltcodet "á" "a'"  "\\'a"
  , ltcodet "ä" "a.." "\\\"a"
  , ltcodet "â" "a^"  "\\^a"
  , ltcodet "ç" "c,"  "\\,c"
  , ltcodet "è" "e`"  "\\`e"
  , ltcodet "é" "e'"  "\\'e"
  , ltcodet "ê" "e^"  "\\^e"
  , ltcodet "í" "i'" "\\'i"
  , ltcodet "ì" "i`" "\\`i"
  , ltcodet "ï" "i.." "\\\"i"
  , ltcodet "ô" "o^"  "\\^o"
  , ltcodet "ö" "o.." "\\\"o"
  , ltcodet "ú" "u'" "\\'u"
  , ltcodet "ù" "u`"  "\\'u"
  , ltcodet "û" "u^"  "\\^u"
  , ltcodet "ü" "u.." "\\\"u"
  , ltcodet "æ" "ae"  "\\ae"
  , ltcodet "œ" "oe"  "\\oe"
  , ltcodet "ễ" "e^~" "\\begingroup{}\\fontencoding{T5}\\selectfont \\~\\ecircumflex\\endgroup{}"
  , code "ø" "osl"
  , code "Ø" "Osl"
  , code "ᴓ" "osls"

  -- Box Drawing
  , code "─" "boxlr"
  , code "━" "boxLR"
  , code "│" "boxtb"
  , code "┃" "boxTB"
  , code "┄" "boxlr-"
  , code "┅" "boxLR-"
  , code "┆" "boxtb-"
  , code "┇" "boxTB-"
  , code "┈" "boxlr."
  , code "┉" "boxLR."
  , code "┊" "boxtb."
  , code "┋" "boxTB."
  , code "┌" "boxbr"
  , code "┍" "boxbR"
  , code "┎" "boxBr"
  , code "┏" "boxBR"
  , code "┐" "boxbl"
  , code "┑" "boxbL"
  , code "┒" "boxBl"
  , code "┓" "boxBL"
  , code "└" "boxtr"
  , code "┕" "boxtR"
  , code "┖" "boxTr"
  , code "┗" "boxTR"
  , code "┘" "boxtl"
  , code "┙" "boxtL"
  , code "┚" "boxTl"
  , code "┛" "boxTL"
  , code "├" "boxtbr"
  , code "┝" "boxtbR"
  , code "┞" "boxTbr"
  , code "┟" "boxtBr"
  , code "┠" "boxTBr"
  , code "┡" "boxTbR"
  , code "┢" "boxtBR"
  , code "┣" "boxTBR"
  , code "┤" "boxtbl"
  , code "┥" "boxtbL"
  , code "┦" "boxTbl"
  , code "┧" "boxtBl"
  , code "┨" "boxTBl"
  , code "┩" "boxTbL"
  , code "┪" "boxtBL"
  , code "┫" "boxTBL"
  , code "┬" "boxblr"
  , code "┭" "boxbLr"
  , code "┮" "boxblR"
  , code "┯" "boxbLR"
  , code "┰" "boxBlr"
  , code "┱" "boxBLr"
  , code "┲" "boxBlR"
  , code "┳" "boxBLR"
  , code "┴" "boxtlr"
  , code "┵" "boxtLr"
  , code "┶" "boxtlR"
  , code "┷" "boxtLR"
  , code "┸" "boxTlr"
  , code "┹" "boxTLr"
  , code "┺" "boxTlR"
  , code "┻" "boxTLR"
  , code "┼" "boxtblr"
  , code "┽" "boxtbLr"
  , code "┾" "boxtblR"
  , code "┿" "boxtbLR"
  , code "╀" "boxTblr"
  , code "╁" "boxtBlr"
  , code "╂" "boxTBlr"
  , code "╃" "boxTbLr"
  , code "╄" "boxTblR"
  , code "╅" "boxtBLr"
  , code "╆" "boxtBlR"
  , code "╇" "boxTbLR"
  , code "╈" "boxtBLR"
  , code "╉" "boxTBLr"
  , code "╊" "boxTBlR"
  , code "╋" "boxTBLR"
  , code "╌" "boxlr:"
  , code "╍" "boxLR:"
  , code "╎" "boxtb:"
  , code "╏" "boxTB:"
  , code "═" "boxLR="
  , code "║" "boxTB="
  , code "╒" "boxbR="
  , code "╓" "boxBr="
  , code "╔" "boxBR="
  , code "╕" "boxbL="
  , code "╖" "boxBl="
  , code "╗" "boxBL="
  , code "╘" "boxtR="
  , code "╙" "boxTr="
  , code "╚" "boxTR="
  , code "╛" "boxtL="
  , code "╜" "boxTl="
  , code "╝" "boxTL="
  , code "╞" "boxtbR="
  , code "╟" "boxTBr="
  , code "╠" "boxTBR="
  , code "╡" "boxtbL="
  , code "╢" "boxTBl="
  , code "╣" "boxTBL="
  , code "╤" "boxbLR="
  , code "╥" "boxBlr="
  , code "╦" "boxBLR="
  , code "╧" "boxtLR="
  , code "╨" "boxTlr="
  , code "╩" "boxTLR="
  , code "╪" "boxtbLR="
  , code "╫" "boxTBlr="
  , code "╬" "boxTBLR="
  , code "╭" "boxbrc"
  , code "╮" "boxblc"
  , code "╯" "boxtlc"
  , code "╰" "boxtrc"
  , code "╱" "boxtrbl"
  , code "╲" "boxtlbr"
  , code "╳" "boxx"
  , code "╴" "boxl"
  , code "╵" "boxt"
  , code "╶" "boxr"
  , code "╷" "boxb"
  , code "╸" "boxL"
  , code "╹" "boxT"
  , code "╺" "boxR"
  , code "╻" "boxB"
  , code "╼" "boxlR"
  , code "╽" "boxtB"
  , code "╾" "boxLr"
  , code "╿" "boxTb"

  -- Spaces
  , code " " "nbsp"
  , code " " "ensp"
  , code " " "fgsp"
  ]
