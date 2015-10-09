{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module TclParse
  ( TclWord(..)
  , runParse
  , parseList
  , parseVar
  , parseRichStr
  , parseHex
  , parseInt
  , parseSub
  , TokCmd
  , SubCmd
  , parseSubst
  , parseSubstAll
  , Subst(..)
  , SubstArgs(..)
  , allSubstArgs
  , tclParseTests
  ) where


import BSParse
-- import Text.Parsec.ByteString
-- import Text.ParserCombinators.Parsec.Prim ((<|>))
-- import Text.ParserCombinators.Parsec.Combinator
-- import Text.Parsec
import Util 
import qualified Data.ByteString.Char8 as B
import Data.Char (digitToInt,isHexDigit)
import Test.HUnit


data TclWord
  = Word       !BString
  | Subcommand SubCmd !BString
  | NoSub      !BString
  | Expand     TclWord
    deriving (Show,Eq)

type TokCmd = (TclWord, [TclWord])
type SubCmd = [TokCmd]

runParse :: Parser SubCmd
runParse = parseStatements

asCmds lst = [(c,a) | (c:a) <- lst]

parseStatements :: Parser SubCmd
-- parseStatements = trimmed  ((parseStatement `sepBy` stmtSep) `pass` trailing) `wrapWith` asCmds
-- parseStatements = trimmed (parseStatement `sepBy` stmtSep)  `wrapWith` asCmds
-- parseStatements = ((parseStatement `sepBy` stmtSep) `pass` trailing) `wrapWith` asCmds
parseStatements = trimmed (parseStatement`sepBut1` stmtSep <* trailing ) `wrapWith` asCmds
--  where stmtSep = eatSpaces *> parseMany1 (parseOneOf ("\n;" :: [Char])) <* eatSpaces
 where stmtSep = parseOneOf ("\n;" :: [Char])
       trailing = parseMany stmtSep

-- eatSpaces *> return [],
parseStatement :: Parser [TclWord]
parseStatement = try (choose [eatComment, parseTokens, eof *> return []])
 where
  eatComment = parseComment >> return []

parseComment = do
    pchar '#'
    parseMany (ignored <|> escapedCharBString)
    eatSpaces
 where ignored = try $ B.singleton <$> parseNoneOf "\n\\" "not newline or slash"

parseTokens :: Parser [TclWord]
parseTokens = eatSpaces *> parseToken `sepBut` spaceSep
-- <* many spaceSep
--  where
spaceSep = parseOneOf " \t" <?> "spaces"

parseToken :: Parser TclWord
parseToken = do
--    eatSpaces
   h <- lookAhead anyChar <?> "token"
   case h of
--      ' '  -> eatSpaces >> parseToken
     '{'  -> try parseExpand <|> parseNoSub
     '"'  -> parseRichStr `wrapWith` Word
     '\\' -> handleEsc
--      ' '  -> return []
     _    -> wordToken `wrapWith` Word
 where parseNoSub = NoSub <$> parseBlock
       parseExpand = parseLit "{*}" >> Expand <$> parseToken

parseRichStr = quotes (inside `wrapWith` B.concat)
 where noquotes = try $ B.singleton <$> parseNoneOf "\"\\[$" "non-quote chars"
       inside = parseMany $ choose [noquotes, escapedCharBString, consumed parseSub, inner_var]
       inner_var = consumed (try parseVar <|> bchar '$')

parseSub :: Parser SubCmd
parseSub = brackets parseStatements

handleEsc :: Parser TclWord
handleEsc = try line_continue <|> esc_word
 where line_continue = parseLit "\\\n" >> eatSpaces >> parseToken
       esc_word = chain [escapedCharBString, tryGet wordToken] `wrapWith` Word

trimmed p = tryWhite *> p <* tryWhite
 where tryWhite = try (B.concat <$> many1 whiteSpace) <|> return B.empty

whiteSpace = getPred1 (`B.elem` " \t\n") "whitespace"

parseVar :: Parser BString
parseVar = pchar '$' >> parseVarBody

parseVarBody = chain
  [ initial
  , tryGet getNS
  , tryGet parseInd
  ]
 where getNS = chain [sep, varTerm, tryGet getNS]
       initial = chain [tryGet sep, varTerm]
       sep = parseLit "::"
       varTerm = try (getPred1 wordChar "word") <|> braceVar
       parseInd = chain [bchar '(', getPred (/= ')'), bchar ')']

wordToken = consumed (parseMany1 (try someVar <|> try inner <|> someCmd))
 where simple = B.singleton <$> parseNoneOf " $[]\n\t;\\" "inner word"
       inner = consumed (parseMany1 (try simple <|> escapedCharBString))
       someVar = consumed parseVar
       someCmd = consumed parseSub

ensureEof p = p <* eof
-- ensureEof p = p

parseList = ensureEof parseList_

parseList_ :: Parser [BString]
parseList_ = trimmed listItems
 where listItems = listElt `sepBut` whiteSpace

listElt :: Parser BString
listElt = try parseBlock <|> try parseStr <|> getListItem

getListItem = do
  s <- getInput
  let (w,n) = B.splitAt (listItemEnd s) s
  if B.null w
   then fail "can't parse list item"
   else count (B.length w) anyChar *> return w

listItemEnd s = inner 0 False where 
   isTerminal = (`B.elem` "{}\" \t\n")
   inner i esc
     | i == B.length s = i
     | esc             = inner (i+1) False
     | otherwise =
         case B.index s i of
          '\\' -> inner (i+1) True
          v  -> if isTerminal v
                 then i
                 else inner (i+1) False

parseInt :: Parser Int
parseInt = (checkStartsWith '0' >> try (try parseHex <|> parseDecInt)) <|> parseDecInt
parseHex = hex_str `wrapWith` h2d
 where
   hex_str = parseLit "0x" >> getPred1 isHexDigit "hex digit"
   h2d = B.foldl' (\a c -> a * 16 + (digitToInt c)) 0

data Subst
  = SStr !BString
  | SVar !BString
  | SCmd SubCmd
     deriving (Eq,Show)

data SubstArgs = SubstArgs { s_vars :: Bool, s_esc :: Bool, s_cmds :: Bool } deriving Show

allSubstArgs = SubstArgs True True True
parseSubstAll = parseSubst allSubstArgs

parseSubst :: SubstArgs -> Parser [Subst]
parseSubst (SubstArgs vars esc cmds) = inner `wrapWith` sconcat
 where no_special = try $ B.singleton <$> parseNoneOf "\\[$" "non-special chars"
       inner = parseMany (choose [ try (st no_special)
                                 , try (may vars SVar parseVar)
                                 , try (may cmds SCmd parseSub)
                                 , try tryEsc
                                 , st1 parseAny
                                 ])
       st x = x `wrapWith` SStr
       st1 x = x `wrapWith` (SStr . B.singleton)
       may b c f = if b then f `wrapWith` c else st (consumed f)
       tryEsc = if esc then (SStr <$> escChar) else fail "no esc"
       escChar = pchar '\\' >>  escCharStr <$> parseAny
       sconcat [] = []
       sconcat (SStr s:xs) = let (sl,r) = spanStrs xs []
                             in SStr (B.concat (s:sl)) : sconcat r 
       sconcat (t:xs) = t : sconcat xs
       spanStrs (SStr x:xs) a = spanStrs xs (x:a)
       spanStrs rst a = (reverse a,rst)
--        sreduce lst = case lst of
--                []                 -> []
--                (SStr x:SStr y:xs) -> sreduce ((SStr (B.append x y)):xs)
--                (x:xs) -> x : sreduce xs

escCharStr = B.singleton . escapeChar
{-# INLINE escapeChar #-}

escapeChar c = case c of
          'n' -> '\n'
          't' -> '\t'
          'a' -> '\a'
          _  -> c


tclParseTests = TestList
  [ runParseTests
  , parseVarBodyTests
  , parseListTests
  , parseTokensTests
  , wordTokenTests
  , parseRichStrTests
  , commentTests
  , parseSubstTests
  ]

commentTests = "parseComment" ~: TestList
  [ "# hey there" `should_be` ""
  , "# hey there \n FISH" `should_be` "\n FISH"
  , "# hey there \\\n FISH" `should_be` ""
  ]
  where should_be str res = (B.unpack str) ~: Right ((),res) ~=? runParser1 parseComment str

runParseTests = "runParse" ~: TestList
  [ "one token" ~: pr ["exit"] ?=? "exit"
  , "multi-line" ~: pr ["puts", "44"] ?=? " puts \\\n   44"
  , "escaped space" ~: pr ["puts", "\\ "] ?=? " puts \\ "
  , "empty" ~: ([],"") ?=? " "
  , "empty2" ~: ([],"") ?=? ""
  , "unmatched" ~: "{ { }" `should_fail` ()
  , "a b " ~: "a b " `should_be` ["a", "b"]
  , "\n \na b\n" `should_be` ["a", "b"]
  , "two vars" ~: pr ["puts", "$one$two"] ?=? "puts $one$two"
  , "brace inside" ~: pr ["puts", "x{$a}x"] ?=? "puts x{$a}x"
  , "brace" ~: pr ["puts", "${oh no}"] ?=? "puts ${oh no}"
  , "arr 1" ~: pr ["set","buggy(4)", "11"] ?=? "set buggy(4) 11"
  , "arr 2" ~: pr ["set","buggy($bean)", "11"] ?=? "set buggy($bean) 11"
  , "arr 3" ~: pr ["set","buggy($bean)", "$koo"] ?=? "set buggy($bean) $koo"
  , "arr 4" ~: pr ["set","buggy($bean)", "${wow}"] ?=? "set buggy($bean) ${wow}"
  , "arr w/ esc index" ~: pr ["set","x(\\))", "4"] ?=? "set x(\\)) 4"
  , "quoted ws arr" ~: pr ["set","arr(1 2)", "4"] ?=? "set \"arr(1 2)\" 4"
  , "hashed num" ~: pr ["uplevel", "#1", "exit"] ?=? "uplevel #1 exit"
  , "command appended" ~: pr ["set", "val", "x[nop 4]"] ?=? "set val x[nop 4]"
  , "unquoted ws arr" ~: pr ["puts","$arr(1 2)"] ?=? "puts $arr(1 2)"
  , "expand" ~: ([(Word "incr", [Expand (Word "$boo")])], "") ?=? "incr {*}$boo"
  , "no expand" ~: ([(Word "incr", [NoSub "*", Word "$boo"])], "") ?=? "incr {*} $boo"
  , "with newline" ~: "puts HI;; \n ;" `should_be` ["puts", "HI"]
  , "subcmd starter" ~: "puts [nop]Cat" `should_be` ["puts", "[nop]Cat"]
  ]
 where should_fail str () = (runParser1 runParse str) `should_fail_` ()
       should_be str res = Right (pr res) ~=? (runParser1 runParse str)
       (?=?) (res,r) str = Right (res, r) ~=? runParser1 runParse str
       pr (x:xs) = ([(Word x, map Word xs)], "")
       pr []     = error "bad test!"

wordTokenTests = "wordToken" ~: TestList
  [ "empty" ~: "" `should_fail` ()
  , "Simple2" ~: ("$whoa", "") ?=? "$whoa"
  , "Simple with bang" ~: ("whoa!", " ") ?=? "whoa! "
  , "braced, then normal" ~: valid_word "${x}$x"
  , "normal, then cmd" ~: ("fish[nop 5]", "") ?=? "fish[nop 5]"
  , "cmd, misc, then cmd" ~: ("[nop 2]::[nop 2]", "") ?=? "[nop 2]::[nop 2]"
  , "non-var, then var" ~: ("**$x", "") ?=? "**$x"
  , "non-var, then var w/ space" ~: valid_word "**${a b}"
  , "escaped" ~: valid_word "x\\ y"
  , "inner bracket" ~: valid_word "a{{"
  , "inner bracket 2" ~: valid_word "a}|"
  ]
 where should_fail str _ = (runParser1 wordToken str) `should_fail_` ()
       (?=?) res str = Right res ~=? runParser1 wordToken str
       should_be str res = Right res ~=? runParser1 wordToken str
       valid_word x = x `should_be` (x,"")

parseVarBodyTests = "parseVarBody" ~: TestList
  [ "empty string" ~: "" `should_fail` ()
  , "standard" ~: "boo" ?=> ("boo", "")
  , "global" ~: "::boo" ?=> ("::boo", "")
  , "arr1" ~: "boo(one)" ?=> ("boo(one)", "")
  , "ns arr1" ~: "::big::boo(one)" ?=> ("::big::boo(one)", "")
  , "::big(3)$::boo(one)" ?=> ("::big(3)", "$::boo(one)")
  , "triple" ~: "::one::two::three" ?=> ("::one::two::three","")
  , "brace" ~: "::one::{t o}::three" ?=> ("::one::t o::three","")
  , "brace2" ~: "{O M G!}" ?=> ("O M G!","")
  , "nest brace1" ~: "{a{nest}" ?=> ("a{nest","")
  , "nest brace" ~: "{a{nest}}" `should_fail` ()
  , "mid paren" ~: "::one::two(1)::three" ?=> ("::one::two(1)", "::three")
  , "\\( in index" ~: "x(\\()" ?=> ("x(\\()", "")
  ]
 where (?=>) str (p,r) = Right (p, r) ~=? runParser1 parseVarBody str
       should_fail a () = B.unpack a ~: (runParser1 parseVarBody a) `should_fail_` ()

parseListTests = "parseList" ~: TestList
  [ " x " `should_be` ["x"]
  , ""   `should_be` []
  , "\t \t" `should_be` []
  , " x y " `should_be` ["x", "y"]
  , "x y" ~: "x y" ?=> ["x", "y"]
  , "x { y {z}}" `should_be` ["x", " y {z}"]
  , "x { y 0 }" ~: "x { y 0 }" ?=> ["x", " y 0 "]
  , "x [puts yay]" ~: "x [puts yay]" ?=> ["x", "[puts", "yay]"]
  , " y { \\{ \\{ \\{ } { x }" `should_be` ["y", " \\{ \\{ \\{ ", " x "]
  , "unmatched fail" ~: fails " { { "
  , "x {y 0}" ~: "x {y 0}" ?=> ["x", "y 0"]
  , "with nl" ~: "x  1 \n y 2 \n z 3" ?=> ["x", "1", "y", "2", "z", "3"]
  , "escaped1" ~: "x \\{ z" ?=> ["x", "\\{", "z"]
  , "no whitespace" ~: fails "\"x\"\"y\""
  ]
 where (?=>) str res = Right res ~=? runParser parseList () "" str
       fails str = (runParser1 parseList str) `should_fail_` ()
       should_be str res = (B.unpack str) ~: Right res ~=? runParser parseList () "" str

parseRichStrTests = "parseRichStr" ~: TestList [
       full_parse ""
      ,full_parse "\\\""
      ,full_parse "how [list \"are\"] you?"
      ,full_parse "eat [list {\"}]"
      ,full_parse "say ${\"}"
      ,full_parse "dolla $ bill"
   ]
  where should_be str res = (B.unpack str) ~: Right res ~=? runParser1 parseRichStr str
        full_parse str = (B.concat ["\"", str, "\""]) `should_be` (str,"")

parseTokensTests = "parseTokens" ~: TestList [
     " x " ~: "x" ?=> ([Word "x"], "")
     ," x y " ~: " x y " ?=> ([Word "x", Word "y"], " ")
     ,"x y" ~: "x y" ?=> ([Word "x", Word "y"], "")
     ,"x { y 0 }" ~: "x { y 0 }" ?=> ([Word "x", nosub " y 0 "], "")
     ,"x {y 0}" ~: "x {y 0}" ?=> ([Word "x", nosub "y 0"], "")
     ,"{y}{z}" ?=> ([nosub "y"], "{z}")
   ]
 where (?=>) str (res,r) = Right (res, r) ~=? runParser2 parseTokens str
       nosub = NoSub

parseSubstTests = "parseSubst" ~: TestList [
    (all_on, "A cat") `should_be` [SStr "A cat"]
    ,(all_on, "A $cat") `should_be` [SStr "A ", SVar "cat"]
    ,(no_var, "A $cat") `should_be` [SStr "A $cat"]
    ,(all_on, "A \\cat") `should_be` [SStr "A cat"]
    ,(no_esc, "A \\cat") `should_be` [SStr "A \\cat"]
    ,(all_on, "\\[fish]") `should_be` [SStr "[fish]"]
    ,(all_on, "[fish face") `should_be` [SStr "[fish face"]
    ,(no_esc, "One \\n Two") `should_be` [SStr "One \\n Two"]
    ,(no_esc, " \\$fish ") `should_be` [SStr " \\", SVar "fish", SStr " "]
  ]
 where should_be (opt, str) res = (B.unpack str) ~: Right (res,"") ~=? runParser1 (parseSubst opt) str
       ma (v,e,c) = SubstArgs v e c
       all_on = ma (True,True,True)
       no_esc = ma (True,False,True)
       no_var = ma (False,True,True)