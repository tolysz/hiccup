{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module BSParse where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Internal (w2c)
import Control.Monad (mplus)
import Data.Ix
import Util
import Test.HUnit
import Text.Parsec.ByteString
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim ((<|>))
-- import qualified Text.Parsec.Token as P
-- import Text.Parsec.Language (haskellDef)
-- lexer       = P.makeTokenParser haskellDef

-- type Parser a = ParserT BString a
-- type Parser a = BString -> Result a
-- type ParseMonad = Either String
-- type Result a = ParseMonad (a, BString)

eatSpaces :: Parser ()
eatSpaces = spaces
-- takeWreturn ((), dropSpaces s)
{-# INLINE eatSpaces #-}

emit :: a -> Parser a
emit = return
{-# INLINE emit #-}

-- consumed :: Parser t -> Parser BString
-- consumed p = do
--     (_,r) <- lookAhead p
--     let lendiff = B.length s - B.length r
--     return (B.take lendiff s, r)
-- {-# INLINE consumed #-}


-- (</>) :: Parser t -> Parser t -> Parser t
-- a </> b = (a v) `mplus` (b v))
-- {-# INLINE (</>) #-}

tryGet fn = fn <|> return ""
     
-- chain_ lst = consumed (foldr (.>>) (emit ()) lst)

-- chain :: [Parser BString] -> Parser BString
-- chain lst = (foldr pcons (emit []) lst) `wrapWith` B.concat
-- chain =
{-# INLINE chain #-}

choose = foldr1 (<|>)
{-# INLINE choose #-}

pjoin :: (t1 -> t2 -> t3) -> Parser t1 -> Parser t2 -> Parser t3
pjoin op a b = op <$> a <*> b
--  do
--        (w,r)   <- a s
--        (w2,r2) <- b r
--        return ((op w w2), r2)
{-# INLINE pjoin #-}

pass = pjoin (\a _ -> a)
{-# INLINE pass #-}

(.>>) = pjoin (\_ b -> b)
{-# INLINE (.>>) #-}

pcons :: Parser t -> Parser [t] -> Parser [t]
pcons = pjoin (:)
{-# INLINE pcons #-}

parseMany :: Parser t -> Parser [t]
parseMany p = inner <|> emit []
 where inner = parseMany1 p
   
parseMany1 p = p `pcons` (parseMany p)
{-# INLINE parseMany1 #-}

parseLit :: BString -> Parser BString
parseLit w = string (B.unpack w) >> return w

-- if w `B.isPrefixOf` s
--                     then return (w, B.unsafeDrop (B.length w) s)
--                     else fail $ "didn't match " ++ show w
{-# INLINE parseLit #-}


parseLen :: Int -> Parser BString
parseLen !i = B.pack <$> count i anyChar
-- \s -> if B.length s < i
--                      then fail ("can't parse " ++ show i ++ ", not enough")
--                      else return $ B.splitAt i s
{-# INLINE parseLen #-}

parseOneOf !cl = parseCharPred (`elem` cl) ("one of " ++ show cl)

parseNoneOf cl desc = getPred1 (`B.notElem` cl) desc

pchar :: Char -> Parser Char
pchar = char -- parseCharPred (== c) (show c)
{-# INLINE pchar #-}

parseAny = anyChar
-- parseCharPred (const True) "any char"
parseCharPred pred desc s = case B.uncons s of
                            Nothing    -> failStr "eof"
                            Just (h,t) -> if pred h then return $! (B.singleton h,t)
                                                    else failStr (show h)
 where failStr what = fail $ "expected " ++ desc ++ ", got " ++ what
{-# INLINE parseCharPred #-}


wrapWith fn wr s = fn s >>= \(!w,r) -> return (wr w, r) 
{-# INLINE wrapWith #-}

getPred p s = return $! (w,n)
 where (w,n) = B.span p s

-- TODO: slightly inaccuate error messages
getPred1 pred desc s = if B.null w then fail ("wanted " ++ desc ++ ", got eof") else return $! (w,n)
 where (w,n) = B.span pred s
{-# INLINE getPred1 #-}


parseEof :: Parser ()
parseEof = eof
-- if B.null s
--                then return ((), s)
--                else fail $ "expected eof, got " ++ show (B.take 20 s)

-- sepBy1 :: Parser t -> Parser t2 -> Parser [t]
-- sepBy1 p sep = p `pcons` (parseMany (sep .>> p))

-- sepBy p sep = (p `sepBy1` sep) </> emit []

commaSep :: Parser t -> Parser [t]
commaSep = (`sepBy` (eatSpaces .>> pchar ','))

checkStartsWith :: Char -> Parser ()
checkStartsWith !c = do
  h <- lookAhead anyChar
--   h <- safeHead (show c) s
  if h == c then return () -- emit () s
            else fail $ "expected " ++ show c ++ ", got " ++ show h
{-# INLINE checkStartsWith #-}
  
-- safeHead r s = if B.null s then fail ("expected " ++ r ++ ", got eof") else return (w2c . B.unsafeHead $ s)
-- {-# INLINE safeHead #-}

-- between l r p = (l .>> p) `pass` r
-- {-# INLINE between #-}

brackets = between (pchar '[') (eatSpaces .>> pchar ']')
quotes = between (pchar '"') (pchar '"')

wordChar !c = c /= ' ' && (inRange ('a','z') c || inRange ('A','Z') c || inRange ('0','9') c || c == '_')

braceVar = between (pchar '{') (pchar '}') inner
 where inner = many $ choose [nobraces, escapedChar]
       nobraces = noneOf "}\\" -- "not } or \\"

parseStr :: Parser BString
parseStr = B.pack <$> quotes inside
 where noquotes = noneOf "\"\\"
--   "non-quote chars"
       inside = many (noquotes <|> escapedChar)

parseDecInt :: Parser Int 
parseDecInt = read <$> many1 digit
-- P.integer lexer
-- case B.readInt s of
--                  Just x -> return x
--                  Nothing -> fail "expected int"
{-# INLINE parseDecInt #-}

escapedChar :: Parser Char
escapedChar = char '\\' >> parseAny

parseBlock :: Parser BString
parseBlock = B.pack <$> between (pchar '{') (pchar '}') nest_filling
 where inner = choose [(:[]) <$> nobraces, (:[]) <$>  escapedChar, braces]
       nest_filling :: Parser String
       nest_filling = concat <$> parseMany inner
       braces = concat <$> chain [(:[]) <$> pchar '{', nest_filling, (:[]) <$> pchar '}']
       nobraces = noneOf "{}\\" -- <?> "non-brace chars"
{-# INLINE parseBlock #-}

chain :: [Parser a] -> Parser [a]
chain = sequence


-- # TESTS # --

should_fail_ act _ = let res = case act of 
                                 Left _ -> True
                                 _      -> False
                     in TestCase $ assertBool "should fail" res
{--
parseStrTests = "parseStr" ~: TestList [
      "Escaped works" ~: ("Oh \\\"yeah\\\" baby.", "") ?=? "\"Oh \\\"yeah\\\" baby.\"",
      "Parse Str with leftover" ~: ("Hey there.", " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: ("How about \\$44?", "") ?=? "\"How about \\$44?\"",
      "bad parse1" ~: "What's new?" `should_fail` ()
   ]
 where (?=?) res str = Right res ~=? parseStr str
       should_fail str _  = (parseStr str) `should_fail_` ()

braceVarTests = TestList [
      "Simple" ~: ("data", "") ?=? "{data}",
      "With spaces" ~: (" a b c d ",  " ") ?=? "{ a b c d } ",
      "With esc" ~: (" \\} yeah! ", " ") ?=? "{ \\} yeah! } ",
      "bad parse" ~: "{ oh no" `should_fail` (),
      "bad parse" ~: "pancake" `should_fail` ()
   ]
 where (?=?) res str = Right res ~=? braceVar str
       should_fail str _  = (braceVar str) `should_fail_` ()


blockTests = "code block" ~: TestList [
  "Fail nested" ~: "  {       the end" `should_fail` (),
  "Pass nested" ~: "{  { }}" `should_be` "  { }",
  "Pass empty nested" ~: "{ }" `should_be` " ",
  "Fail nested" ~: "  { {  }" `should_fail` (),
  "Pass escape" ~: "{ \\{ }" `should_be` " \\{ ",
  "Pass escape 2" ~: "{ \\{ \\{ }" `should_be` " \\{ \\{ ",
  "Pass escape 3" ~: "{ \\\\}" `should_be` " \\\\",
  "Pass escape 4" ~: "{ \\} }" `should_be` " \\} "
  ,"no braces" ~: "happy" `should_fail` ()
 ]
 where should_be act exp = Right (exp, B.empty) ~=? parseBlock act
       should_fail act _ = (parseBlock act) `should_fail_` ()


bsParseTests = TestList [ blockTests, braceVarTests, parseStrTests ]

-- # ENDTESTS # --
-}
