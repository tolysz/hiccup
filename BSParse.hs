{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module BSParse
 ( consumed
 , tryGet
 , getPred
 , getPred1
 , wrapWith
 , parseLit
 , parseMany
 , parseMany1
 , parseOneOf
 , parseNoneOf
 , parseStr
 , parseLen
 , parseBlock
 , parseCharPred
 , chain
 , choose
 , pass
 , sepBut
 , sepBut1
 , parseEof
 , safeHead
 , eatSpaces
 , quotes
 , brackets
 , commaSep
 , pjoin
 , braceVar
 , checkStartsWith
 , parseAny
 , wordChar
 , bchar
 , pchar
 , parseDecInt
 , escapedChar
 , escapedCharBString
 , should_fail_
 , runParser1
 , runParser2
 , bsParseTests
 , reminder
 , module Text.Parsec
 , module Text.Parsec.ByteString
)where

import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Unsafe as B
-- import Data.ByteString.Internal (w2c)
import Control.Monad
-- (mplus)
import Data.Ix
import Util
import Test.HUnit
import Text.Parsec
import Text.Parsec.ByteString
-- import Text.ParserCombinators.Parsec.Char
-- import Text.ParserCombinators.Parsec.Combinator
-- import Text.ParserCombinators.Parsec.Prim ((<|>))
-- import Control.Monad
-- import qualified Text.Parsec.Token as P
-- import Text.Parsec.Language (haskellDef)
-- lexer       = P.makeTokenParser haskellDef

-- type Parser a = ParserT BString a
-- type Parser a = BString -> Result a
-- type ParseMonad = Either String
-- type Result a = ParseMonad (a, BString)

eatSpaces :: Parser ()
eatSpaces = void (many1 (parseOneOf " \t")) <|> return ()
-- takeWreturn ((), dropSpaces s)
{-# INLINE eatSpaces #-}

-- emit :: a -> Parser a
-- emit = return
-- {-# INLINE emit #-}

consumed :: Parser t -> Parser BString
consumed p = do
     s <- getInput
     try p <|> fail "consume failed"
     r <- getInput
     let lendiff = B.length s - B.length r
     return $ B.take lendiff s

--     let lendiff = B.length s - B.length r
--     return (B.take lendiff s, r)
{-# INLINE consumed #-}


-- (</>) :: Parser t -> Parser t -> Parser t
-- a </> b = (a v) `mplus` (b v))
-- {-# INLINE (</>) #-}

tryGet fn = try fn <|> return ""

-- chain_ lst = consumed (sequence_ lst)

-- chain :: [Parser BString] -> Parser BString
-- chain lst = (foldr pcons (emit []) lst) `wrapWith` B.concat

chain :: Monoid a =>  [Parser a] -> Parser a
chain = fmap mconcat . sequence
-- chain =
{-# INLINE chain #-}

choose = choice
-- {-# INLINE choose #-}

pjoin :: (t1 -> t2 -> t3) -> Parser t1 -> Parser t2 -> Parser t3
pjoin op a b = op <$> a <*> b
--  do
--        (w,r)   <- a s
--        (w2,r2) <- b r
--        return ((op w w2), r2)
{-# INLINE pjoin #-}

pass = pjoin const
{-# INLINE pass #-}

-- (.>>) = (>>)
--  pjoin (\_ b -> b)
-- {-# INLINE (.>>) #-}

-- pcons :: Parser t -> Parser [t] -> Parser [t]
-- pcons = pjoin (:)
-- {-# INLINE pcons #-}

parseMany :: Parser t -> Parser [t]
parseMany = many
-- try inner <|> emit []
--  where inner = parseMany1 p

parseMany1 :: Parser t -> Parser [t]
parseMany1 = many1
-- p `pcons` (parseMany p)
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

parseOneOf :: String -> Parser Char
parseOneOf = oneOf
-- parseCharPred (`elem` cl) ("one of " ++ show cl)

parseNoneOf :: String -> String -> Parser Char
parseNoneOf cl desc = noneOf cl <?> desc
--  parseCharPred (`B.notElem` cl) desc

pchar :: Char -> Parser Char
pchar = char -- parseCharPred (== c) (show c)
{-# INLINE pchar #-}

bchar c = B.singleton <$> pchar c

parseAny = anyChar
-- parseCharPred (const True) "any char"
parseCharPred :: (Char -> Bool) -> String -> Parser Char
parseCharPred pred desc= satisfy pred <?> "expected " ++ desc ++ ", got"
--   s <- getInput
--   (do
--     c <- anyChar
--     unless (pred c) $ fail ""
--     return c
--    ) <?> "expected " ++ desc ++ ", got "
--   case B.uncons s of
--      Nothing    -> failStr "eof"
--      Just (h,t) -> if pred h then setInput t >> return h
--                              else failStr (show h)
--  where failStr what = fail $ "expected " ++ desc ++ ", got " ++ what
{-# INLINE parseCharPred #-}


-- wrapWith fn wr s = fn s >>= \(!w,r) -> return (wr w, r)
wrapWith fn wr = wr <$> fn
{-# INLINE wrapWith #-}

getPred :: (Char -> Bool) -> Parser BString
-- getPred p =
-- <$> many1 p
--    s <- getInput
--    let (w,n) = B.span p s
--    setInput $! n
--    return $! w


getPred p = B.pack <$> many1 (satisfy p)
--   return $ B.pack b
-- return $! (w,n)
--  where (w,n) = B.span p s

-- isPred :: (Char -> Bool) -> Parser Char
-- isPred = satisfy
--   try $ do
--     c <- anyChar
--     unless (p c) (fail $ "got: " ++ [c])
--     return c

-- TODO: slightly inaccuate error messages
getPred1 :: (Char -> Bool) -> String -> Parser BString
getPred1 pred desc = B.pack <$> many1 (satisfy pred) <?> "wanted " ++ desc ++ ", but fail"
--   do
--   i <- getInput
--   getPred pred <?> "wanted " ++ desc ++ ", but fail to get it" ++ show i
--   if B.null w then fail ("wanted " ++ desc ++ ", but fail to get it") else return w
--  where (w,n) = B.span pred s
{-# INLINE getPred1 #-}


parseEof :: Parser ()
parseEof = eof
-- if B.null s
--                then return ((), s)
--                else fail $ "expected eof, got " ++ show (B.take 20 s)

-- sepBy1 :: Parser t -> Parser t2 -> Parser [t]
-- sepBy1 p sep = p `pcons` (parseMany (sep .>> p))

-- sepBy p sep = (p `sepBy1` sep) </> emit []

sepBut1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBut1 p sep     = do { x <- p
                       ; try (do{ sep
                            ; xs <- sepBut1 p sep
                            ; return (x:xs)
                            })
                          <|> return [x]
                        }

sepBut :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBut p sep  = sepBut1 p sep <|> return []


commaSep :: Parser t -> Parser [t]
commaSep = (`sepBy` (eatSpaces >> pchar ','))

checkStartsWith :: Char -> Parser ()
checkStartsWith !c = do
  h <- lookAhead anyChar
--   h <- safeHead (show c) s
  unless (h == c) $ fail $ "expected " ++ show c ++ ", got " ++ show h
{-# INLINE checkStartsWith #-}

safeHead :: String -> Parser Char
safeHead r = lookAhead anyChar <?> ("expected " ++ r ++ ", got eof")
--   if B.null s then fail ("expected " ++ r ++ ", got eof") else return (w2c . B.unsafeHead $ s)
{-# INLINE safeHead #-}

-- between l r p = (l .>> p) `pass` r
-- {-# INLINE between #-}

brackets = between (pchar '[') (try (eatSpaces >> pchar ']'))
quotes   = between (pchar '"') (try (pchar '"'))

wordChar :: Char -> Bool
wordChar !c = c /= ' ' && (inRange ('a','z') c || inRange ('A','Z') c || inRange ('0','9') c || c == '_')

braceVar :: Parser BString
braceVar = B.pack <$> between (pchar '{') (try (pchar '}')) (concat <$> inner)
 where inner = many $ choose [(:[]) <$> nobraces, escapedChar]
       nobraces = noneOf "}\\" -- "not } or \\"

parseStr :: Parser BString
parseStr = B.pack <$> quotes ( concat <$> inside)
 where noquotes = noneOf "\"\\"
--   "non-quote chars"
       inside = many ((:[]) <$> try noquotes <|> escapedChar)

parseDecInt :: Parser Int 
parseDecInt = read <$> many1 digit
-- P.integer lexer
-- case B.readInt s of
--                  Just x -> return x
--                  Nothing -> fail "expected int"
{-# INLINE parseDecInt #-}
reminder :: Parser BString
reminder = B.pack <$> many anyChar

escapedChar :: Parser [Char]
escapedChar = do
  char '\\'
  c <- parseAny
  return ['\\', c]

escapedCharBString :: Parser BString
escapedCharBString = B.pack <$> escapedChar


parseBlock :: Parser BString
parseBlock = B.pack <$> between (pchar '{') (pchar '}') nest_filling
 where inner = choose [(:[]) <$> nobraces, escapedChar, braces]

       nest_filling :: Parser String
       nest_filling = concat <$> parseMany inner

       braces = chain [(:[]) <$> pchar '{', nest_filling, (:[]) <$> pchar '}']
       nobraces = noneOf "{}\\" -- <?> "non-brace chars"
{-# INLINE parseBlock #-}


-- parseBlock :: Parser BString
-- parseBlock = B.pack <$> between (pchar '{') (pchar '}') nest_filling
--  where inner = choose [(:[]) <$> nobraces, (:[]) <$>  escapedChar, braces]
--        nest_filling :: Parser String
--        nest_filling = concat <$> parseMany inner
--        braces = concat <$> chain [(:[]) <$> pchar '{', nest_filling, (:[]) <$> pchar '}']
--        nobraces = noneOf "{}\\" -- <?> "non-brace chars"
-- {-# INLINE parseBlock #-}
-- # TESTS # --

should_fail_ act _ = let res = case act of
                                 Left _       -> True
                                 Right (_,"") -> False
                                 _            -> True
                     in TestCase $ assertBool ("should fail " ++ show act) res

runParser1 :: Parser a -> BString -> Either ParseError (a, BString)
runParser1 p  = runParser ww () ""
  where
    ww = do
     pr <- p
--      i <- reminder
     i <- getInput
     return (pr,i)

runParser2 :: Parser a -> BString -> Either ParseError (a, BString)
runParser2 p  = runParser ww () ""
  where
    ww = (,) <$> p <*>reminder
--      i <- getInput
--      return (pr,i)

parseStrTests = "parseStr" ~: TestList [
      "Escaped works" ~: ("Oh \\\"yeah\\\" baby.", "") ?=? "\"Oh \\\"yeah\\\" baby.\"",
      "Parse Str with leftover" ~: ("Hey there.", " 44") ?=? "\"Hey there.\" 44",
      "Parse Str with dolla" ~: ("How about \\$44?", "") ?=? "\"How about \\$44?\"",
      "bad parse1" ~: "What's new?" `should_fail` ()
   ]
 where (?=?) res str = Right res ~=? runParser1 parseStr str
       should_fail str _  = (runParser1 parseStr str) `should_fail_` ()

braceVarTests = TestList [
      "Simple" ~: ("data", "") ?=? "{data}",
      "With spaces" ~: (" a b c d ",  " ") ?=? "{ a b c d } ",
      "With esc" ~: (" \\} yeah! ", " ") ?=? "{ \\} yeah! } ",
      "bad parse" ~: "{ oh no" `should_fail` (),
      "bad parse" ~: "pancake" `should_fail` ()
   ]
 where (?=?) res str = Right res ~=? runParser1 braceVar str
       should_fail str _  = (runParser1 braceVar str) `should_fail_` ()


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
 where should_be act exp = Right (exp, B.empty) ~=? runParser1 parseBlock act
       should_fail act _ = (runParser1 parseBlock act) `should_fail_` ()


bsParseTests = TestList [ blockTests, braceVarTests, parseStrTests ]

-- # ENDTESTS # --

