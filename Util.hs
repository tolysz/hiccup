{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module Util where
import qualified Data.ByteString.Char8 as B
import Control.Monad.Error
import Data.List(intersperse)
import Test.HUnit
import Data.Char (toLower)
import Data.String

type BString = B.ByteString

instance IsString B.ByteString where
  fromString = B.pack

joinWithBS bsl bs = B.concat (intersperse bs bsl)
joinWith bsl c = B.concat (intersperse (B.singleton c) bsl)
{-# INLINE joinWith #-}
pack = B.pack
{-# INLINE pack #-}
unpack = B.unpack
bsNull = B.null
{-# INLINE bsNull #-}

dropSpaces = B.dropWhile (\x -> x == ' ' || x == '\t')
{-# INLINE dropSpaces #-}

mapSnd f = map (\(a,b) -> (a, f b))
{-# INLINE mapSnd #-}
mapFst f = map (\(a,b) -> (f a, b))

ifFails f v = f `catchError` (\_ -> return v)
{-# INLINE ifFails #-}

orElse f f2 = f `catchError` (\_ -> f2)

slurpFile fname = do dat <- B.readFile fname 
                     B.length dat `seq` return dat

listEscape s = if (B.elem ' ' s && not hasBracks) || B.null s 
                 then B.concat [B.singleton '{', s, B.singleton '}'] 
                 else if hasBracks then B.concat (escapeStr s) else s
  where hasBracks = bdepth /= 0
        bdepth    = brackDepth s

downCase = B.map toLower

brackDepth s = match 0 0 False
 where match i c esc = if i >= B.length s 
                          then c
                          else let ni = i+1  
                               in if esc then match ni c False 
                                         else case B.index s i of
                                               '{'  -> match ni (c+1) False
                                               '}'  -> match ni (c-1) False
                                               '\\' -> match ni c True
                                               _    -> match ni c False
                                       

escapeStr s = case B.findIndex (`B.elem` " \n\t{}") s of
                 Nothing -> [s]
                 Just i  -> let (b,a) = B.splitAt i s
                            in b : handle (B.head a) : escapeStr (B.drop 1 a)
 where handle '\n' = "\\n"
       handle ' '  = "\\ "
       handle '{'  = "\\{"
       handle '}'  = "\\}"
       handle _    = error "The impossible happened in handle"

commaList :: String -> [String] -> String
commaList _    []  = ""
commaList _    [a] = a
commaList conj lst = (intercalate ", " (init lst)) ++ " " ++ conj ++ " " ++ last lst

intercalate :: String -> [String] -> String
intercalate xs xss = concat (intersperse xs xss)

match :: Bool -> BString -> BString -> Bool
match nocase pat str = inner 0 0
 where slen = B.length str 
       plen = B.length pat
       ceq a b = if nocase then toLower a == toLower b else a == b
       inner pi si  
        | pi == plen = si == slen
        | otherwise = case B.index pat pi of
                       '*'  -> pi == (plen - 1) || or (map (inner (succ pi)) [si..(slen - 1)])
                       '?'  -> not (si == slen) && inner (succ pi) (succ si)
                       '['  -> let (npi, pred) = getSetPred pat pi plen 
                               in (pred (B.index str si)) && inner npi (succ si)
                       '\\' -> inner (succ pi) si
                       v    -> not (si == slen) && v `ceq` (B.index str si) && inner (succ pi) (succ si)

getSetPred pat pi plen = inner pi [] False
 where inner i lst rang = 
        if i == plen 
           then (i, (`elem` lst))
           else case (B.index pat i,rang) of
                 (']',False) -> (succ i, (`elem` lst)) 
                 (']',True) -> (succ i, (`elem` ('-':lst))) 
                 ('-',_) -> case lst of
                            [] -> inner (succ i) ['-'] False
                            _  -> inner (succ i) lst True
                 (c,False) -> inner (succ i) (c:lst) False
                 (c,True) -> let (h:tl) = lst 
                                 (a,z)  = minmax h c
                             in inner (succ i) ([a..z] ++ tl) False

minmax a b = if b < a then (b,a) else (a,b)
globMatch pat = match False pat
exactMatch pat = (== pat)
globMatches pat = filter (globMatch pat)
exactMatches pat = filter (exactMatch pat) 

utilTests = TestList [joinWithTests, listEscapeTests] where
 
  joinWithTests = TestList [
       ([""],'!') `joinsTo` "" 
       ,(["one"],'!') `joinsTo` "one" 
       ,(["one", "two"],',') `joinsTo` "one,two" 
    ]
   where joinsTo (a,s) r = pack r ~=? ((map pack a) `joinWith` s)

  listEscapeTests = TestList [
      noEscape "one" 
      ,noEscape "[andkda]" 
      ,"" `escapesAs` "{}"
      ,"a b" `escapesAs` "{a b}"
      ,"a { b" `escapesAs` "a\\ \\{\\ b"
      ,"a } b" `escapesAs` "a\\ \\}\\ b"
      ,"a {b} c" `escapesAs` "{a {b} c}"
      ," { " `escapesAs` "\\ \\{\\ "
    ] 
   where escapesAs a b = pack b ~=? listEscape (pack a)
         noEscape a = pack a ~=? listEscape (pack a)
