module RToken (Cmd, toCmd, RToken(..), rtokenTests ) where
import qualified Data.ByteString.Char8 as B
import BSParse (TclWord(..), wrapInterp, runParse, parseArrRef, BString)
import Test.HUnit

type Cmd = (RToken, [RToken])
data RToken = Lit !BString | CatLst [RToken] | CmdTok Cmd 
              | VarRef !BString | ArrRef !BString RToken 
              | Block !BString (Either String [Cmd]) deriving (Eq,Show)

isEmpty (Lit x)    = B.null x
isEmpty (CatLst l) = null l
isEmpty _          = False

compile :: BString -> RToken
compile str = case wrapInterp str of
                   Left s  -> Lit s
                   Right x -> handle x
 where f (Left match) = case parseArrRef match of 
                         Nothing      -> VarRef match
                         Just (n,ind) -> ArrRef n (compile ind)
       f (Right x)    = compCmd x
       handle (b,m,a) = let front = [Lit b, f m]
                        in let lst = (filter (not . isEmpty) (front ++ [compile a]))
                           in case lst of 
                                [a] -> a
                                _   -> CatLst lst

compToken :: TclWord -> RToken
compToken (Word s)               = compile s
compToken (NoSub s res)          = Block s (fromParsed res)
compToken (Subcommand _ c)       = compCmd c

compCmd c = CmdTok (toCmd c)

fromParsed Nothing       = Left "parse failed"
fromParsed (Just (tl,v)) = if B.null v then Right (map toCmd tl) else Left "incomplete parse"


toCmd (x,xs) = (compToken x, map compToken xs)

rtokenTests = TestList [compTests, compTokenTests] where
  compTests = TestList [ 
      "x -> x" ~: "x" `compiles_to` (lit "x")  
      ,"$x -> VarRef x" ~: "$x" `compiles_to` (varref "x")  
      ,"x(1) -> ArrRef x 1" ~: "$x(1)" `compiles_to` (arrref "x" (lit "1"))  
      ,"CatLst" ~: "$x$y" `compiles_to` (CatLst [varref "x", varref "y"])  
      ,"lit" ~: "incr x -1" `compiles_to` lit "incr x -1"
      ,"cmd" ~: "[double 4]" `compiles_to` CmdTok (lit "double", [lit "4"])
    ]

  compTokenTests = TestList [ 
      "1" ~: (mkwd "x")  `tok_to` (lit "x")  
      ,"2" ~: (mknosub "puts 4") `tok_to` (block "puts 4" [(lit "puts", [lit "4"])])
    ]
  
  block s v = Block (B.pack s) (Right v)
  mknosub s = NoSub (B.pack s) (runParse (B.pack s))
  mkwd = Word . B.pack
  lit = Lit . B.pack 
  varref = VarRef . B.pack 
  arrref s t = ArrRef (B.pack s) t
  tok_to a b = do let r = compToken a
                  assertEqual (show a ++ " compiles to " ++ show b) b r
  compiles_to a b = do let r = compile (B.pack a)
                       assertEqual (show a ++ " compiles to " ++ show b) b r