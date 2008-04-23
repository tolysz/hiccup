{-# LANGUAGE BangPatterns #-}
module MathOp(
        plus, 
        minus,
        times,
        divide,
        equals,
        notEquals,
        lessThan,
        lessThanEq,
        greaterThan,
        greaterThanEq, pow, squarert, absfun) where

import qualified TObj as T

data NPair = Ints !Int !Int | Doubles !Double !Double deriving (Eq,Show)

-- TODO: Inline getNumerics manually and get rid of NPair?
getNumerics :: (T.ITObj t) => t -> t -> Maybe NPair
getNumerics !x !y =
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! Ints i1 i2
       _ -> case (T.asDouble x, T.asDouble y) of
               (Just d1, Just d2) -> return $! Doubles d1 d2
               _ -> fail $ "expected numeric"
{-# INLINE getNumerics #-}

numop name iop dop !x !y = 
   case getNumerics x y of
       Just (Ints i1 i2)  -> return $! (T.fromInt (i1 `iop` i2))
       Just (Doubles d1 d2) -> return $! T.fromDouble (d1 `dop` d2)
       _ -> fail $ "can't use non-numeric string as operand of " ++ show name
{-# INLINE numop #-}

plus, minus, times, divide :: (Monad m, T.ITObj t) => t -> t -> m t
plus = numop "+" (+) (+) 
minus = numop "-" (-) (-)
times = numop "*" (*) (*)
divide = numop "/" div (/)

squarert x = do
    case T.asInt x of
      Just i -> return $! T.fromDouble (sqrt (fromIntegral i))
      Nothing -> do
        d1 <- T.asDouble x
	return $! T.fromDouble (sqrt d1)

absfun x = case T.asInt x of
            Nothing -> do d <- T.asDouble x
                          return (T.fromDouble (abs d))
            Just i  -> return (T.fromInt (abs i))

pow x y = do
   case (T.asInt x, T.asInt y) of
       (Just i1, Just i2) -> return $! (T.fromInt (i1^i2))
       _ -> do 
           d1 <- T.asDouble x
           d2 <- T.asDouble y
	   return $! T.fromDouble (d1 ** d2)


lessThan a b = T.fromBool $! (tclCompare a b == LT)

lessThanEq a b = T.fromBool $! (tclCompare a b /= GT)

greaterThan a b = T.fromBool $! (tclCompare a b == GT)

greaterThanEq a b = T.fromBool $! (tclCompare a b  /= LT)

equals a b = T.fromBool $! (tclCompare a b == EQ)

notEquals a b = T.fromBool $! (tclCompare a b /= EQ)

tclCompare a b =
  case (T.asInt a, T.asInt b) of
     (Just i1, Just i2) -> compare i1 i2
     _  -> case (T.asDouble a, T.asDouble b) of
                  (Just d1, Just d2) -> compare d1 d2
		  _ -> compare (T.asBStr a) (T.asBStr b)
{-# INLINE tclCompare #-}