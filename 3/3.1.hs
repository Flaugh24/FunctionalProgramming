data WeirdPeanoNumber = Zero 
                      | Succ (WeirdPeanoNumber) 
                      | Pred (WeirdPeanoNumber)

numToInt :: WeirdPeanoNumber -> Integer
numToInt Zero = 0
numToInt (Succ a) = 1 + (numToInt a)
numToInt (Pred a) = (numToInt a) - 1

intToNum :: Integer -> WeirdPeanoNumber
intToNum x | x > 0 = Succ $ intToNum $ x - 1
           | x < 0 = Pred $ intToNum $ x + 1
           | otherwise = Zero

reduce Zero = Zero
reduce (Succ (Pred a)) = reduce a
reduce (Pred (Succ a)) = reduce a
reduce (Succ a) = let rdcd = reduce a in 
                  case rdcd of (Pred b) -> b 
                               _        -> Succ $ rdcd
reduce (Pred a) = let rdcd = reduce a in
                  case rdcd of (Succ b) -> b
                               _ -> Pred $ rdcd

instance Eq WeirdPeanoNumber where
    (==) a b = rdcdEq (reduce a) (reduce b)
	where
    		rdcdEq Zero Zero = True
    		rdcdEq Zero _    = False
    		rdcdEq _ Zero    = False
   		rdcdEq (Pred a) (Pred b) = rdcdEq a b
    		rdcdEq (Succ a) (Succ b) = rdcdEq a b
    		rdcdEq _ _               = False

instance Ord WeirdPeanoNumber where
    (<=) a b = rcdcOrd (reduce a) (reduce b)
	where
		rcdcOrd (Pred a) (Pred b) = rcdcOrd a b
		rcdcOrd (Succ a) (Succ b) = rcdcOrd a b
		rcdcOrd Zero a  = case a of
                                    Pred _ -> False
                                    _      -> True
		rcdcOrd a Zero  = case a of
                                    Succ _ -> False
                                    _      -> True

instance Show WeirdPeanoNumber where
    show a = show $ numToInt a

instance Num WeirdPeanoNumber where
    (+) Zero a = a
    (+) a Zero = a
    (+) (Pred a) b = (+) a (Pred b)
    (+) (Succ a) b = (+) a (Succ b)

    signum a = case reduce a of 
                      Succ _ -> Succ Zero
                      Pred _ -> Pred Zero
                      _      -> Zero

    negate Zero = Zero
    negate (Pred a) = Succ $ negate a
    negate (Succ a) = Pred $ negate a
	
    abs a = if signum a < Zero then negate a else a

    (*) a b  = rcdcMult (reduce a) (reduce b)
		where
			rcdcMult Zero _ = Zero
                        rcdcMult _ Zero = Zero
			rcdcMult (Succ a) (Succ b) = reduce $ Succ $ a * b + a + b
                        rcdcMult (Pred a) (Pred b) = reduce $ Succ $ a * b - a - b
                        rcdcMult (Succ a) (Pred b) = reduce $ Pred $ a * b - a + b
                        rcdcMult (Pred a) (Succ b) = reduce $ Pred $ a * b + a - b

    fromInteger = intToNum

instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral
    fromEnum = fromInteger.numToInt

instance Real WeirdPeanoNumber where
    toRational x = toRational $ numToInt x


instance Integral WeirdPeanoNumber where
    toInteger = numToInt
    quotRem a b | signum a == signum b = divCnt (abs b) (Zero, (abs a)) 
                | otherwise  = (\(a,b) -> (reduce $ negate a, reduce b)) (divCnt (abs b) (Zero, abs a))
                   where divCnt b  res@(quot, rem) | rem >= b = divCnt b (quot + 1, rem - b)
                                                   | otherwise = res
            


test = show $ (intToNum 5) + fst ( quotRem (intToNum (-235)) (intToNum 4) )
