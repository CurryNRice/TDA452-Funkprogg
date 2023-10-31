{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer

stepsPower n k = k+1 


-- B -------------------------
power1 :: Integer -> Integer -> Integer

power1 n k = product(replicate (fromInteger k) n)

-- C -------------------------
power2 :: Integer -> Integer -> Integer

power2 n 0 = 1

power2 n k = if even k then power2 (n*n) (div k 2) else n * (power2 n (k-1))

-- D -------------------------
{- 

<Describe your test cases here>

5^1
2^0
2^2


 -}

-- 
{-
prop_powers :: Integer -> Integer -> Integer
prop_powers n k = if ((power n k == power1 n k) and (power n k == power2 n k)) then 1 else 0
-}
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = and ((power n k == power1 n k), (power n k == power2 n k))

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined