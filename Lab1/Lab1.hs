{- Lab 1
   Date: 2023-11-01
   Authors: Karl Kjellmer, Albin Skeppstedt
   Lab group: 5
 -}
import Test.QuickCheck

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
power2 n k | even k    = power2 (n*n) (div k 2)
           | otherwise = n * power2 n (k-1)

-- D -------------------------
{- 
<Describe your test cases here>

5^1 - Testing the identity
2^0 - Testing the special case ^0
2^2 - Testig a generic case 
This tests "all" interesting cases of non negative numbers
 -}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = and [(pow == power1 n k), (pow == power2 n k)]
   where pow = power n k 


--
powerTest :: Bool
powerTest = and [prop_powers (fst x) (snd x) | x <- tests]
   where tests = [(5, 1), (2, 0), (2, 2)]

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = and [(pow == power1 n' k'), (pow == power2 n' k')]
   where n' = abs n
         k' = abs k
         pow = power n' k'