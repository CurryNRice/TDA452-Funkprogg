-- 1:

-- [x | x <- xs, condition]

filter' f xs = [x | x <- xs, f x]

-- 3:
count' f xs = length [() | x <- xs, f x]

-- 4:
iter 0 f x = x
iter n f x = iter (n-1) f (f x)

-- 5:
pow n x = iter (n-1) (*x) x

-- 6: 
iter' n f x = a
    where [a] = drop n (take (n+1) (iterate f x))