module Calculus ( --integrate,
                  --derivative,
                  --nth_derivative,
                  --arclength,
                  arclength_inv
                ) where

import Data.Ratio

δx :: Double
δx = 0.00001

-- Gets the nth row of triangle numbers
triangle_gen :: Int -> [Integer]
triangle_gen 0 = [1]
triangle_gen n = map kth $ [0..n]
    where
        last = triangle_gen (n - 1)
        kth 0 = 1
        kth k | k == n    = 1
              | otherwise =  x + y
            where
                (x:y:_) = drop (k - 1) last

-- Gets the kth element of the nth triangle number row
triangle :: Int -> Int -> Integer
triangle n k = head $ drop k $ triangle_gen n

-- Integrates a function `f' from a to b
integrate :: Double -> (Double -> Double) -> Double -> Double
integrate a f b = fix_sign * δx * (sum $ map f x_vals)
    where
        (a', b') = (min a b, max a b)
        fix_sign | a <= b    =  1
                 | otherwise = -1
        x_vals = take (floor ((b' - a')/δx)) $ scanl (+) a' (repeat δx)

-- Returns the derivative of f at point x.
derivative :: (Double -> Double) -> Double -> Double
derivative = nth_derivative 1

-- Returns a function representing the nth derivative of f.
nth_derivative :: Int -> (Double -> Double) -> Double -> Double
nth_derivative n f x = fromRational $ sum $ map g [0..n]
    where
        g k = toRational(sign k * triag n k) * (func k x) * delta
        sign k    = if (k `mod` 2) == 0 then 1 else -1
        triag n k = triangle n k
        func k x  = (toRational . f)(x + (fromIntegral $ n - k)*δx)
        delta     = 1 / (toRational δx)^n

-- Returns the arclength of f(x) between a and b. (where the last parameter `b'
-- is curried).
arclength :: Double -> (Double -> Double) -> (Double -> Double)
arclength a f = integrate a (\x -> sqrt $ (derivative f x)^2 + 1)

close :: Double -> Double -> Double -> Bool
close x y delta = (low + delta) > high
    where
        low  = min x y
        high = max x y

-- Returns the initial domain of the function.
find_domain :: Double -> Double -> (Double -> Double) -> Double -> (Double, Double)
find_domain start last f target | close target (f last) (10*δx) = (last, last) -- If we found it (fluke).
                                | target < (f last)             = (start, last)
                                | otherwise                     = find_domain last  (last + 2*(last - start))  f target
--                                                                                ^ Doubles the search space  ^

-- Does a binary search on the interval [low, high] for a value 'x' such that
-- f(x) = target +/- 10δ
bsearch :: Double -> Double -> Double -> (Double -> Double) -> Double
bsearch low high target f | close y target (10*δx) = mid
                          | y < target            = bsearch mid high target f
                          | otherwise             = bsearch low mid  target f
    where
        mid = (low + high) / 2
        y   = f mid

-- Returns the x value such that the arclength of f from a to x will be equal to t.
arclength_inv :: Double -> (Double -> Double) -> Double -> Double
arclength_inv a f t = bsearch x y t func
    where
        (x, y) = find_domain a (a + 1) func t
        func   = arclength a f
