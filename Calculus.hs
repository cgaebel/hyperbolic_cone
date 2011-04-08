module Calculus ( integrate,
                  derivative,
                  arclength,
                  arclength_inv
                ) where
δx :: Double
δx = 0.00001

-- a slightly larger delta, for when we need it ;)
dx :: Double
dx = 0.01

-- Integrates a function `f' from a to b
integrate :: Double -> (Double -> Double) -> Double -> Double
integrate a f b = fix_sign * (sum $ map f x_vals) * δx
    where
        (a', b') = (min a b, max a b)
        fix_sign | a <= b    =  1
                 | otherwise = -1
        x_vals = takeWhile (<= b') $ scanl (+) a' (repeat δx)

-- Returns the derivative of f at point x.
derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f(x + δx) - f(x)) / δx

-- Returns the arclength of f(x) between a and b. (where the last parameter `b'
-- is curried).
arclength :: Double -> (Double -> Double) -> (Double -> Double)
arclength a f = integrate a (\x -> sqrt $ (derivative f x)**2 + 1)

-- No need to have a termination condition, the list MUST be infinite.
-- Approximates a value of a strictly increasing, continuous function,
-- where the inputs and outputs to the function are in the infinite list.
-- When the outputs get to `target_val', it returns the matching x-value.
approximate :: [(Double, Double)] -> Double -> Double
approximate [] _       = error "approximate needs to be passed an infinite list to work."
approximate [(_, _)] _ = approximate [] 1.0
approximate ((x1, y1):(x2, y2):xs) target_val =
    if y1 < target_val && target_val < y2 then
        (x1 + x2) / 2 -- y1 < target_val < y2, therefore, average them.
    else
        approximate ((x2, y2):xs) target_val

-- Returns a value of 'b' such that arclength a b f ~ el
-- Note: This is insanely slow. Should I be better-defining the search space and
-- binary searching?
arclength_inv :: Double -> (Double -> Double) -> Double -> Double
arclength_inv a f el = approximate candidates el
    where
        inputs     = [a, (a + dx) ..]
        outputs    = map (arclength a f) inputs
        candidates = zip inputs outputs
