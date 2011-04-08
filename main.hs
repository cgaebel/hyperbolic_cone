module Main where

import Graphics.Gnuplot.Simple
import Calculus

-- The minimum radius of the cone:
α = 1 :: Double

-- The maximum radius of the cone:
β = 10 :: Double

-- Our original function. Given a radius of the cone, returns the distance from
-- the asymptote.
--f  = (** (-2))
f x = x

-- The inverse of f.
--f' = (** (-1/2))
f' x = x

-- The new, translated function.
g  x = α + (f α) - (f x)

-- The inverse of g.
g' x = f' $ α + (f α) - x

-- Now, the radius of the cone at distance 'x' from the center of the net is
-- equal to g'(x). Therefore, the circumference is going to be equal to 2pi*g'(x).
-- The circumerference of the net's circle will be 2pi*x. Therefore, the
-- circumference of cutout will be 2pi(x - g'(x)), OR two cutouts side-by-side
-- of length pi(x-g'(x)).
cutout_length r = pi * (r - (g' r))

-- the angle of a cutout, measured from the x-axis.
theta r = (cutout_length r) / r

-- Gets the positive and negative points on a circle of radius r.
_2points r = [point r, (neg2nd $ point r)]
  where
    neg2nd (x, y) = (x, -y)
    -- The point on the circle of radius r.
    point r = (r * (cos . theta)(r), r * (sin . theta)(r))

-- Our sample points.
distances = [α, (α + 0.001) .. β]

makepoints = foldr1 (++) $ map _2points distances

--main = plotDots [XRange (α, β), YRange (-β, β)] makepoints
main = print $ arclength_inv 1 (**(-2)) 9.30918
