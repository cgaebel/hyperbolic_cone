module Main where

import Calculus

import Graphics.Gnuplot.Simple

-- The minimum radius of the cone:
α :: Double
α = 1

-- The maximum radius of the cone:
β :: Double
β = 10

-- Our original function. Given a radius of the cone, returns the distance from
-- the asymptote.
f :: (Double -> Double)
f = (** (-2))

-- The new, translated function.
g :: Double -> Double
g x = α + (f α) - (f x)

-- Two cutouts, therefore, we halve the actual length.
cutout_length :: Double -> Double
cutout_length r = (2*pi*r - r_cone) / 2
    where
        r_cone = arclength_inv α g (r - α)

-- the angle of a cutout, measured from the x-axis.
theta :: Double -> Double
theta r = (cutout_length r) / r

-- Gets the positive and negative points on a circle of radius r.
_2points :: Double -> [(Double, Double)]
_2points r = [point r, (neg2nd $ point r)]
    where
        neg2nd (x, y) = (x, -y)
        -- The point on the circle of radius r.
        point r = (r * (cos . theta)(r), r * (sin . theta)(r))

-- Our sample points.
distances :: [Double]
distances = [α, (α + 0.1) .. β]

makepoints :: [(Double, Double)]
makepoints = foldr1 (++) $ map _2points distances

main = plotDots [XRange (α, β), YRange (-β, β)] makepoints
