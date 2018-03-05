-- COMP1100
-- Semester 1, 2018
-- Week 2 Lab
-- Danny Feng (Yanming), February 2018
module Mensuration where
-- Here are a few simple mensuration definitions:
cube::Integer -> Integer
cube x = x*x*x

edgeLength::Integer
edgeLength = 3

volume::Integer
volume = cube edgeLength

-- If I remember correctly, tis is the formula for the surface area
-- of a sphere, in terms of its radius:
surfaceAreaWithRadius :: Float -> Float
surfaceAreaWithRadius r = 4.0 * pi * r^2

prettyQuadraticFormula :: Float -> Float -> Float -> (Float, Float)
prettyQuadraticFormula a b c = ( (-b + sqrtDiscriminant) / denominator,
                                 (-b - sqrtDiscriminant) / denominator )
  where
    sqrtDiscriminant = sqrt (b*b - 4*a*c)
    denominator = 2*a

areaOfTriangle :: Float -> Float -> Float -> Maybe Float
areaOfTriangle a b c
 | (a+b>=c && a+c>=b && b+c>=a) = Just (sqrt(s*(s-a)*(s-b)*(s-c)))
 | (a+b<c || a+c<b || b+c<a) = Nothing
 | otherwise = error "Non-exhaustive guards in function: grade"

 where
 s = (a+b+c)/2
