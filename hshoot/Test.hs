module Hshoot.Test where

import Hshoot.Soe
import Hshoot.Quat
import Test.QuickCheck

instance Arbitrary Quaternion where
    arbitrary = do
      w <- arbitrary :: Gen Float
      x <- arbitrary :: Gen Float
      y <- arbitrary :: Gen Float
      z <- arbitrary :: Gen Float
      return (Q w [x, y, z])

prop_magnitude x = quatMagnitude (conjugate x) == quatMagnitude x

--prop_magnitude2 = quatMagnitude (Q 2 [3, 5, 7]) == (sqrt (4+9+25+49))

prop_identite2pi v = rotateVert (Q 1 [0, 0, 0]) v == v

prop_normalize v = not (v == (0,0,0)) ==> (1 - magnitude (toList $ vertNormalize v)) < 0.1
toList (x, y, z) = [x, y, z]

prop_rotate = rotateVert (Q ((sqrt 2) /2) [0, 0, (sqrt 2) / 2]) (1, 0, 0) == (0, 1, 0)

prop_rotate2 = rotateVert (Q (cos (pi/8)) [0, 0, sin(pi/8)]) (1, 0, 0) == (sqrt(2)/2, sqrt(2)/2, 0)

--prop_rotate3 = rotateVert (Q (cos (pi/8)) [0, sin(pi/8), 0]) (1, 0, 0) == (sqrt(2)/2, 0, sqrt(2)/2)
--prop_rotate3 = rotateVert (Q (cos (pi/8)) [sin(pi/8),0,0]) (1, 0, 0) == (1,0,0)