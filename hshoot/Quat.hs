module Hshoot.Quat where

import Hshoot.Soe
import Debug.Trace

{--
cf http://tfc.duke.free.fr/coding/md5-specs-en.html
--}

data Quaternion = Q Float [Float]

instance Eq Quaternion where
  (Q s1 v1) /= (Q s2 v2) = s1 /= s2 || v1 /= v2

instance Show Quaternion where
  showsPrec p (Q s [x, y, z]) =
    showChar '[' . shows s . showString (signify "" x)
                 . shows (abs x) . showString (signify "i" y)
                 . shows (abs y) . showString (signify "j" z)
                 . shows (abs z) . showString "k]"
    where
    signify name number = name ++ (if number >= 0.0 then " + " else " - ")

instance Num Quaternion where
  fromInteger x = Q (fromInteger x) []
  (Q s1 v1) + (Q s2 v2) = Q (s1 + s2) (zipWith (+) v1 v2)
  (Q s1 v1) - (Q s2 v2) = Q (s1 - s2) (zipWith (-) v1 v2)

  (Q s1 [x1, y1, z1]) * (Q s2 [x2, y2, z2])
     = Q (s1 * s2 - x1 * x2 - y1 * y2 - z1 * z2)
      [(s1 * x2 + x1 * s2 + y1 * z2 - z1 * y2),
       (s1 * y2 - x1 * z2 + y1 * s2 + z1 * x2),
       (s1 * z2 + x1 * y2 - y1 * x2 + z1 * s2)]

  abs (Q s v) = Q (abs s) v

  signum (Q s v) = Q (signum s) [0, 0, 0]

conjugate :: Quaternion -> Quaternion
conjugate (Q w [x, y, z]) = (Q w [-x, -y, -z])

magnitude = sqrt . (foldr f 0.0)
        where a `f` b = a * a + b

vertNormalize :: Vertex -> Vertex
vertNormalize (vx, vy, vz)=
    let mag = magnitude [vx, vy, vz] in
    (vx / mag, vy / mag, vz / mag)

quatMagnitude :: Quaternion -> Float
quatMagnitude (Q w v) = magnitude (w : v)

quatComplete :: Vertex -> Quaternion
quatComplete (x, y, z) =
    let diff = 1 - x*x - y*y - z*z in
    if (diff > 0) then
       Q (- (sqrt diff)) [x, y, z]
    else
       Q 0 [x, y, z] --error "can't complete quat"

quatNormalize :: Quaternion -> Quaternion
quatNormalize q@(Q w [x, y, z]) = 
    let mag = quatMagnitude q in
    if mag == 0
       then
--           (Q 0 [0, 0, 0])
           error "magnitude nulle ?"
       else
           (Q (w/mag) [x/mag, y/mag, z/mag])

multQuatVert :: Quaternion -> Vertex -> Quaternion
multQuatVert (Q w [x, y, z]) (vx, vy, vz) =
    (Q (-x*vx - y*vy - z*vz)
       [w*vx + y*vz - z*vy,
        w*vy + z*vx - x*vz,
        w*vz + x*vy - y*vx])

multQuat ::  Quaternion -> Quaternion -> Quaternion
multQuat (Q w v1@[x, y, z]) (Q wb v2@[xb, yb, zb])= 
    (Q (w*wb - (scalar v1 v2))
        (vplus (vplus (prod wb v1) (prod w v2)) (pvect v1 v2)))
    where scalar [x1, y1, z1] [x2, y2, z2]= x1 * x2 + y1 * y2 + z1 * z2
          prod k [x, y, z] = [k * x, k * y, k * z]
          vplus [x1, y1, z1] [x2, y2, z2] = [x1 + x2, y1 + y2, z1 + z2]
          vsub [x1, y1, z1] [x2, y2, z2] = [x1 - x2, y1 - y2, z1 - z2]
          pvect [x1, y1, z1] [x2, y2, z2] = [y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2]
          

rotateVert :: Quaternion -> Vertex -> Vertex
rotateVert q v =
    let inv = (quatNormalize . conjugate) q
        q2 = multQuatVert q v
        (Q w [x, y, z]) = multQuat q2 inv in
    (x, y, z)