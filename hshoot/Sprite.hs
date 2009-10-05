module Hshoot.Sprite where

import Hshoot.Soe
import Hshoot.Quat
import Hshoot.ParseMd5
import Debug.Trace
import Graphics.UI.SDL.Keysym

md5Vertex :: Md5Data -> [Vertex]
md5Vertex (Md5Data _ joints meshes) = do
    mesh <- meshes
    vert <- vertex mesh
    let subweights = take (countWeight vert) .  drop (startWeight vert) . weights
    return $ foldl (\ (x, y, z) (Md5Weight _ jointIndex bias wpos) ->
               let (Md5Joint _ _ (px, py, pz) orient) = joints !! jointIndex
                   (vx, vy, vz) = rotateVert orient wpos in
               ((px + vx) * bias , (py + vy) * bias , (pz + vz) * bias)
              ) (0,0,0) (subweights mesh)

md5Triangles :: Md5Data -> [Vertex] -> [Shape]
md5Triangles (Md5Data _ _ meshes) vertex = do
  mesh <- meshes
  triangle <- triangles mesh
  let vert n = vertex !! ((vertexIndexes triangle ) !! n)
  return (Triangle (vert 0) (vert 1) (vert 2))

md5ToPic :: Md5Data -> Picture
md5ToPic md5Data =
    Picture blue 
      $ Union
      $ md5Triangles md5Data (md5Vertex md5Data)
--      $ (map (transShape (0.5,0.5,0.5)) $ md5Triangles md5Data (md5Vertex md5Data))

md5ToPicj :: Md5Data -> Picture
md5ToPicj (Md5Data md5Header joints meshes) =
    Picture blue 
      $ Region
      $ Polygon $ (map (\joint -> pos joint) joints)

picToConstBeh :: Picture -> Behavior Picture
picToConstBeh pic = 
    Behavior(\(a, t) ->  map (\x -> pic) a)

movingTriangle :: Animation Shape
movingTriangle t = Triangle (cos t, sin t, 0) (0, 0, 0) (cos (t+1.5), sin(t+1.5) ,0)

red :: Color
red = (255,0,0)

blue :: Color
blue = (0,0,255)

bred :: Behavior Color
bred = liftB red

bblue :: Behavior Color
bblue = liftB blue

color1 :: Behavior Color
color1 = (untilB bred (lbp ->> bblue))

triangle1 :: Behavior Color -> Behavior Picture
triangle1 (Behavior fb) = Behavior(\actionTimes ->
  map (\c -> Picture c $ Region $ Triangle (1,0.5,0) (1,1,0) (0.5,1,0)) (fb actionTimes))

movingTriangle1 :: Behavior Picture 
movingTriangle1 = Behavior( \(actions , times) ->
  map (\t_ -> let t = t_ /300 in
              Picture (0,0,255) 
              $ Region 
              $ transShape (0.5,0.5,0.5) 
              $ resizeShape 0.5
              $ Triangle (cos t, sin t, 0) (0, 0, 0) (cos (t+1.5), sin(t+1.5) ,0)) times)

coord :: Behavior Vertex
coord = (liftB (0,0,0)) `switchB` (key `snapshot` coord =>>
   (\(k, old@(xo, yo, zo)) -> case k of
            SDLK_q -> liftB (xo - 0.01 , yo, zo)
            SDLK_d -> liftB (xo + 0.01 , yo, zo)
            SDLK_z -> liftB (xo , yo + 0.01, zo)
            SDLK_s -> liftB (xo , yo - 0.01, zo)
            _ -> liftB old))

movingTriangle2 :: Behavior Picture
movingTriangle2 = 
    mapB coord (\(x, y, z) ->
                    Picture (0,0,255) 
                    $ Region 
                    $ transShape (0.5 +x ,0.5 +y ,0.5 + z) 
                    $ resizeShape 0.5
                    $ Triangle (0, 0, 0) (0.2, 0, 0) (0.1, 0.1 ,0))

