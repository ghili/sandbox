module Hshoot.GlUtil where

import Hshoot.Soe as S
import Graphics.Rendering.OpenGL.GL.BeginEnd as Gl(renderPrimitive, PrimitiveMode(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec(color, Color3(..), ColorComponent)
import Graphics.Rendering.OpenGL.GL.VertexSpec(Vertex(..))
import Graphics.Rendering.OpenGL.GL(Vertex3(..))
import Graphics.Rendering.OpenGL.GL(GLfloat)
import Control.Monad
import Foreign.C.Types(CFloat)

type GlColor = Color3 Double

triangles :: [S.Vertex] -> IO()
triangles points =
    renderPrimitive Gl.Triangles $ do
      forM_ points (\point@(x, y, z) ->
                           vertex $ Vertex3 ((realToFrac x) :: CFloat) (realToFrac y) (realToFrac z))

toColor :: Color -> Color3 CFloat
toColor (r,g,b) = Color3 (realToFrac (fromInteger(r)/255))  (realToFrac(fromInteger(g)/255))  (realToFrac(fromInteger(b)/255))

drawShape :: Shape -> IO()
drawShape (Triangle v1 v2 v3)  = triangles [v1,v2,v3]
drawShape (S.Polygon vs) = 
    renderPrimitive Gl.Polygon $ do
      forM_ vs (\point@(x, y, z) ->
         vertex $ Vertex3 ((realToFrac x) :: CFloat) (realToFrac y) (realToFrac z))


drawRegion :: Region -> IO()
drawRegion (Region shape) = drawShape shape
drawRegion (Union shapes) = mapM_ drawShape shapes


drawPicture :: Picture -> IO()
drawPicture (pic1 `Over` pic2) = do
  drawPicture pic1
  drawPicture pic2
drawPicture (Picture c region) = do
  color (toColor c)
  drawRegion region
