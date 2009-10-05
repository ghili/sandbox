module Hshoot.Main where

import Foreign.C.Types
import Control.Concurrent.Chan
import Data.StateVar
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.CPUTime
import Graphics.UI.SDL.General as Sdl
import Graphics.UI.SDL.Types(SurfaceFlag(..))
import Graphics.UI.SDL.Video as Sdl
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Time
import Graphics.Rendering.OpenGL.GL.Framebuffer(clear, ClearBuffer(..))
import Graphics.Rendering.OpenGL.GL.CoordTrans(ortho, loadIdentity, viewport)
import Graphics.Rendering.OpenGL.GL.BeginEnd(renderPrimitive, PrimitiveMode(..))
import Graphics.Rendering.OpenGL.GL.Polygons(polygonMode, PolygonMode(Line))
import Graphics.Rendering.OpenGL.GL.FlushFinish(flush)
import Graphics.Rendering.OpenGL.GL.CoordTrans(preservingMatrix, translate, rotate, scale)
import Data.Tensor
import Hshoot.Soe
import Hshoot.GlUtil
import Hshoot.Sprite
import Hshoot.ParseMd5

{-- cf 
http://www.libsdl.org/articles.php
http://www.haskell.org/haskellwiki/Opengl
http://hackage.haskell.org/packages/archive/SDL/0.5.5/doc/html/Graphics-UI-SDL-Video.html --}

main :: IO ()
main = do
  Sdl.init [InitVideo]
  surface <- setVideoMode 640 640 16 [OpenGL]
  polygonMode $= (Line, Line)
  ortho 0 1 0 1 (-1) 1
  (eventChan, addEvents) <- createEventStream
  addEvents
  fish <- parseMd5fish

  let drawPic (Just pic) =
       do clear [ColorBuffer, DepthBuffer]
          preservingMatrix $ do
            rotate 15 $ Vector3 1 1 (1::CFloat)
--            rotate 45 $ Vector3 1 0 (0::CFloat)
--            rotate (-45) $ Vector3 1 0 (0::CFloat)
            translate $ Vector3 0.4 0.4 (0::CFloat)
            scale 0.2 0.2 (0.2::CFloat)
--          putStrLn $ show pic
            drawPicture pic
            flush
            glSwapBuffers
          -- boucle en ajoutant le prochain event
            addEvents
      drawPic Nothing = return ()

  case fish of
    Left e -> do putStrLn "Error parsing : "
                 print e
    Right md5 ->
  --sample snapshot crée un event de nothing et revoie la dernière image calculée?
  -- pb une succession de nothing renvoie la même image? donc pas d'anim possible? non car renvoie un behavior
        mapM_ drawPic (runEvent (sample `snapshot_` fishb) eventChan)
        where fishb = picToConstBeh $ md5ToPic md5

runEvent (UserEvent fe) actionTimes = fe actionTimes

createEventStream :: IO(([Maybe UserAction], [Time]), IO ())
createEventStream = do
  (events, addEvent) <- makeStream
  t0 <- getTicks
  -- ajoute des actions timestampées jusqu'à ce qu'il n'y en ai plus et ajoute nothing

  let loop t = do
        event <- pollEvent
        case event of
         NoEvent -> return ()
         KeyDown(Keysym {symKey = SDLK_ESCAPE}) -> do 
                   quit 
                   exitWith ExitSuccess
         _ -> do addEvent (Just event, t)
                 loop t

      addEvents = do
        t <- getTicks
        let dt = fromIntegral $ t - t0
        loop dt
        addEvent (Nothing, dt)

  return (unzip events, addEvents) -- unzip?



makeStream :: IO ([a], a -> IO())
makeStream = do
  chan <- newChan
  contents <- getChanContents chan
  return (contents, writeChan chan)

{--  t0 <- getTicks
  loop t0
  where 
    loop t0 = do
          event <- pollEvent
          case(event) of
            KeyDown(Keysym {symKey = SDLK_ESCAPE}) -> do 
                        quit 
                        exitWith ExitSuccess
{--            KeyDown(Keysym {symKey = SDLK_q}) -> do
                fillRect surface (Just $ Rect 50 45 100 95) pixel
                glSwapBuffers--}
            _ -> return ()
          delay 10
          render t0
          loop t0
    render t0 = do
      t <- getTicks
      clear [ColorBuffer, DepthBuffer]
      drawPicture (Picture (100,50,150)
                  $ Region $ transShape (0.5,0.5,0.5)
                           $ resizeShape 0.5
                           $ movingTriangle (fromIntegral (t - t0)/1000))
--      drawPicture (Picture (50,50,50) (Region $ Triangle (1,0.5,0) (1,1,0) (0.5,1,0)))
      triangles (50,50,50) [(0,0,0), (0.2,0.2,0), (0,0.2,0)]
      polygon (50,50,50) [(0,0,0),(1,0,0), (1,0.1,0), (0,0.1,0)]
      flush
      glSwapBuffers--}