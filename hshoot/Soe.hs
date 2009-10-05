module Hshoot.Soe where

import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym

type Coord = Float

data Shape = Triangle Vertex Vertex Vertex
           | Polygon [Vertex]
 deriving Show

data Region = Region Shape 
            | Union [Shape]
 deriving Show

data Picture = Picture Color Region
             | Picture `Over` Picture
 deriving Show

type Side = (Vertex, Vertex)

type Vertex = (Coord, Coord, Coord)

type Color = (Integer, Integer, Integer)

type Time = Float

type Animation a = Time -> a

type Point = (Int, Int)

type UserAction = Event

newtype Behavior a = Behavior (([Maybe UserAction], [Time]) -> [a])

newtype UserEvent a = UserEvent (([Maybe UserAction], [Time]) -> [Maybe a])

-- crée un behavior de constante de valeur x
liftB x = Behavior( \(actions, times) -> map (\_ -> x) times)

mapB :: Behavior a -> (a -> b) -> Behavior b
mapB (Behavior fb) f = Behavior(\ actionTimes -> map f (fb actionTimes))

-- filtre les actions click gauche
lbp :: UserEvent ()
lbp = UserEvent(\(actions, _) ->
  map (\action -> case action of Just(MouseButtonDown _ _ ButtonLeft) -> Just()
                                 _ -> Nothing) actions)

--leftArrow :: UserEvent()
--leftArrow = case key of UserEvent( 

key :: UserEvent SDLKey
key = UserEvent(\(actions, _) ->
  map (\action -> case action of Just(KeyDown(Keysym k _ _)) -> Just(k)
                                 _ -> Nothing) actions)

-- change un behavior suivant un event
-- pour chaque (action, temps) on teste sur l'event n'est pas vide, alors on applique son behavior
-- le match est lazy pour ne pas évaluer (fe actionTimes) entièrement
untilB :: Behavior a -> UserEvent (Behavior a) -> Behavior a
(Behavior fb) `untilB` (UserEvent fe) = 
  Behavior (\actionTimes @ (actions, times) -> loop actions times (fe actionTimes) (fb actionTimes))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) = 
              b : (case e of
                    Nothing -> loop us ts es bs
                    Just (Behavior fb') -> fb'(us, ts))

switchB :: Behavior a -> UserEvent (Behavior a) -> Behavior a
(Behavior fb) `switchB` (UserEvent fe) = 
  Behavior (\actionTimes @ (actions, times) -> loop actions times (fe actionTimes) (fb actionTimes))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) = 
              b : (case e of
                    Nothing -> loop us ts es bs
                    Just (Behavior fb') -> loop us ts es (fb'(us, ts)))

-- en fait map
(=>>) :: UserEvent a -> (a -> b) -> UserEvent b
UserEvent fe =>> f = UserEvent (\uts -> map aux (fe uts))
                 where aux (Just a) = Just (f a)
                       aux Nothing = Nothing

-- affirme pour un event sans sujet qu'il est sujet d'un behavior
(->>) :: UserEvent () -> Behavior b -> UserEvent (Behavior b)
e ->> b = e =>> (\_ -> b)

-- renvoie un event qui contient en plus du sujet de l'event, le behavior avant event
snapshot :: UserEvent a -> Behavior b -> UserEvent (a, b)
UserEvent fe `snapshot` Behavior fb = UserEvent (\uts -> zipWith aux (fe uts) (fb uts))
    where aux (Just x) y = Just (x, y)
          aux Nothing _ = Nothing

-- pour un event qui a déjà un sujet le discarde en event d'un behavior
snapshot_ :: UserEvent a -> Behavior b -> UserEvent b
snapshot_ e b = e `snapshot` b =>> snd

-- crée un event qui ne s'applique que lors d'actions nothing
sample :: UserEvent ()
sample = UserEvent (\(actions, _) ->  map aux actions)
    where aux Nothing = Just()
          aux (Just _) = Nothing

trans :: Vertex -> Vertex -> Vertex
trans (x, y, z) (i, j, k) = (x + i, y + j, z + k)

transShape :: Vertex -> Shape -> Shape
transShape v (Triangle v1 v2 v3) = Triangle (trans v v1) (trans v v2) (trans v v3)
transShape v (Polygon vs) = Polygon (map (trans v) vs)

resize :: Float -> Vertex -> Vertex
resize f (x, y, z) = (x * f , y * f, z *f)

resizeShape :: Float -> Shape -> Shape
resizeShape f (Triangle v1 v2 v3) = Triangle (resize f v1) (resize f v2) (resize f v3)
resizeShape f (Polygon vs) = Polygon (map (resize f) vs)

