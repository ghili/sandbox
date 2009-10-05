module Hshoot.ParseMd5 where

import Text.ParserCombinators.Parsec
import System.Directory(getHomeDirectory)
import Hshoot.Soe
import Hshoot.Quat
import Debug.Trace

data Md5Joint = Md5Joint {
      name :: String,
      jointParent :: Int,
      pos :: Vertex,
      orient :: Quaternion
} deriving Show

data Md5Mesh = Md5Mesh {
      shader :: String,
      numVertex :: Int,
      vertex :: [Md5Vertex],
      numTriangle :: Int,
      triangles :: [Md5Triangle],
      numWeight :: Int,
      weights :: [Md5Weight]
} deriving Show

data Md5Vertex = Md5Vertex {
      vertexIndex :: Int,
      sTex :: Coord,
      tTex :: Coord,
      startWeight :: Int,
      countWeight :: Int
}

data Md5Triangle = Md5Triangle {
      triangleIndex :: Int,
      vertexIndexes :: [Int]
}

data Md5Weight = Md5Weight {
      weightIndex :: Int,
      joint :: Int,
      bias :: Float,
      weightPos :: Vertex
}

data Md5Header = Md5Header {
      version :: String,
      commandLine :: String,
      numJoints :: Int,
      numMeshes :: Int
} deriving Show

data Md5Data = Md5Data {
      md5Header :: Md5Header,
      joints :: [Md5Joint],
      meshes :: [Md5Mesh]
} deriving Show

instance Show(Md5Vertex) where
    show (Md5Vertex idx s t sw cw) = 
        "vert " ++ (show idx) ++ " " ++ (show s) ++ " " ++ (show t)++ " " ++ (show sw)++ " " ++ (show cw) ++ "\n"

instance Show(Md5Triangle) where
    show (Md5Triangle idx [v0, v1, v2]) = 
        "tri " ++ (show idx) ++ " " ++ (show v0) ++ " " ++ (show v1)++ " " ++ (show v2) ++ "\n"

instance Show(Md5Weight) where
    show (Md5Weight idx j b pos) = 
        "weight " ++ (show idx) ++ " " ++ (show j) ++ " " ++ (show b)++ " " ++ (show pos) ++ "\n"

readMd5File :: IO String
readMd5File = do
  homedir <- getHomeDirectory
--  Prelude.readFile $ homedir ++ "/work/workspaces/blender/blender2md5/test/Bob_with_lamp_clean.md5mesh"
  Prelude.readFile $ homedir ++ "/work/workspaces/blender/blender2md5/test/test.md5"

md5Parser :: GenParser Char st Md5Data
md5Parser = do
  header <- md5HeaderParser
  eol
  string "joints {" >>  eol
  jointLines <- cumulLines md5JointLineParser
  eol
  m <- md5MeshParser
  return Md5Data { md5Header = header, joints = jointLines, meshes = [m]}

md5JointLineParser = do
  (many (oneOf "\t "))
  n <- quotedWord
  p <- nextWord
  string " ("
  px <- nextWord
  py <- nextWord
  pz <- nextWord
  string " ) ("
  ox <- nextWord
  oy <- nextWord
  oz <- nextWord
  string " )" >> eol
  return Md5Joint{
                 name = n,
                 jointParent = read p ,
                 pos = ((read px),(read py),(read pz)),
                 orient = quatComplete ((read ox),(read oy),(read oz))
               }

md5MeshParser :: GenParser Char st Md5Mesh
md5MeshParser = do
  string "mesh {" >>  eol >> namedWord "shader "
  s <- quotedWord
  eol >> eol >> namedWord "numverts "
  nv <- nextWord
  eol
  v <- cumulLines md5VertexParser
  eol >> namedWord "numtris "
  nt <- nextWord
  t <- cumulLines md5TriangleParser
  eol >> namedWord "numweights "
  nw <- nextWord
  w <- cumulLines md5WeightParser
  return Md5Mesh { shader = s , numVertex = read nv, vertex = v, numTriangle = read nt , triangles = t, numWeight = read nw, weights = w}

md5VertexParser = do
  namedWord "vert"
  idx <- nextWord
  string " ("
  s <- nextWord
  t <- nextWord
  string " )"
  sw <- nextWord
  cw <- nextWord
  return Md5Vertex{ vertexIndex = read idx, sTex = read s, tTex = read t, startWeight = read sw, countWeight = read cw }

md5TriangleParser = do
  namedWord "tri"
  idx <- nextWord
  v0 <- nextWord
  v1 <- nextWord
  v2 <- nextWord
  return Md5Triangle{ triangleIndex = read idx, vertexIndexes = [read v0, read v1, read v2]}

md5WeightParser = do
  namedWord "weight"
  idx <- nextWord
  j <- nextWord
  b <- nextWord
  string " ("
  px <- nextWord
  py <- nextWord
  pz <- nextWord
  string " )"
  return Md5Weight{ weightIndex = read idx, joint = read j, bias = read b, weightPos = (read px, read py, read pz) }

md5HeaderParser :: GenParser Char st Md5Header
md5HeaderParser = do
  string "MD5Version "
  v <- many digit
  eol >> string "commandline "
  cmd <- quotedWord
  eol >> eol >> string "numJoints "
  nj <- many digit
  eol >> string "numMeshes "
  nm <- many digit
  eol
  return Md5Header{version = v, commandLine = cmd, numJoints = read nj, numMeshes = read nm}

quotedWord = do
  char '"'
  word <- many (noneOf "\"")
  char '"'
  return word

nextWord = do
  many (space <|> tab)
  word <- many (noneOf "\n\r\t ")
  return word

namedWord name = do
  many (space <|> tab)
  string name

eol = try(string "\n\r")
      <|> try(string "\r\n")
      <|> string "\n"
      <|> try(comment >> eol)
      <?> "end of line"

comment = do
  many (oneOf " \t" ) 
  string "//"
  many (noneOf "\n")

cumulLines getLine = do
  line <- getLine
  next <- nextLines
  return $ line : next
  where nextLines = do
               try(cumulLines getLine)
               <|> (many (noneOf "\r\n") >> eol >> return [])


test = do
  content <- readMd5File
  case parse md5Parser "test" content of
    Left e -> do putStrLn "Error parsing : "
                 print e
    Right r -> print r

parseMd5fish = do 
  content <- readMd5File
  return $ parse md5Parser "test" content