{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Process.Minizinc
import Process.Minizinc.Inspect

import Paths_minizinc_process

main :: IO ()
main = do
  example001
  example003

-- Example001
data Input = Input {x :: Int}
  deriving (Generic)

instance ToJSON Input

instance Hashable Input

data Output = Output {y :: Int}
  deriving (Show, Generic)

instance FromJSON Output

example001 :: IO ()
example001 = do
  path <- getDataFileName "models/example001.mzn"
  let mzn = simpleMiniZinc @Input @Output path 1000 Gecode
  let solve = runLastMinizincJSON mzn
  traverse solve [Input x | x <- [0..10] ] >>= print

-- Example003
data User = User { name :: Text , likeUser :: User -> Int }

userFromPairs :: Text -> [(Text, Int)] -> Int -> User
userFromPairs n pairs defaultlikeness =
  User n f
  where
    f user = case lookup (name user) pairs of Nothing -> defaultlikeness ; Just v -> v

data Room = Room { label :: Text, roomSize :: Int }
  deriving (Show)

data Input003 = Input003 { nUsers :: Int , nRooms :: Int , attirance :: [[Int]] , size :: [Int] }
  deriving (Generic)
instance ToJSON Input003

instance Hashable Input003

data Output003 = Output003 { room :: [Int] }
  deriving (Show, Generic)

instance FromJSON Output003

example003 :: IO ()
example003 = do
  path <- getDataFileName "models/example003.mzn"
  let solve = runLastMinizincJSON (simpleMiniZinc @Input003 @Output003 path 10000 Gecode)

  let prefs = [ [likeUser u1 u2 | u2 <- users] | u1 <- users ]
  output <- solve $ Input003 (length users) (length rooms) prefs [roomSize r | r <- rooms]
  case output of
    Nothing -> print "no solutions"
    Just mapping -> print $ zip (fmap name users)
                                (fmap (\roomidx -> label $ rooms !! roomidx) (room mapping))

  where
    users = [albert, paul, philip, sarah, sylvia, zoe]
    rooms = [large, medium, small]

    albert, paul, philip, sarah, sylvia, zoe :: User
    albert = userFromPairs "albert" [("zoe", 8), ("paul", 2)] (-1)
    paul   = userFromPairs "paul"   [("philip", 10), ("sarah", 3), ("sylvia", -5)] 0
    philip = userFromPairs "philip" [("philip", 5), ("sarah", 5), ("sylvia", 3)] 1
    sarah  = userFromPairs "sarah"  [("sylvia", 5), ("paul", -4)] 0
    sylvia = userFromPairs "sylvia" [("paul", -5), ("sarah", 10), ("philip", 10)] 3
    zoe    = userFromPairs "zoe"    [] 0

    large, medium, small :: Room
    large  = Room "large"  4
    medium = Room "medium" 3
    small  = Room "small"  2
