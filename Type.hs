module Type where

import System.Posix.Types

data Photo = Photo
  { photoName :: String
  , time :: EpochTime
  , description :: Maybe String
  }
  deriving (Show, Eq)

instance Ord Photo where
    compare p1 p2 = compare (time p2) (time p1)

data Album = Album
  { albumName :: String
  , photos :: [Photo]
  }
  deriving (Show)
