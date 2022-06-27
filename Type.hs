module Type where

import Data.Text.Lazy (Text)
import System.Posix.Types

data Photo = Photo
  { photoName :: String
  , time :: EpochTime
  , description :: Maybe Text
  }
  deriving (Show, Eq)

instance Ord Photo where
    compare p1 p2 = compare (time p2) (time p1)

data Album = Album
  { albumName :: String
  , photos :: [Photo]
  }
  deriving (Show)
