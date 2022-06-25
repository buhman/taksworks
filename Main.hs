{-# LANGUAGE ViewPatterns #-}

import System.Posix.Directory
import System.Posix.Files
import Control.Monad (mapM)
import Data.List (sort)

import Type (Photo(Photo), Album(Album))
import Render (render)
import Image (saveThumbnails)

foreverM :: (Monad m) => m String -> m [String]
foreverM f = do
  v <- f
  if v == "" then pure [] else do
    v1 <- foreverM f
    pure (v:v1)

filterM :: (Monad m) => (String -> m FileStatus) -> (FileStatus -> Bool) -> [String] -> m [String]
filterM f p [] = pure []
filterM f p (x:xs) = do
  status <- f x
  if (p status) then do
    x1 <- filterM f p xs
    pure (x:x1)
    else (filterM f p xs)

getFileStatusRel :: String -> String -> IO FileStatus
getFileStatusRel b x = getFileStatus (b ++ "/" ++ x)

listPaths :: DirStream -> IO [String]
listPaths ds = do
  paths0 <- foreverM $ readDirStream ds
  pure $ filter (\x -> x /= "." && x /= "..") paths0

listPathsPred :: (FileStatus -> Bool) -> String -> IO [String]
listPathsPred p baseDir = do
  ds <- openDirStream baseDir
  paths <- listPaths ds
  filterM (getFileStatusRel baseDir) p paths

isPhotoExt :: String -> Bool
isPhotoExt (reverse -> (t:x:e:d:_))
   = (ext == ".jpg")
  || (ext == ".png")
  || (ext == ".jpeg")
  where
    ext = d:e:x:t:[]
isPhotoExt _ = False

withExt :: String -> String -> String
withExt ext (reverse -> (t:x:e:rest)) = (reverse rest) ++ ext

groupPaths :: [String] -> [(String, Maybe String)]
groupPaths ps = map withDescFile . filter isPhotoExt $ ps
  where
    withDescFile p = let txtFile = withExt "txt" p
                         maybeTxtFile = if elem txtFile ps then Just txtFile else Nothing
                     in (p, maybeTxtFile)

getPhoto :: String -> (String, Maybe String) -> IO Photo
getPhoto baseDir (filename, descriptionPath) = do
  description <- maybe (pure Nothing) get descriptionPath
  status <- getStatus baseDir filename
  pure $ Photo filename (modificationTime status) description
  where
    get p = Just <$> readFile (baseDir ++ "/" ++ p)

getStatus :: String -> String -> IO FileStatus
getStatus baseDir p = do
  status <- getFileStatus pp
  pure status
  where
    pp = baseDir ++ "/" ++ p

getAlbum :: String -> String -> IO Album
getAlbum baseDir name = do
  ds <- openDirStream albumDir
  paths <- listPathsPred isRegularFile albumDir
  photos <- mapM (getPhoto albumDir) $ groupPaths paths
  pure $ Album name (sort photos)
  where
    albumDir = baseDir ++ "/" ++ name

sortFilterLines :: [String] -> [String] -> [String]
sortFilterLines order albums =
  filter (\x -> elem x albums) order

main :: IO ()
main = do
  let baseDir = "./photos"
  dirs <- listPathsPred isDirectory baseDir
  let dirs0 = filter (\(x:_) -> x /= '.') dirs
  lines <- lines <$> readFile "./photos/albums.txt"
  let dirs1 = sortFilterLines lines dirs0
  albums <- mapM (getAlbum baseDir) dirs1
  render albums
  saveThumbnails albums
