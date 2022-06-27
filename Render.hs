{-# LANGUAGE OverloadedStrings #-}

module Render where

import Prelude hiding (head, title, div)
import qualified Prelude as P

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T

import System.Directory (createDirectoryIfMissing)

import Control.Monad (forM_, mapM_)

import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A hiding (title)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Type

figureCaption :: T.Text -> Html
figureCaption s = do
  figcaption $ toHtml s

galleryFigure :: Bool -> Album -> Photo -> Html
galleryFigure direct album photo = figure $ do
  a ! href hrefPath $ do
    img ! src imgPath
  if direct then mapM_ figureCaption (description photo) else pure ()
  where
    photoPath = toValue $ "/photos/" ++ (albumName album) ++ "/" ++ (photoName photo)
    pagePath = toValue $ "/" ++ (albumName album) ++ "/" ++ (photoName photo) ++ "/"
    thumbPath = toValue $ "/thumbnails/" ++ (albumName album) ++ "/" ++ (photoName photo) ++ ".png"
    hrefPath = if direct then photoPath else pagePath
    imgPath = if direct then photoPath else thumbPath

grid :: Album -> Html
grid album = do
  section $ do
    h2 $ toHtml $ albumName album
    div ! class_ "gallery" $ mapM_ (galleryFigure False album) (photos album)


headSegment :: Html
headSegment = head $ do
  title "taks.works"
  meta ! charset "utf-8"
  link ! rel "shortcut icon" ! href "/static/logo.png"
  meta ! name "viewport" ! content "width=device-width"
  link ! rel "stylesheet" ! href "/static/stylesheet.css"

copyright :: T.Text
copyright = "© 2022 taks.works"

footerSegment :: Html
footerSegment = footer $ toHtml $ copyright

mainPage :: [Album] -> Html
mainPage albums = html $ do
  headSegment
  body $ do
    header $ do
      a ! href "/" $ do
        img ! src "/static/logo.png" ! alt "taks.works"
        h1 "taks.works"
    --section $ do
    --  h2 "albums"
    --  div ! class_ "gallery" $ mapM_

    mapM_ grid albums

    footerSegment


linkPath :: Album -> Photo -> AttributeValue
linkPath album photo = toValue $ "/" ++ albumName album ++ "/" ++ photoName photo ++ "/"

photoPage :: Album -> Photo -> Maybe Photo -> Maybe Photo -> Html
photoPage album photo prevPhoto nextPhoto = html $ do
  headSegment
  body $ do
    nav $ do
      a ! href "/" $
        img ! src "/static/logo.png" ! alt "taks.works"

      mapM_ prev prevPhoto
      mapM_ next nextPhoto

    div ! class_ "single" $ galleryFigure True album photo

    footerSegment

  where
    prev p = a ! href (linkPath album p) $
      img ! src "/static/prev.png" ! alt "prev"

    next p = a ! href (linkPath album p) $
      img ! src "/static/next.png" ! alt "next"



renderPhoto :: Album -> Photo -> Maybe Photo -> Maybe Photo -> IO ()
renderPhoto album photo prevPhoto nextPhoto = do
  let s = renderHtml $ photoPage album photo prevPhoto nextPhoto
  createDirectoryIfMissing True dirPath
  B.writeFile path s
  where
    dirPath = (albumName album ++ "/" ++ photoName photo)
    path = (dirPath ++ "/index.html")
    staticPath = (dirPath ++ "/static")

renderAlbum :: Album -> IO ()
renderAlbum album = do
  f (photos album) Nothing
  where
    f (c:next:rest) prev = do
      renderPhoto album c prev (Just next)
      f (next:rest) (Just c)
    f (c:[]) prev = renderPhoto album c prev Nothing

render :: [Album] -> IO ()
render albums = do
  let s = renderHtml $ mainPage albums
  B.writeFile "index.html" s

  mapM_ renderAlbum albums
