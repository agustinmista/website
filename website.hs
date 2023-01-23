{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((>=>))
import Data.List (isSuffixOf)
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.Environment (lookupEnv)

import Hakyll

----------------------------------------
-- Site builder

main :: IO ()
main = do
  websiteUrl <- lookupEnvOrFail "WEBSITE_URL"
  hakyllWith defaultConfiguration { providerDirectory = "website" } $ do

    ----------------------------------------
    -- Index page

    match "index.md" $ do
      let ctx = defaultContext <> pubsContext <> postsContext
      route (setExtension "html")
      compile $ do
        getResourceBody
        >>= applyAsTemplate ctx
        >>= renderPandoc
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    ----------------------------------------
    -- Other standalone static pages (about, contact, etc.)

    match "*.md" $ do
      let ctx = defaultContext
      route niceRoute
      compile $ do
        pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    ----------------------------------------
    -- Publications

    create ["pubs/index.html"] $ do
      let ctx = defaultContext <> pubsContext
      route idRoute
      compile $ do
        newItem
        >>= loadAndApplyTemplate "templates/pub-archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"     ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    match "pubs/*" $ version "pub" $ do
      let ctx = defaultContext
      route niceRoute
      compile $ do
        pandocCompiler
        >>= loadAndApplyTemplate "templates/pub.html"     ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    ----------------------------------------
    -- Blog

    create ["blog/index.html"] $ do
      let ctx = defaultContext <> postsContext
      route idRoute
      compile $ do
        newItem
        >>= loadAndApplyTemplate "templates/post-archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"      ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    match "blog/*" $ version "post" $ do
      let ctx = defaultContext <> dateContext
      route niceRoute
      compile $ do
        pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    ----------------------------------------
    -- Teaching

    create ["teaching/index.html"] $ do
      let ctx = defaultContext <> coursesContext
      route idRoute
      compile $ do
        newItem
        >>= loadAndApplyTemplate "templates/course-archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html"        ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    match "teaching/*" $ version "course" $ do
      let ctx = defaultContext <> dateContext
      route niceRoute
      compile $ do
        pandocCompiler
        >>= loadAndApplyTemplate "templates/course.html"  ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls
        >>= relativizeUrls

    ----------------------------------------
    -- Atom RSS

    create ["atom.xml"] $ do
      let ctx = defaultContext <> postSnapshotsContext <> bodyField "description"
      route idRoute
      compile $ do
        loadPostSnapshots
        >>= traverse (absolutizeUrls websiteUrl)
        >>= renderAtom (atomConfig websiteUrl) ctx

    ----------------------------------------
    -- Assets (images, pdfs, etc.)

    match "assets/**/*" $ do
      route idRoute
      compile copyFileCompiler

    ----------------------------------------
    -- Stylesheets

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    ----------------------------------------
    -- Templates

    match "templates/*" $ do
      compile templateBodyCompiler


----------------------------------------
-- Atom feed configuration

atomConfig :: String -> FeedConfiguration
atomConfig websiteUrl = FeedConfiguration {
  feedTitle = "Agustín Mista",
  feedDescription = "This feed provides the latests posts from my personal website",
  feedAuthorName = "Agustín Mista",
  feedAuthorEmail = "agustin@mista.me",
  feedRoot = websiteUrl
}

----------------------------------------
-- Resource loaders

loadPubs :: Compiler [Item String]
loadPubs = loadAll ("pubs/*" .&&. hasVersion "pub")

loadPosts :: Compiler [Item String]
loadPosts = loadAll ("blog/*" .&&. hasVersion "post")

loadPostSnapshots :: Compiler [Item String]
loadPostSnapshots = loadAllSnapshots ("blog/*" .&&. hasVersion "post") "content"

loadCourses :: Compiler [Item String]
loadCourses = loadAll ("teaching/*" .&&. hasVersion "course")

----------------------------------------
-- Contexts

-- Single item contexts

dateContext :: Context String
dateContext = dateField "date" "%B %e, %Y"

-- List contexts

postsContext :: Context String
postsContext = listField "posts" defaultContext (loadPosts >>= recentFirst)

postSnapshotsContext :: Context String
postSnapshotsContext = listField "post_snapshots" defaultContext (loadPostSnapshots >>= recentFirst)

pubsContext :: Context String
pubsContext = listField "pubs" defaultContext (loadPubs >>= recentFirst)

coursesContext :: Context String
coursesContext = listField "courses" defaultContext (loadCourses >>= recentFirst)

----------------------------------------
-- Helpers

index :: String
index = "index.html"

-- Create a new empty item
newItem :: Monoid m => Compiler (Item m)
newItem = makeItem mempty


-- Make a relative URL absolute given a base domain
-- (only used when creating atom feed)
absolutizeUrls :: String -> Item String -> Compiler (Item String)
absolutizeUrls domain item = return (fmap (relativizeUrlsWith domain) item)

-- Remove the last index.html from a URL
-- E.g.: /foo/bar/index.html --> /foo/bar
-- To be used in conjunction with `niceRoute`
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls item = return (fmap (withUrls cleanIndex) item)
  where
    cleanIndex url
      | index `isSuffixOf` url = take (length url - length index - 1) url
      | otherwise              = url

-- Route an URL through a subfolder and an index file inside
-- E.g.: /foo/bar.md --> /foo/bar/index.html
niceRoute :: Routes
niceRoute = customRoute $ \ident ->
  takeDirectory (toFilePath ident) </>
  takeBaseName (toFilePath ident) </>
  index

-- Look for an environment variable and raise an error if not defined
lookupEnvOrFail :: String -> IO String
lookupEnvOrFail key = do
  mb <- lookupEnv key
  case mb of
    Nothing -> error ("Missing environment variable: " <> key)
    Just val -> return val
