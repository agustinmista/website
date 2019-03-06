{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import System.FilePath

import Hakyll


main :: IO ()
main = hakyll $ do

  match "assets/**/*" $ do
    route   $ idRoute
    compile $ copyFileCompiler

  match "css/*" $ do
    route   $ idRoute
    compile $ compressCssCompiler

  match "templates/*" $ do
    compile $ templateBodyCompiler

  match "static/*" $ do
    route   $ topLevel .$. niceRoute
    compile $ do
      let context = defaultContext
      pandocCompiler
        >>= applyDefaultTemplate context
        >>= relativizeUrls

  match "publications/*" $ version "pub" $ do
    route   $ setExtension "html"
    compile $ do
      let context = pubCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/pub.html" context
        >>= applyDefaultTemplate context
        >>= relativizeUrls
  
  match "teaching/*" $ version "course" $ do
    route   $ setExtension "html"
    compile $ do
      let context = courseCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/course.html" context
        >>= applyDefaultTemplate context
        >>= relativizeUrls

  match "blog/*" $ version "post" $ do
    route   $ setExtension "html"
    compile $ do
      let context = postCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" context
        >>= applyDefaultTemplate context
        >>= relativizeUrls


  match "index.md" $ do
    route   $ setExtension "html"
    compile $ do
      pubs  <- takeRecent =<< loadPublications
      posts <- takeRecent =<< loadPosts
      let context = indexCtx posts pubs
      getResourceBody
        >>= applyAsTemplate context
        >>= renderPandoc
        >>= applyDefaultTemplate context
        >>= relativizeUrls

  create ["publications/index.html"] $ do
    route   $ idRoute
    compile $ do
      pubs <- recentFirst =<< loadPublications
      let context = pubsCtx pubs
      newItem
        >>= loadAndApplyTemplate "templates/pub-archive.html" context
        >>= applyDefaultTemplate context
        >>= relativizeUrls
  
  create ["teaching/index.html"] $ do
    route   $ idRoute
    compile $ do
      courses <- recentFirst =<< loadCourses
      let context = coursesCtx courses
      newItem
        >>= loadAndApplyTemplate "templates/course-archive.html" context
        >>= applyDefaultTemplate context
        >>= relativizeUrls

  create ["blog/index.html"] $ do
    route   $ idRoute
    compile $ do
      posts <- recentFirst =<< loadPosts
      let context = blogCtx posts
      newItem
        >>= loadAndApplyTemplate "templates/post-archive.html" context
        >>= applyDefaultTemplate context
        >>= relativizeUrls


-- Loaders
loadPublications :: Compiler [Item String]
loadPublications = loadAll ("publications/*" .&&. hasVersion "pub")

loadPosts :: Compiler [Item String]
loadPosts = loadAll ("blog/*" .&&. hasVersion "post")

loadCourses :: Compiler [Item String]
loadCourses = loadAll ("teaching/*" .&&. hasVersion "course")

-- Item collection contexts
indexCtx :: [Item String] -> [Item String] -> Context String
indexCtx posts pubs
  =  listField  "pubs"  pubCtx  (pure pubs)
  <> listField  "posts" postCtx (pure posts)
  <> defaultContext

pubsCtx :: [Item String] -> Context String
pubsCtx pubs
  =  listField  "pubs"  pubCtx (pure pubs)
  <> defaultContext

coursesCtx :: [Item String] -> Context String
coursesCtx courses
  =  listField  "courses"  courseCtx (pure courses)
  <> defaultContext

blogCtx :: [Item String] -> Context String
blogCtx posts
  =  listField  "posts" postCtx (pure posts)
  <> defaultContext

-- Single item contexts
postCtx :: Context String
postCtx
  =  dateField "date" "%B %e, %Y"
  <> defaultContext

pubCtx :: Context String
pubCtx = defaultContext

courseCtx :: Context String
courseCtx = defaultContext


-- Utilities
applyDefaultTemplate :: Context a -> Item a -> Compiler (Item String)
applyDefaultTemplate = loadAndApplyTemplate "templates/default.html"

takeRecent :: [Item String] -> Compiler [Item String]
takeRecent = fmap (take 4) . recentFirst

newItem :: Monoid m => Compiler (Item m)
newItem = makeItem mempty

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident
      = takeDirectory (toFilePath ident)
        </> takeBaseName (toFilePath ident)
        </> "index.html"

topLevel :: Routes
topLevel = customRoute (takeBaseName . toFilePath)

(.$.) :: Routes -> Routes -> Routes
(.$.) = composeRoutes
