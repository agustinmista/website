{-# LANGUAGE OverloadedStrings #-}

import System.FilePath

import Hakyll

----------------------------------------
-- Site builder

main :: IO ()
main = hakyllWith defaultConfiguration { providerDirectory = "website" } $ do

  match "index.md" $ do
    let ctx = defaultContext <> pubsContext <> postsContext
    route (setExtension "html")
    compile $ do
      getResourceBody
      >>= applyAsTemplate ctx
      >>= renderPandoc
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
  
  match "static/*" $ do
    let ctx = defaultContext
    route (topLevel `composeRoutes` niceRoute)
    compile $ do
      pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  create ["pubs/index.html"] $ do
    let ctx = defaultContext <> pubsContext
    route idRoute
    compile $ do
      newItem 
      >>= loadAndApplyTemplate "templates/pub-archive.html" ctx
      >>= loadAndApplyTemplate "templates/default.html"     ctx
      >>= relativizeUrls
  
  create ["teaching/index.html"] $ do
    let ctx = defaultContext <> coursesContext
    route idRoute
    compile $ do
      newItem
      >>= loadAndApplyTemplate "templates/course-archive.html" ctx
      >>= loadAndApplyTemplate "templates/default.html"        ctx
      >>= relativizeUrls

  create ["blog/index.html"] $ do
    let ctx = defaultContext <> postsContext
    route idRoute
    compile $ do
      newItem
      >>= loadAndApplyTemplate "templates/post-archive.html" ctx
      >>= loadAndApplyTemplate "templates/default.html"      ctx
      >>= relativizeUrls

  match "pubs/*" $ version "pub" $ do
    let ctx = defaultContext 
    route (setExtension "html")
    compile $ do
      pandocCompiler
      >>= loadAndApplyTemplate "templates/pub.html"     ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
  
  match "teaching/*" $ version "course" $ do
    let ctx = defaultContext <> dateContext 
    route (setExtension "html")
    compile $ do
      pandocCompiler
      >>= loadAndApplyTemplate "templates/course.html"  ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  match "blog/*" $ version "post" $ do
    let ctx = defaultContext <> dateContext 
    route (setExtension "html")
    compile $ do
      pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
  
  match "assets/**/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ do
    compile templateBodyCompiler

----------------------------------------
-- Loaders

loadPubs :: Compiler [Item String]
loadPubs = loadAll ("pubs/*" .&&. hasVersion "pub")

loadPosts :: Compiler [Item String]
loadPosts = loadAll ("blog/*" .&&. hasVersion "post")

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

pubsContext :: Context String
pubsContext = listField "pubs" defaultContext (loadPubs >>= recentFirst)

coursesContext :: Context String
coursesContext = listField "courses" defaultContext (loadCourses >>= recentFirst) 

----------------------------------------
-- Helpers

newItem :: Monoid m => Compiler (Item m)
newItem = makeItem mempty

niceRoute :: Routes
niceRoute = customRoute $ \ident -> 
  takeDirectory (toFilePath ident) </> takeBaseName (toFilePath ident) </> "index.html"

topLevel :: Routes
topLevel = customRoute (takeBaseName . toFilePath)
