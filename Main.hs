{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Hakyll
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Data.Monoid (mappend)
import qualified Data.Map as M
import Data.Traversable (for)
import Control.Monad.Trans.State.Strict (execState, modify)

--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------
behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x

partitionOn :: (a -> Bool) -> [a] -> [[a]]
partitionOn _ [] = []
partitionOn f xs' = filter (not . null) $ go f xs' []
  where
    go _ [] front = [reverse front] 
    go f (x:xs) front = if f x
                          then (reverse front) : (go f xs [x])
                          else go f xs (x:front)


partitionWithState :: (a -> Bool) -> [a] -> [[a]]
partitionWithState f xs = reverse $ flip execState [[]] $ for xs $ \x -> do
  if f x
     then modify ([x]:)
     else modify $ \(x':xs') -> (x'++ [x]) : xs'

headerLevelEq n (Header n' _ _) = n == n'
headerLevelEq _ _ = False

partitionPandoc (Pandoc m blks) = map (Pandoc m) $ partitionOn (headerLevelEq 2) blks


test = do
  Right doc <- fmap (readOrg defaultHakyllReaderOptions) $ readFile "blog.org"
  print doc
  putStrLn "---------------------------------------------"
  for (zip [1..] $ partitionPandoc doc) $ \ (n,d) -> do
    print (n, writeMarkdown def d)
--splitDocAtLevel = walkM $ splitWhen (headerLevelEq 2)

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  `mappend` mathCtx
  `mappend` defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

archiveCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Archives"
  `mappend` defaultContext

indexCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Home"
  `mappend` defaultContext

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------
static :: Routes -> Pattern -> Rules ()
static r f = match f $ do
    route r
    compile copyFileCompiler

directory :: (Pattern -> Rules a) -> String -> Rules a
directory act f = act $ fromGlob $ f ++ "/**"

assets :: Rules ()
assets = do
  mapM_ (static stripAssets) [ "assets/*", "assets/fonts/*", "assets/css/*", "assets/js/*"
                             , "assets/img/*", "assets/pe-icons/*"
                             ]
  mapM_ (directory (static stripAssets)) [ "assets/rs-plugin/*", "assets/bootstrap/*"
                                         , "assets/cubeportfolio/*", "assets/pe-icons/*"
                                         ]
  where
    stripAssets = gsubRoute "assets/" (const "")

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ setExtension "html"
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/page.html"    postCtx
      >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= relativizeUrls

archive :: Rules ()
archive = do
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
        >>= relativizeUrls

index :: Rules ()
index = do
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      getResourceBody
        >>= applyAsTemplate (indexCtx posts)
        >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

main :: IO ()
main = hakyllWith cfg $ do
  assets
  pages
  posts
  archive
  index
  templates
