#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [dhall neat-interpolation random])"
#! nix-shell -i runghc
{-
  Must be run in folder containing the source code
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- enter in repl:
--   :set -XOverloadedStrings

import           Basement.Block     (create)
import           Control.Monad      (forM, forM_, replicateM, unless, when)
import           Data.List          (sort)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Time.Calendar (Day, addDays, diffDays, fromGregorian,
                                     toGregorian)
import           Data.Time.Clock    (UTCTime (utctDay), getCurrentTime)
import           Dhall              (FromDhall, Generic, auto, input)
import           NeatInterpolation  (text)
import           System.Directory   (createDirectoryIfMissing, doesFileExist,
                                     listDirectory)
import           System.IO.Unsafe   (unsafePerformIO)
import           System.Random      (Random, randomRIO)
-- https://hackage.haskell.org/package/neat-interpolation-0.3.2.1/docs/NeatInterpolation.html

{-
  Gives a random number between from and to
  Uses unsafeIO to get the number out of IO
  It's safe because we're only shuffling
-}
randomNum :: System.Random.Random a => a -> a -> a
randomNum from to =
  unsafePerformIO $
  randomRIO (from, to)

{-
  Given a list, returns a random element
-}
randomPull :: [a] -> a
randomPull lst = lst !! r'
  where r' = randomNum 0 l
        l  = length lst - 1

blogPost
  :: Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Day
  -> Text
blogPost title see adj1 adj2 seal date =
  [text|
    ---
    title: $title
    ---

    $see this $adj1, $adj2 seal!
    <img
      src="/images/$seal"
      alt="A picture of a $adj1, $adj2 seal! <3"
      width="400"
    />
  |]

-- Returns a filePath, and a corresponding random blog post
sealText :: Config -> Integer -> Day -> (FilePath, Text)
sealText config n date = ( fileName', bp)
  where
    fileName' =
         show date <> "-"
      <> "seal-post-"
      <> show n
      <> ".markdown"
    date'     = T.pack . show $ date
    title     = T.pack $ "Seal Post Number " <> show n
    title'    = T.replace " " "-" title
    bp = blogPost' (toGregorian date)

    blogPost' :: (Integer, Int, Int) -> Text
    blogPost' (_, 09, 26) =
      blogPost
        title
        "Birthdayingly gaze at"
        "spoiled"
        "older"
        "birthdaySeal.jpg"
        date

    blogPost' (_, 04, 01) =
      blogPost
        title
        "Foolishly look at"
        "beautiful"
        "supermodel, singing"
        "singerSeal.jpg"
        date

    blogPost' (_, _, _) =
      blogPost
        title
        (randomPull $ looks config)
        (randomPull $ adjectives1 config)
        (randomPull $ adjectives2 config)
        (randomPull $ unsafeListDirContents $ sealImagesPath config)
        date

-- Generating all the previous blog posts
-- Only need to do this once
allDatesSince :: Day -> [Day]
allDatesSince startDate = map (`addDays` startDate) [0..daysSinceStart]
  where
    daysSinceStart = diffDays today startDate
    {-# NOINLINE today #-}
    today = unsafePerformIO $ utctDay <$> getCurrentTime

allBlogPosts :: Config -> [(FilePath, Text)]
allBlogPosts config = map f zippedDates
  where
    f = uncurry $ sealText config
    zippedDates = zip [1..] (allDatesSince (startDate config))

writeToFile :: Config -> (FilePath, Text) -> IO()
writeToFile config (fp, txt) = do
  -- fileExists <- doesFileExist fp'
  -- unless fileExists (write fp' txt)
  write fp' txt
  where
    write = TIO.writeFile
    fp'   = postsOutputPath config <> "/" <> fp

-- For all the blog posts
-- Write them to file
unsafeGenerateAllBlogs :: Config -> IO ()
unsafeGenerateAllBlogs config =
  forM_ (allBlogPosts config) (writeToFile config)

main :: IO ()
main = do
  putStrLn "Starting blog post generation script"

  config <- input auto "./config.dhall"
  let postsOutputPath' = postsOutputPath config
  putStrLn "Validated config successfully"

  createDirectoryIfMissing True postsOutputPath'
  unsafeGenerateAllBlogs config
  putStrLn $ "Generated blog posts successfully to " <> postsOutputPath'

prettyPrint :: Show a => [a] -> IO ()
prettyPrint = putStr . unlines . map show

{-
  Config
-}
data Config = Config
  { adjectives1     :: [Text]
  , adjectives2     :: [Text]
  , looks           :: [Text]
  , sealImagesPath  :: FilePath
  , postsOutputPath :: FilePath
  , startDate       :: Day
  } deriving (Generic, Show)
instance FromDhall Config

{-
  Utils
-}
unsafeListDirContents :: FilePath -> [Text]
unsafeListDirContents = map T.pack . sort . unsafePerformIO . listDirectory
