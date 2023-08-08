#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [dhall neat-interpolation random])"
#! nix-shell -i runghc

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- enter in repl:
--   :set -XOverloadedStrings

import           Control.Monad      (forM, forM_, replicateM, unless, when)
import           Data.List          (sort)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Data.Time.Calendar (Day, addDays, diffDays, toGregorian)
import           Data.Time.Clock    (UTCTime (utctDay), getCurrentTime)
import           Dhall              (FromDhall, Generic, auto, input)
import           NeatInterpolation  (text)
-- https://hackage.haskell.org/package/neat-interpolation-0.3.2.1/docs/NeatInterpolation.html
import           System.Directory   (createDirectoryIfMissing, doesFileExist,
                                     listDirectory)
import           System.IO.Unsafe   (unsafePerformIO)
import           System.Random      (Random (randoms), mkStdGen)


{-
  Config, loaded from a dhall file
-}
data Config = Config
  { adjectives1     :: [Text]
  , adjectives2     :: [Text]
  , looks           :: [Text]
  , sealImagesPath  :: FilePath
  , postsOutputPath :: FilePath
  , startDate       :: Day
  , seed            :: Int
  } deriving (Generic, Show)
instance FromDhall Config


{-
  Main function
-}
main :: IO ()
main = do
  putStrLn "Starting blog post generation script"

  config <- input auto "./config.dhall"
  let postsOutputPath' = postsOutputPath config
  putStrLn "Validated config successfully"

  createDirectoryIfMissing True postsOutputPath'
  generateAllBlogPosts config
  putStrLn $ "Generated blog posts successfully to " <> postsOutputPath'


{-
  For all the blog posts
  Write them to file
-}
generateAllBlogPosts :: Config -> IO ()
generateAllBlogPosts config = forM_ allBlogPosts' writeToFile'
  where
    allBlogPosts' = allBlogPosts config
    writeToFile' = writeToFile config

allBlogPosts :: Config -> [(FilePath, Text)]
allBlogPosts config = map createSealText zippedDates
  where
    createSealText = uncurry3 $ sealText config
    zippedDates = zip3 [1..] randomNumbers (allDatesSince (startDate config))
    randomNumbers = randoms (mkStdGen (seed config))

    -- | Converts a curried function to a function on a triple.
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f (a,b,c) = f a b c

    allDatesSince :: Day -> [Day]
    allDatesSince startDate = map (`addDays` startDate) [0..daysSinceStart]
      where
        daysSinceStart = diffDays today startDate
        {-# NOINLINE today #-}
        today = unsafePerformIO $ utctDay <$> getCurrentTime

writeToFile :: Config -> (FilePath, Text) -> IO()
writeToFile config (fp, txt) = do
  fileExists <- doesFileExist fp'
  unless fileExists (write fp' txt)
  where
    write = TIO.writeFile
    fp'   = postsOutputPath config <> "/" <> fp


{-
  Returns a filePath, and a corresponding random blog post
-}
sealText :: Config -> Integer -> Int -> Day -> (FilePath, Text)
sealText config n rand date = ( fileName', bp )
  where
    fileName' =
         show date <> "-"
      <> "seal-post-"
      <> show n
      <> ".markdown"
    date'     = T.pack . show $ date
    title     = T.pack $ "Seal Post Number " <> show n
    title'    = T.replace " " "-" title
    sealImagesPath' = sealImagesPath config

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
        sealImagesPath'

    blogPost' (_, 04, 01) =
      blogPost
        title
        "Foolishly look at"
        "beautiful"
        "supermodel, singing"
        "singerSeal.jpg"
        date
        sealImagesPath'

    blogPost' (_, _, _) =
      blogPost
        title
        (randomPull rand (looks config))
        (randomPull rand (adjectives1 config))
        (randomPull rand (adjectives2 config))
        (randomPull rand (unsafeListDirContents sealImagesPath'))
        date
        sealImagesPath'
      where
        randomPull r txts = txts !! (rand `mod` l)
          where l = length txts - 1

    {-
      Blog post format
    -}
    blogPost
      :: Text
      -> Text
      -> Text
      -> Text
      -> Text
      -> Day
      -> FilePath
      -> Text
    blogPost title see adj1 adj2 seal date imagesPath =
      let imagesPath' = T.pack imagesPath
      in
      [text|
        ---
        title: $title
        ---

        $see this $adj1, $adj2 seal!
        <img
          src="/$imagesPath'/$seal"
          alt="A picture of a $adj1, $adj2 seal! <3"
          width="400"
        />
      |]


{-
  Utils
-}
unsafeListDirContents :: FilePath -> [Text]
unsafeListDirContents = map T.pack . drop 2 . sort . unsafePerformIO . listDirectory
--                                   ^^^^^^
-- drop 2 used to remove the birthday and singer photos
-- TODO: find a better way to hardcode this

prettyPrint :: Show a => [a] -> IO ()
prettyPrint = putStr . unlines . map show
