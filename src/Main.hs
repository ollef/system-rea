{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Foldable
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Time
import Network.HTTP.Conduit(simpleHttp)
import System.Directory
import System.FilePath
import Text.Printf
import Text.Read
import qualified Xeno.DOM as Xeno

type Nr = ByteString

data Article = Article
  { articleName :: !ByteString
  , articlePrice :: !Double
  } deriving (Eq, Ord, Show)

data ArticleData = ArticleData
  { mnr :: !(Maybe Nr)
  , mname :: !(Maybe ByteString)
  , mprice :: !(Maybe Double)
  } deriving (Eq, Ord, Show)

instance Semigroup ArticleData where
  (<>) = mappend

instance Monoid ArticleData where
  mempty = ArticleData empty empty empty
  mappend (ArticleData x1 y1 z1) (ArticleData x2 y2 z2)
    = ArticleData (x1 <|> x2) (y1 <|> y2) (z1 <|> z2)

parseFile :: FilePath -> IO (HashMap Nr Article)
parseFile file = do
  text <- BS.readFile file

  case Xeno.parse text of
    Left e -> error $ "Parsing failed: " ++ show e
    Right root -> do
      let articles = [Xeno.contents node | Xeno.Element node <- Xeno.contents root, Xeno.name node == "artikel"]
          articleData = do
            contents <- articles
            return $ foldl' mappend mempty $ do
              content <- contents
              case content of
                Xeno.Element node | Xeno.name node == "nr" -> do
                  let nr = listToMaybe [t | Xeno.Text t <- Xeno.contents node]
                  return mempty { mnr = nr }
                Xeno.Element node | Xeno.name node == "Namn" -> do
                  let namn = listToMaybe [t | Xeno.Text t <- Xeno.contents node]
                  return mempty { mname = namn }
                Xeno.Element node | Xeno.name node == "Prisinklmoms" -> do
                  let pris = listToMaybe [t | Xeno.Text t <- Xeno.contents node]
                  return mempty { mprice = pris >>= readMaybe . UTF8.toString }
                _ -> return mempty

          articleMap = HashMap.fromList
            [ (nr, Article name price)
            | ArticleData (Just nr) (Just name) (Just price) <- articleData
            ]

      unless (length articleData == HashMap.size articleMap) $
        putStrLn "Warning: Couldn't parse everything"

      return articleMap

diffMaps :: HashMap Nr Article -> HashMap Nr Article -> [(Nr, Article, Article)]
diffMaps map1 map2 = sortBy (comparing priceDiff) $ do
  (nr, article2) <- HashMap.toList map2
  case HashMap.lookup nr map1 of
    Just article1
      | p2 < p1 -> return (nr, article1, article2)
      where
        p2 = articlePrice article2
        p1 = articlePrice article1
    _ -> []
  where
    priceDiff (_, a1, a2) = articlePrice a2 / articlePrice a1

dataDir :: FilePath
dataDir = "data"

getCurrentDate :: IO String
getCurrentDate = do
  c <- getCurrentTime
  let (y, m, d) = toGregorian $ utctDay c
  return $ printf "%04d-%02d-%02d" y m d

main :: IO ()
main = do
  createDirectoryIfMissing True dataDir
  files <- listDirectory dataDir
  today <- getCurrentDate
  unless (today `elem` files) $ do
    putStrLn "Laddar ner dagens dator"
    res <- simpleHttp "https://www.systembolaget.se/api/assortment/products/xml"
    BSL.writeFile (dataDir </> today) res
    putStrLn "Färdig"

  let diffFiles = take 1 $ sortBy (flip compare) $ filter (/= today) files

  todayMap <- parseFile $ dataDir </> today

  forM_ diffFiles $ \diffFile -> do
    otherMap <- parseFile $ dataDir </> diffFile
    forM_ (diffMaps otherMap todayMap) $ \(nr, article1, article2) -> do
      let p2 = articlePrice article2
          p1 = articlePrice article1
          percent = (1 - p2 / p1) * 100
          info
            = UTF8.toString (articleName article2)
            ++ " (Nr " ++ UTF8.toString nr ++ "), "
            ++ show p1 ++ "kr (" ++ diffFile ++ ") -> " ++ show p2 ++ " kr (" ++ today ++ ")"
      putStrLn $ printf "Prissänkning (%.1f%%): " percent ++ info
